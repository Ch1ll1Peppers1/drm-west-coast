library(tidyverse)
library(lubridate)
library(here)

trawl_survey_codes <- read_csv("trawl_survey_codes.csv") %>% 
  select(-`...1`)

bt <- read_csv("glorys_bt.csv") %>% 
  left_join(trawl_survey_codes) %>% 
  mutate(newdate = as_date(date))

cpue <- read_csv(here("processed-data","biomass_time.csv"))%>%
  filter(year < 2020)

maxyr <- max(cpue$year)
minyr_glorys <- 1993

survey_start_times <- cpue %>% 
  mutate(month_year = paste0(startmonth,"-",year)) %>%
  select(survey, year, month_year) %>%
  distinct() %>%
  mutate(ref_yr = paste0(survey,"-",month_year), # get unique survey identifier ("reference year")
         survey_date = dmy(paste0('01-',month_year))# get earliest possible survey start date
  ) 

tmp <- data.frame(survey=rep("WCANN", 2), year=c(2019, 2020), month_year = c("5-2019", "5-2020"), ref_yr = c("WCANN-5-2019","WCANN-5-2020"))
tmp <- tmp |> 
  mutate(survey_date = dmy(paste0('01-',month_year)))

survey_start_times <- bind_rows(survey_start_times, tmp)

glorys_ref_yrs <- expand.grid(month=seq(1, 12, 1), year=seq(minyr_glorys, maxyr, 1), survey=unique(survey_start_times$survey)) %>% 
  
  mutate(survey = as.character(survey),
         survey_month_year = paste0(survey,"-",month,"-",year)) %>% 
  mutate(ref_yr_prep = ifelse(survey_month_year %in% survey_start_times[survey_start_times$year>=minyr_glorys,]$ref_yr, survey_month_year, NA), 
         month_year = paste0(month,"-",year)) %>% 
  group_by(survey) %>% 
  fill(ref_yr_prep, .direction="up") %>%  
  group_by(survey) %>% 
  arrange(year) %>% 
  mutate(ref_yr = ifelse(survey_month_year==ref_yr_prep, lead(ref_yr_prep), ref_yr_prep)) %>% 
  select(ref_yr, survey, month_year) %>% 
  ungroup() %>% 
  left_join(survey_start_times %>% select(ref_yr, survey_date) %>% distinct(), by="ref_yr") 


glorys_bt <- bt %>% 
  mutate(date = newdate,
         year = year(date),
         month = month(date),
         month_year = paste0(month,"-",year)
  ) %>% 
  filter(str_detect(date, '-02-29', negate=TRUE)) %>% 
  left_join(glorys_ref_yrs, by=c('survey','month_year')) %>% 
  filter(!is.na(ref_yr)) 
tmp <- glorys_bt %>% group_by(ref_yr) %>% summarise(n=length(bt))


bad_ref_yrs <- tmp %>% filter(n<364) %>% pull(ref_yr) # for GLORYS, this flags all the 1993 surveys

mhw_glorys_full <- glorys_bt |> 
  mutate(date_lag = survey_date - date) %>%
  filter(date_lag < 365,
         date_lag >= 0) 

bt_wcann <- mhw_glorys_full |> 
  filter(surveyID == 18) |> 
  group_by(month_year, ref_yr, year) |> 
  summarise(mean_bt = mean(bt)) |> 
  group_by(ref_yr) |> 
  summarise(mean_month_bt = mean(mean_bt),
            max_month_bt = max(mean_bt),
            min_month_bt = min(mean_bt))

write_csv(bt_wcann, here("glorys_wcann_bt_for_lmrc.csv"))
