## package config
if (!file.exists("cleanup"))
  instantiate::stan_package_configure()

## licensing
if (!file.exists("LICENSE.md"))
  usethis::use_gpl_license()

## Rmd readme
if (!file.exists("README.Rmd"))
  usethis::use_readme_rmd()

## badge for experimental
usethis::use_lifecycle_badge("experimental")

## later (building a webpage for the pkg)
## usethis::use_pkgdown()
usethis::use_pkgdown_github_pages()

## creating a vignette
if (!file.exists("vignettes/get-started.Rmd"))
  usethis::use_vignette(name = "get-started")

usethis::use_news_md()

## build pkgdown website
## usethis::use_github_action("pkgdown")

usethis::use_build_ignore("vignettes/*_files")

usethis::use_git_ignore("*_files", "vignettes")

## creating quarto vignettes vignette
if (!file.exists("vignettes/get-started.qmd"))
  usethis::use_vignette(name = "get-started.qmd")

if (!file.exists("vignettes/theory.qmd"))
  usethis::use_vignette(name = "theory.qmd")

if (!file.exists("vignettes/examples.qmd"))
  usethis::use_vignette(name = "examples.qmd")
