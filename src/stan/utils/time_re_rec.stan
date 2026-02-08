/**
 * @title Adding process error
 *
 * @description
 * 
 * @param lrec log recruitment for each site
 * @param z "process error"
 * 
 * @return an array of numbers by age, year and patch
 */
matrix add_pe(vector lrec, vector z) {
  array[3] int dimensions;
  int nt = num_elements(z);
  int np = num_elements(lrec) %/% nt; // %/% is integer division!
  matrix[nt, np] output = to_matrix(lrec, nt, np);
  for (p in 1:np) {
    output[, p] += z;
  }
  return output;
}
