// counting number of zeros
int num_non_zero_fun(vector y) {
  int A = 0;
  int N = size(y);
    
  for (n in 1 : N) {
    if (y[n] != 0) {
      A += 1;
    }
  }
  return A;
}
  
array[] int non_zero_index_fun(vector y, int A) {
  int N = size(y);
  array[A] int non_zero_index;
  int counter = 0;
  for (n in 1 : N) {
    if (y[n] != 0) {
      counter += 1;
      non_zero_index[counter] = n;
    }
  }
  return non_zero_index;
}
  
array[] int zero_index_fun(vector y, int Z) {
  int N = size(y);
  array[Z] int zero_index;
  int counter = 0;
  for (n in 1 : N) {
    if (y[n] == 0) {
      counter += 1;
      zero_index[counter] = n;
    }
  }
  return zero_index;
}
