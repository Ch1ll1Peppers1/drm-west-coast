/**
 * @title Applying movement
 *
 * @description
 * 
 * @param lambda array of number of individuals per age, time, and patch
 * @param M movement matrix
 * @param mov_age ages at which movement starts (this can be generalized)
 * 
 * @return an array of numbers by age, year and patch
 */
array[] matrix apply_movement(array[] matrix lambda, matrix M,
                              array[] int mov_age) {
  array[3] int dimensions;
  dimensions = dims(lambda);
  array[dimensions[1]] matrix[dimensions[2], dimensions[3]] output;
  output = lambda;
  for (a in 1:dimensions[1]) {
    if (mov_age[a]) {
      for (time in 1:dimensions[2]) {
        output[a, time, 1:dimensions[3]] =
          lambda[a, time, 1:dimensions[3]] * M';
      }
    }
  }
  return output;
}
