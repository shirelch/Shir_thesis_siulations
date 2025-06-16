mean_diff_from_median = function(x) {
  median_value = median(x, na.rm = TRUE)
  mean_diff    = mean(abs(x - median_value), na.rm = TRUE)
  return(mean_diff)
}
