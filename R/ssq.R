#' Compute sum of squares
#' @noRd
ssq = function(x) {
  assert_numeric(x, any.missing = FALSE, min.len = 1)
  sum(x^2)
}
