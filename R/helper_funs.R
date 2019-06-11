#' Returns logical vector replacing NA by FALSE
#'
#' @param vec A vector of logical values
#' (possible containing NA) or values that can be
#' transformed to logicals
is_true = function(vec) {
  vec[is.na(vec)] = FALSE
  vec = as.logical(vec)
  return(vec)
}
