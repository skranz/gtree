# Functions that can be part of formulas in game structures etc.

str_starts_with = function(str, pattern) {
  str.starts.with(str,pattern)
}

str_ends_with = function(str, pattern) {
  str.ends.with(str,pattern)
}


str_contains = function(str,pattern) {
  has.substr(str,pattern)
}

str_split = function(str,pattern,ind) {
  restore.point("str_split")
  res = str.split(str,pattern)
  sapply(res, function(vec) vec[ind])
}

str_combine = function(...) {
  paste0(...)
}

is_true = function(val) {
  is.true(val)
}