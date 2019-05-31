


memory.list = function(x) {
  library(pryr)
  if (is.environment(x))
    x = as.list(x)

  x.size = format(object.size(x), units="auto")
  if (!is.list(x))
    return(x.size)

  if (length(x)==0) return("0")

  restore.point("fhdfjdhfk")

  # Sort by size
  sizes = lapply(x, object.size)
  ord = order(-unlist(sizes))
  x = x[ord]

  fsizes = lapply(sizes[ord], format, units="auto")
  kids = lapply(x,memory.list)
  cad = sapply(x, function(obj) find.address(obj))
  names(kids) = paste0(names(x),": ", fsizes, " ", cad)

  size.li = list(paste0(x.size, " ",refs(x)-1," ",find.address(x)))
  names(size.li) = x.size
  c(size.li,kids)
}

# More robust version of pryr:::address
find.address = function(x) {
  .GlobalEnv$.FIND.ADDRESS.TEMP = x
  res = pryr:::address2(".FIND.ADDRESS.TEMP",.GlobalEnv)
  .GlobalEnv$.FIND.ADDRESS.TEMP = NULL
  res
}
