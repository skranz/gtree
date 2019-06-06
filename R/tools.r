does.intersect = function(A,B) {
  length(intersect(A,B))>0
}

#' Change a call object like call = quote(c(x+1,x+2))
#' into a list of calls list(quote(x+1), quote(x+2))
list.call.to.call.list = function(call) {
  sym = as.character(call[[1]])
  if (sym=="c" | sym=="list") {
    call[[1]] = as.symbol("expression")
  } else {
    return(call)
  }
  as.list(eval(call))
}

remove.from.list = function(li, names) {
  use = setdiff(names(li), names)
  li[use]
}


name.by.name = function(li) {
  names = sapply(li, function(x) x$name)
  names(li) = names
  li
}


#' Transform a pure rhs formula like ~x+2 to a quoted call
f2c = function(x) {
  if (is(x,"formula")) return(x[[length(x)]])
  x
}

eval.or.return = function(expr, envir = parent.frame(), enclos = if (is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()) {
  if (is.call(expr) | is.name(expr) | is.expression(expr)) {
    return(eval(expr,envir, enclos))
  }
  return(expr)
}

do.call.if.exists = function(what, args, envir=parent.frame()) {
  if (exists(what)) return(do.call(what, args, envir=envir))
  return(NULL)
}


is.empty = function(x) {
  len = length(x)
  if (len==0) return(TRUE)
  if (len>1) return(FALSE)
  if (is.character(x)) {
    if (x=="") return(TRUE)
    #return(str.trim(x)=="")
  }
  FALSE
}

# Create a data frame quickly
quick.df = function (...)
{
  df = list(...)
  attr(df, "row.names") <- 1:length(df[[1]])
  attr(df, "class") <- "data.frame"
  df
}

# converts an R list to a css style string
list.to.style = function(x) {
  paste0(names(x),": ", x, collapse="; ")
}


try.with.msg = function(expr, msg="") {
  #expr = substitute(expr)
  res = try(expr,silent = TRUE)
  if (is(res,"try-error")) {
    #restore.point("try.with.msg.has.error")
    stop(paste0(msg,"\n", as.character(res)),call. = FALSE)
  }
  res
}

named.list = function(names, ...) {
	li = list(...)
	names(li) = names
	li
}

set.names = function(x, names) {
	names(x) = names
	x
}

deparse1 = function (call, collapse = "")
{
    paste0(deparse(call, width = 500), collapse = collapse)
}

#' Like paste0 but returns an empty vector if some string is empty
sc = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}


first.non.null = function(...) {
  args = list(...)
  args = args[!sapply(args, is.null)]
  if (length(args)==0) return(NULL)
  args[[1]]
}

# takes a vector of ids
# replaces it by a vector of indices starting from 1
id.to.index = function(id, unique.ids = unique(id)) {
	match(id, unique.ids)
}

