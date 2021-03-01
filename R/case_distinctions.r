example.case_distinction = function() {
  a = c(5,10,3,6)
  b = 1:4
  c = b*10
  d = b*100

  cases(a<=3,b, a==5,c, d)


  multi_size_cases(tibble(b,c), a<=5,
                   tibble(c),a>5)

  cond = c(TRUE, FALSE)

}

cases = function(...) case_distinction(...)

#' A simple function to define case distinctions
#'
#' @family Helper Functions
case_distinction = function(...) {
  args = list(...)

  restore.point("case_distinction")
  n = length(args)
  # if uneven number of elements, last element is a value
  if (n %% 2 == 1) {
	  cond.ind = seq(1,n-1,by=2)
	  val.ind = c(seq(2,n-1,by=2),n)
  } else {
	  cond.ind = seq(1,n,by=2)
	  val.ind = seq(2,n,by=2)
  }


  vals = args[val.ind]
  cond = args[cond.ind]
  nv = length(vals)

  len = max(sapply(cond,length),sapply(vals,length))
  v = rep(vals[[nv]], length.out=len)
  if (nv >1) {
    for (i in (nv-1):1) {
      rows = which(rep(cond[[i]], length.out=len))
      v[rows] = rep(vals[[i]], length.out=len)[rows]
    }
  }
  v
}

example.multi_size_cases = function() {
  a = c(5,10,3,6)
  b = 1:4
  c = b*10

  multi_size_cases(tibble(b,c), a<=5,
                   tibble(c),a>5)

  cond = c(TRUE, FALSE)

}

# returns a list of tibbles for each condition
multi_size_cases = function(...) {
  args = list(...)

  restore.point("multi_size_cases")
  n = length(args)

  vals = args[seq(1,n,by=2)]
  conds = args[seq(2,n,by=2)]
  nv = length(vals)
  nr = NROW(vals[[1]])
  case = rep(0,nr)

  i=1
  for (i in 1:nv) {
    rows = case == 0 & conds[[i]]
    case[rows] = i
  }
  list(case=case, vals=vals)
}
