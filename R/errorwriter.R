keyErrorLog = function(stop=FALSE, append.char="<br>", line.break.char="<br>", verbose=FALSE) {
  kel = new.env(parent = emptyenv())

  kel$terminate = FALSE
  kel$append.char = append.char
  kel$line.break.char = line.break.char
  kel$log = list()
  kel$count = 0
  kel$key = ""
  kel$verbose = verbose
  kel$params = list()
  kel$stop = stop
  kel$withKey = function(expr, key=paste0(kel$key,",",sub.key), sub.key="", envir=parent.frame()) {
    expr = substitute(expr)
    base.key = kel$key
    kel$key = key
    eval(expr, envir)
    kel$key = base.key
  }
  kel$setKey = function(...) {
    args = unlist(list(...))
    names(args) = NULL
    restore.point("kel$setKey")
    kel$key = paste0(args, collapse=",")
  }
  kel$error = function(message,..., params=kel$params,message.type="error", append=TRUE, do.stop=TRUE) {
    kel$write(message,..., params=params, message.type=message.type,append=append, do.stop=TRUE)
  }
  kel$warning = function(message,..., params=kel$params,message.type="warning", append=TRUE) {
    kel$write(message,..., params=params, message.type=message.type,append=append, do.stop=FALSE)
  }

  kel$write = function(message,...,terminate=FALSE, params=c(kel$params,list(...)), key=kel$key, message.type="error", append=TRUE, do.stop=kel$stop) {
    params = c(params,list(...))
    restore.point("kel$write")
    #cat("\nkel: ");print(kel)
    # format vector params by default
    params = lapply(params, function(param) paste0(param, collapse=",  "))
    #message = replace.whiskers(message, params, eval=FALSE)
    message = whisker::whisker.render(message, params)

    if (!is.null(kel$line.break.char)) {
      message = gsub("\n",kel$line.break.char,message)
    }
    if (append) {
      kel$log[[key]] = paste0(kel$log[[key]], append.char, message)
    } else {
      kel$log[[key]] = message
    }
    kel$count = kel$count+1
    emsg = paste0("\nkel ", kel$count," ",kel$key, ": ",message)
    if (do.stop) {
      stop(emsg)
    } else if (verbose) {
      cat(emsg)
    }
    kel$terminate = terminate | kel$terminate
  }
  kel$setparams = function(...) {
    args = list(...)
    kel$params[names(args)] = args
  }
  kel$kelTry = function(expr,msg="", add.error.to.msg=TRUE, default=NA) {
    res = try(expr)
    if (is(res,"try-error")) {
      kel$params$error = as.character(res)
      if (add.error.to.msg)
      	msg = paste0(msg,": ", as.character(res))
      kel$write(msg)
      return(default)
    }
    return(res)
  }

  kel

}


set.leaf.value = function(li, value) {
  if (!is.list(li)) {
    return(value)
  } else {
    nli = lapply(li, set.leaf.value, value=value)
    return(nli)
  }
}


