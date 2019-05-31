# Convert games into subsequent formats

# Game formats:
# jg: json game, format stored and modified in web interface
# rg: r game, jg transformed and simplified as r object
# vg: variant game, an r game reduced to a simple variant
#   - vtg: variant type game, a modified vg that allows for multiple types
# tg: extensive table form game, derived from a variant game
# efg: Gambit extensive form game format

examples.jg.to.rg = function() {
	setwd("D:/libraries/gtree/projects/UltimatumGame/")

	gameId = "UltStratMeth"
	gameId = "AB"
	jg = get.jg(gameId = gameId)
	rg = jg.to.rg(jg)
	varpar = rg$varpar
	varpar
}


jg.to.rg = function(jg, stop=gtree.stop.on.error()) {
  restore.point("jg.to.rg")
  rg = new.env(parent = emptyenv())

  rg$kel = keyErrorLog(stop = stop)
  rg$gameId = jg$gameId
  rg$gameInfo = jg$gameInfo
  parse.jg.varpar(rg,jg)
  parse.jg.stages(rg,jg)
  varpar = rg$varpar
  rg$stages

  rg$jg.hash = digest(jg)


  return(rg)
}

parse.jg.stages = function(rg, jg, kel=rg$kel) {
  restore.point("parse.jg.stages")
  rg$stages = jg.rparse.formulas(jg$stages,key = "stages",kel = kel)
  rg$stages = lapply(rg$stages, function(stage) {
  	restore.point("jhsjhdhsfhuh")
    if (length(stage$actions)>0)
  		names(stage$actions) = get.names(stage$actions)
  	if (length(stage$nature)>0)
  		names(stage$nature) = get.names(stage$nature)
  	if (length(stage$compute)>0)
  		names(stage$compute) = get.names(stage$compute)
  	for (ind in seq_along(stage$actions)) {
  		stage$actions[[ind]]$domain.var = get.strategyMethodDomainVar(action = stage$actions[[ind]], rg=rg, kel=kel)
  	}



  	# combine all addins from the different special categories

  	# first merge categories to a list with one entry per addin type
  	special = do.call("c",set.names(stage$special,NULL))
    # now we merge all addin types to a list of all addins
    li = lapply(names(special), function(type) {
      ai.li = special[[type]]
      if (length(ai.li)==0) return(NULL)
      lapply(ai.li, function(ai) {
        ai$type = type
        ai
      })
    })
    ai.li = do.call("c", li)
    if (!is.null(ai.li)) {
      names = sapply(ai.li, function(ai) ai$name)
      names(ai.li) = names
    } else {
      ai.li = list()
    }
    stage$ai.li = ai.li

    # store a list of used addin types for further reference
    stage$ai.types = unique(unlist(lapply(ai.li, function(ai) ai$type)))
    rg$ai.types = unique(c(rg[["ai.types"]], stage$ai.types))

    # remove special to save memory
    stage$special = NULL

  	stage
  })
  names(rg$stages) = get.names(rg$stages)
  invisible(rg$stages)
}


parse.jg.varpar = function(rg, jg, kel=rg$kel) {
  restore.point("parse.jg.varpar")
  kel$key = "varpar,varparTable"
  x = jg$varpar
  x = lapply(x, function(xr) {
  	unlist(lapply(xr, function(xv) {
  		if (is.null(xv))
  			xv = ""
  		xv
  	}))
  })
  mat = do.call(rbind, x)
  if (NROW(mat)<2) {
    kel$write("You need to specify least one variant.", terminate=TRUE)
  }

  rg$parnames = setdiff(as.vector(mat[1,-1]),"descr")
  if (any(duplicated(rg$parnames))) {
    kel$write("You have duplicated parameter names {{dups}}.", dups=paste0(rg$parnames[duplicated(rg$parnames)], collapse=", "), terminate=TRUE)
    return(NULL)
  }

  rg$variants = mat[-1,1]
  if (any(duplicated(rg$rg$variants))) {
    kel$write("You have duplicated variant names {{dups}}.", dups=paste0(rg$variants[duplicated(rg$variants)], collapse=", "), terminate=TRUE)
    return(NULL)
  }

  descr.col = which(mat[1,]=="descr")
  if (length(descr.col)==1) {
    rg$descr.variants = mat[-1,descr.col]
  } else {
    kel$write("You need to have (exactly) one column 'descr' that can contain a short description of the variant.", terminate=TRUE)
    rg$descr.variants = rep("", length(rg$variants))
    descr.col = NULL
  }
  vals = mat[-1,-c(1, descr.col), drop=FALSE]
  if (NCOL(vals)<1) {
    kel$write("The first column contains the variant names, and then you need at least one parameter column numPlayers.", terminate=TRUE)
    return(NULL)
  }

  baseline = as.list(vals[1,])
  names(baseline) = rg$parnames
  baseline = parse.jg.params(baseline, variant = rg$variants[1], kel=kel, eval.formula = FALSE)
  varpars = vector("list",length(rg$variants))
  names(varpars) = rg$variants
  #varpars[[1]] = baseline
  for (i in seq_along(rg$variants)) {
    params = as.list(vals[i,])
    names(params) = rg$parnames
    varpars[[i]] = parse.jg.params(params,baseline,rg$variants[i], kel)
  }
  varpars = as.data.frame(data.table::rbindlist(varpars))
  rownames(varpars) = rg$variants
  rg$varpar = varpars
  if (!"numPlayers" %in% names(varpars)) {
    kel$write("You don't have the required parameter numPlayers.")
  } else if (!is.numeric(varpars$numPlayers)) {
    kel$write("The parameter numPlayers is not numeric everywhere.")
  }

  invisible(varpars)
}

parse.jg.params = function(params, baseline=NULL,variant, kel, eval.formula = TRUE) {
  restore.point("parse.jg.params")
  np = vector("list", length(params))
  names(np) = names(params)
  i = 1
  kel$setparams(variant=variant)
  for (i in seq_along(params)) {
    name = names(params)[i]
    kel$params$parname = kel$params$field = name
    if (params[[i]]=="") {
      # parse baseline parameters
      if (is.null(baseline)) {
        kel$write("An empty parameter {{parname}} is not allowed in your first variant {{variant}}")
        np[[i]] = ""
        next
      }
      # empty parameters will be set equal to the baseline value
      params[[i]] = baseline[[i]]
      #next
    }
    if (eval.formula) {
    	val = kel$kelTry(jg.parse.formula(params[[i]],np),"Error when parsing parameter {{parname}} in variant {{variant}}:\n{{error}}")
    } else {
    	val = params[[i]]
    	if (substring(val,1,1) != "=") {
    		val = kel$kelTry(jg.parse.formula(params[[i]],np),"Error when parsing parameter {{parname}} in variant {{variant}}:\n{{error}}")
    	}
    }
    np[[i]] = val
  }
  np
}

convert.atom = function(x, remove.quotes = TRUE) {
  if (!is.character(x)) return(x)

	y = suppressWarnings(as.logical(x))
  if (!all(is.na(y))) return(y)
  y = suppressWarnings(as.numeric(x))
  if (!(all(is.na(y)))) return(y)
  if (remove.quotes) {
    quoted = substring(x,1,1) == '"'
    x[quoted] = substring(x[quoted],2,nchar(x[quoted])-1)
  }
  x
}

jg.rparse.formulas = function(li, key="", kel, need.quotes=FALSE) {
  if (is.list(li)) {
    restore.point("jg.parse.formula.list")

    inds = names(li)
    if (is.null(inds))
      inds = seq_along(li)
    res = lapply(seq_along(li), function(i) {
      field = names(li[inds])[i]
      need.quotes = field %in% c("set","probs","formula")
      jg.rparse.formulas(li[[i]],paste0(key,",",inds[i]),kel, need.quotes=need.quotes)
    })
    if (!is.null(names(li))) names(res) = names(li)
    return(res)
  }
  restore.point("jg.parse.formula.atom")
  kel$key = key

  kel$kelTry(jg.parse.formula(li,values=NULL, eval=FALSE, need.quotes=need.quotes),"{{error}}")

}

jg.parse.formula = function(str, values, eval=!missing(values), need.quotes=FALSE) {
  restore.point("jg.parse.formula")
  char = substring(str,1,1)
  if (char == "=") {
    mode = "formula"
  } else if (char == "[") {
    mode = "list"
  } else {
    mode = "atomic"
  }
  if (mode == "atomic") {
    quoted = substring(str,1,1) == '"'
    val = convert.atom(str)
    if (is.character(val) & !quoted & need.quotes) {
      stop('Please quote your single string value "', val,'". If you want to type an R formula, start with th equal sign =.')
    }
    return(val)
  }
  if (mode == "list") {
    restore.point("iddfbduzf7zv")
    val = unlist(yaml.load(str))
    return(convert.atom(val))
  }
  # a true formula
  org.str = str
  #str = rewrite.if.formula(str)
  str = substring(str,2)
  call = parse.as.call(text=str)
  if (!eval) return(call)
  eval(call,values)
}

rewrite.if.formula = function(str) {
  restore.point("rewrite.if.formula")
  if (!has.substr(str, " IF ") & !has.substr(str, " ELSE")) {
    return(str)
  }
  txt = sep.lines(str)
  chars = substring(txt,1,1)
  if (!all(chars=="=")) {
    stop(paste0("In an IF condition each line has to start with '=', but you wrote:\n", str))
  }
  txt = str.trim(substring(txt,2))
  if.line = has.substr(txt, " IF ")
  else.line = has.substr(txt, " ELSE")
  if (any(!if.line & !else.line)) {
    stop(paste0("In an IF condition each line must contain an IF or ELSE (ELSE only for the final line), but you wrote:\n", str))
  }
  if (any(which(else.line) != length(txt))) {
    stop(paste0("In an IF condition, you can put the ELSE condition only in the final line, but you wrote:\n", str))
  }
  left = c(str.left.of(txt[if.line]," IF "))
  right = str.right.of(txt[if.line]," IF ")
  args.str = paste0(left,",",right,collapse=",")
  if (sum(else.line)>0) {
    args.str = paste0(args.str,",", str.left.of(txt[else.line]," ELSE"))
  }
  args.str = gsub("\n","", args.str, fixed=TRUE)
  res = paste("=case_distincion(",args.str,")")
  res
}



get.strategyMethodDomainVar = function(action,rg,kel) {
	restore.point("eval.strategyMethodDomain")

	smd = action$strategyMethodDomain

	if (is.character(smd)) {
		if (nchar(smd)==0) return(NULL)
		return(smd)
	}

	# smd is now a call

	smd.vars = find.variables(smd)
	params = as.list(rg$varpar[1,])

	if (!all(smd.vars %in% names(params))) {
		kel$error(paste0("A strategyMethodDomain can only depend on parameters but not any other variable. Thus you cannot condition on ", paste0(setdiff(md.vars, names(rg$params)), collapse=", " ),"."))
	}
	smd = names(eval(smd, params))
	smd
}
