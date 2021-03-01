# Reduce an R game rg to a single variant game vg

# Steps
# 1. Set variant and params
# 2. TO DO: Remove stages whose condition always fails in this variant

examples.rg.to.vg = function() {
	setwd("D:/libraries/XEconDB/projects/UltimatumGame/")

	gameId = "UltStratMeth"
	gameId = "ultimatumGame"
	jg = get.jg(gameId = gameId)
	rg = jg.to.rg(jg)
	vg = rg.to.vg(variant=1, rg=rg)
	vg$stages[[2]]
	varpar = rg$varpar
	varpar

}


rg.to.vg = function(rg, variant=1, stop=gtree.stop.on.error()) {
  restore.point("rg.to.vg")
  vg = new.env(parent = emptyenv())
  vg$kel = keyErrorLog(stop=stop)
  vg$gameId = rg$gameId
  if (is.numeric(variant)) variant = rg$variants[variant]
  vg$variant = variant
  vg$vg.id = paste0(vg$gameId,"_",vg$variant)
  vg$params = as.list(rg$varpar[variant,,drop=FALSE])
  vg$stages = rg$stages
  vg = extract.vg.vars.info(vg=vg)
  vg$jg.hash = rg$jg.hash
  class(vg) = c("gtree_vg","environment")
  return(vg)
}

# a vector of the classes of all vg variables
# useful for otree export without need to
# compute the whole table game tg representation
extract.vg.vars.info = function(vg, kel=vg$kel) {
	restore.point("extract.vg.vars.info")

  # variables defined in stages
  svars = unique(unlist(lapply(vg$stages, function(stage) {
    get.names(c(stage$actions, stage$nature, stage$compute))
  })))

  # all variables and paramaters
  vars = unique(c(names(vg$params), svars,vg$variant))
  n = length(vars)

  # a list with values
  vals = lapply(1:n, function(i) NA)
  names(vals) = vars
  vals[names(vg$params)] = vg$params
  vals$variant = vg$variant

  classes = sapply(1:n, function(i) "NA")
  names(classes) = vars
  classes[names(vg$params)] = sapply(vg$params, function(x) class(x)[1])


  stage.num = 2
  # go through stages and compute values and class of variables
  for (stage.num in seq_along(vg$stages)) {
  	stage = vg$stages[[stage.num]]
    stage.key = kel$setKey("stages", stage.num)

    need.vars = condition.need.vars= NULL
    # check condition
    kel$setKey(stage.key, "condition")
  	cond = stage$condition
	  if (!is.call(cond) &!is.name(cond)) {
	    # no condition
	    if (!is.empty(cond)) {
	    	kel$write("Either you specify no stage condition, or you write an R formula starting with '=', which evaluates as TRUE or FALSE.")
	    }
	  } else {
	  	condition.need.vars = need.vars = find.variables(cond)
	  }
    kel$kelTry(eval(cond, vals))



    for (a.num in seq_along(stage$nature)) {
    	a = stage$nature[[a.num]]
      var = a$name
    	move.key = kel$setKey(stage.key, "nature", a.num)


    	kel$setKey(move.key, "set")
      set = kel$kelTry(eval(a$set, vals), msg=paste0("Evaluating set for ", var))
      need.vars = unique(c(need.vars,find.variables(a$set)))

    	kel$setKey(move.key, "probs")
      kel$kelTry(eval(a$probs, vals), msg=paste0("Evaluating probs for ", var))
      need.vars = unique(c(need.vars,find.variables(a$probs)))

      if (length(set)>0) {
        val = set[ceiling(length(set)*0.3)]
        vals[[var]] = val
        classes[[var]] = class(val)[1]
      }
    }
    for (a.num in seq_along(stage$compute)) {
    	a = stage$compute[[a.num]]
      var = a$name
    	move.key = kel$setKey(stage.key, "compute", a.num)
      var = a$name
    	kel$setKey(move.key, "formula")
    	if (is.call(a$formula) | is.name(a$formula)) {
    		variables = find.variables(a$formula)
    		undefined = setdiff(variables, vars)
    		if (length(undefined) >0 ) {
    			kel$write(paste0("The variable(s) ",paste0(undefined, collapse=", ")," have not been defined earlier."))
    		}
    	}
      val = kel$kelTry(eval(a$formula, vals), msg=paste0("Evaluating formula for ", var))
      need.vars = unique(c(need.vars,find.variables(a$formula)))


      vals[[var]] = val
      classes[[var]] = class(val)[1]
    }

    domain.vars = NULL
    for (a.num in seq_along(stage$actions)) {
    	restore.point("dhfkjdhfuihdufih")
    	a = stage$actions[[a.num]]
    	move.key = kel$setKey(stage.key, "actions", a.num)

    	var = a$name
      set = kel$kelTry(eval(a$set, vals),msg=paste0("Evaluating set for ", var))
      need.vars = unique(c(need.vars,find.variables(a$set)))

      if (length(set)>0) {
        val = set[ceiling(length(set)*0.3)]
        vals[[var]] = val
        classes[[var]] = class(val)[1]
      }

      smd = eval.strategyMethodDomain(vg=vg, action=a, stage.num = stage.num, kel=kel)
      domain.vars = unique(c(domain.vars,names(smd)))
      vg$stages[[stage.num]]$actions[[a.num]]$domain.vals = smd
    }

    # check observe
  	# observe is fixed, no formula
  	kel$setKey(stage.key, "observe")
 		observe = stage$observe
  	if (is.character(observe)) {
			unknown = setdiff(observe, c(names(vals),""))
    	if (length(unknown)>0) {
      	kel$write("You cannot observe the variable(s) {{unknown}}, because they have not been defined earlier.", unknown=unknown)
    	}
			need.vars = unique(c(need.vars,setdiff(observe, domain.vars)))
    } else if (is.call(observe) | is.name(observe)) {
    		kel$warning("Warning: Better don't use a formula for observe: Forms and export to oTree may not work correctly. If you have fixed variables that are observed, just write a list, like [var1, var2]. If the observed variables depend on earlier variables, better create multiple stages with different conditions, that then each have fixed observed variables.")
    		kel$kelTry(eval(observe, vals))
				need.vars = unique(c(need.vars,setdiff(find.variables(observe), domain.vars)))

    }

    if (identical(stage$player,"") & length(stage$actions)>0) {
  	  kel$setKey(stage.key, "player")
      kel$write(paste0("You have specified actions but no player for the stage ", stage$name,"."),message.type = "error")
    }


 		stage.vars = unique(c(get.names(stage$actions), get.names(stage$nature), get.names(stage$compute)))

 		need.vars = setdiff(need.vars,c("",stage.vars))


 		# need vars are useful to determine which stages can be shown
 		vg$stages[[stage.num]]$stage.vars = stage.vars
 		vg$stages[[stage.num]]$need.vars = need.vars
 		vg$stages[[stage.num]]$condition.need.vars = condition.need.vars
 		vg$stages[[stage.num]]$domain.vars = domain.vars



  }



  vg$vars = vars
  vg$vars.class = classes
  vg$vars.sample = vals
  vg
}

eval.strategyMethodDomain = function(action,vg, stage.num, kel) {
	restore.point("eval.strategyMethodDomain")

	smd = action$strategyMethodDomain
	domain.var = action$domain.var

	if (is.empty(smd)) return(NULL)

	if (is.character(smd)) {
		if (nchar(smd)==0) return(NULL)
		# try to find the specified variable
		var = smd
		old.set = list()
		has.found = FALSE
		for (sn in seq_len(stage.num-1)) {
			actions = vg$stages[[sn]]$actions
			if (smd %in% get.names(actions)) {
				action = actions[[smd]]
				set = action$set

				# check if the reference variable
				# has been defined before with a different
				# set.
				differ = sapply(old.set, function(os) {
					!identical(set, os)
				})
				if (any(differ)) {
					kel$error(paste0("Sorry, but the domain variable ", smd," is defined multiple times in earlier stages with different sets. We need a unique set definition to automatically compute the strategy domain. Consider to manually specify the strategy domain. For example,<br> =list(domainvar=1:5)"))
				}
				has.found = TRUE
				old.set = c(old.set, list(set))
			}
		}
		if (!has.found) {
			kel$error(paste0("Sorry, but your domain variable ", smd," is not defined as an action in an earlier stage."))
		}
		var = smd
		smd = substitute(list(var = set), list(set=set))
	}

	if (!is.call(smd) & !is.expression(smd)) {
		kel$error(paste0("You must either leave the strategyMethodDomain empty, specify a single action name or enter a r formula that evaluates to a list or data frame and has the domain variables as names. Examples:<br>list(offer = 1:5)<br>tibble(cost=c('low','high','unknown'),observed=c(TRUE,TRUE,FALSE))"))
	}

	# smd is now a call

	smd.vars = find.variables(smd)
	if (!all(smd.vars %in% names(vg$params))) {
		kel$error(paste0("A strategyMethodDomain can only depend on parameters but not any other variable. Thus you cannot condition on ", paste0(setdiff(md.vars, names(vg$params)), collapse=", " ),"."))
	}
	smd = eval(smd, vg$params)
	names(smd) = domain.var
	smd
}
