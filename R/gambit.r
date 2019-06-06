example.gambit.solve.eq = function() {
	# set working directory to project directory
  setwd("D:/libraries/gtree/myproject")
	gameId = "TestGambit"
	gameId = "UltimatumGame"
	tg = get.tg(gameId = gameId,never.load = !FALSE)

	eq.li = gambit.solve.eq(tg)

	eq.li = get.eq(tg)
	eq.li
  eqo.df = eq.outcomes(eq.li, tg=tg)
  eqo.df

  ceqo = cond.eq.outcomes(eq.li, cond=list(probA=c(0.2,0.8)),tg = tg, expected=TRUE)

  eceqo = expected.cond.eq.outcomes(ceqo)



	# Inequity aversion
  alpha = 0.371; beta=0.31
  util.funs = list(ineqAvUtil(1, alpha,beta),ineqAvUtil(2,alpha,beta))
  eq.li = get.eq(tg, util.funs = util.funs)
  #eq.li = get.eq(tg, util.funs = util.funs, just.spe=FALSE)
  eqo.df = eq.outcomes(eq.li, tg=tg)
  eqo.df

  eeqo.df = expected.eq.outcomes(eqo.df)


  # conditional equilibrium outcomes for all maxOffers
  cond = expand.grid(maxOffer = unique(tg$oco.df$maxOffer))
  eo = eq.li %>%
  	cond.eq.outcomes(cond = cond, tg=tg) %>%
  	expected.eq.outcomes(group.vars = c("eq.ind",names(cond)))

  library(ggplot2)
  ggplot(eo, aes(x=maxOffer, y=accept, fill=is.eqo)) + geom_bar(stat = "identity") + ggtitle("Acceptance probabilty as function of maxOffer")

  ggplot(eo, aes(x=maxOffer, y=payoff_1, fill=is.eqo)) + geom_bar(stat = "identity")

  ggplot(eo, aes(x=maxOffer, y=util_1, fill=is.eqo)) + geom_bar(stat = "identity")


  # solve mixed equilibria
 	gameId = "Pennies"
	tg = get.tg(gameId = gameId,never.load = FALSE)
	eq.li = gambit.solve.eq(tg, mixed=TRUE)

	eq.li = gambit.solve.eq(tg, mixed=TRUE, solver="gambit-enummixed -q -d 4")


	eqo = eq.outcomes(eq.li, tg=tg)
  eeqo = expected.eq.outcomes(eqo)

}




#' Finds one or all mixed strategy equilibria
gambit.solve.eq = function(tg, mixed=FALSE, just.spe=TRUE, efg.file=tg.efg.file.name(tg), efg.dir=NULL, gambit.dir="", solver=NULL, eq.dir = get.eq.dir(tg$gameId), save.eq = FALSE, solvemode=NULL, efg.file.with.dir = file.path(efg.dir,efg.file)) {

  restore.point("gambit.solve.eq")


	# internal solver not using gambit
	if (isTRUE(solvemode=="spe_xs")) {
		return(solve.all.tg.spe(tg=tg, eq.dir=eq.dir,save.eq=save.eq))

	}

	solver = get.gambit.solver(solver=solver, mixed=mixed, just.spe=just.spe, solvemode=solvemode)

	# Create temporary efg.file
  # if efg file does not exist
	use.temp.dir = FALSE
  if (length(efg.file.with.dir)==0) {
	  efg.dir = tempdir()
	  use.temp.dir = TRUE
    efg.file.with.dir = file.path(efg.dir, efg.file)
  }
	if (!isTRUE(file.exists(efg.file.with.dir)) | use.temp.dir) {
	  tg.to.efg(tg, file.with.dir = efg.file.with.dir)
	}

	#solver = "gambit-enumpure -q -P -D"
  start.time = Sys.time()

	com = paste0(gambit.dir, solver," ",efg.file.with.dir)
  res  = system(com, intern=TRUE)
  status = attr(res,"status")
  if (isTRUE(status==1)) {
    stop(res)
  }

  # no equilibrium found
  if (length(res)==0)
    return(NULL)

  eq.li = gambit.out.txt.to.eq.li(res, tg=tg, efg.move.inds=compute.efg.move.inds(tg=tg, efg.file=efg.file.with.dir))

  solve.time = Sys.time()-start.time
  attr(eq.li,"solve.time") = solve.time

  if (save.eq) {
	 eq.id = get.eq.id(tg=tg, just.spe = just.spe, mixed=mixed, solvemode=solvemode)
	 save.eq.li(eq.li=eq.li, eq.id=eq.id,eq.dir=eq.dir,tg=tg)
  }

  if (use.temp.dir) {
    file.remove(efg.file.with.dir)
  }

  eq.li
}

gambit.out.txt.to.eq.li = function(txt, tg, compact=FALSE, efg.move.inds=NULL) {
  restore.point("gambit.out.txt.to.eq.li")

  # no equilibrium found
  if (length(txt)==0) return(NULL)

  # in large games, equilibria may be longer than one line
  txt = merge.lines(txt)

  txt = sep.lines(txt,"NE,")[-1]



  # compact equilibirum representation
  # One equilibrium is just a vector that first contains for each
  # information set move the probability that it ocurs
  # afterwards, we also have the probability of moves of nature
  # ordered like .info.set.move.ind
  ceq.li = lapply(strsplit(txt,",",fixed=TRUE), function(vec) as.numeric(vec))


  # We have to inject these probabilties in our equilibrium template
  # tg$et.mat to generate an equilibrium data.frame eq.df
  # eq.mat will have the same dimensions than oco.df
  # each cell describes the probability that the particluar move
  # takes place:
  # (eq. prob for actions, prob for move of nature, 1 for transformations)
  # rowSums(eq.mat) then give the probability distribution over outcomes
  # for a given equilibrium.

  # et.ind are the indices of et.mat
  # that denote information sets
  et.ind = which(tg$et.mat<0)
  i = 1
  eq.li = lapply(seq_along(ceq.li), function(i) {
  	ceq.to.eq.mat(ceq = ceq.li[[i]],eq.ind=i, et.ind=et.ind, tg=tg, efg.move.inds=efg.move.inds)
  })


}

save.eq.li = function(eq.li, eq.id = get.eq.id(tg=tg,...),tg,  eq.dir=get.eq.dir(tg$gameId),...) {
	eq = list(
		eq.id = eq.id,
		tg.id = tg$tg.id,
		gameId = tg$gameId,
		variant = tg$variant,
		jg.hash = tg$jg.hash,
		eq.li = eq.li
	)
	file = paste0(eq.dir,"/",eq.id,".eq")
	saveRDS(eq,file)
}

xs.col.order = function(df, vg, mode="vars") {
	if (is.null(vg)) return(df)
	params = names(vg$params)
	vars = setdiff(vg$vars,params)
	ind.col = first.non.null(intersect("eqo.ind",colnames(df)),"eq.ind")
	if (has.col(df,"variant")) {
	  if (length(unique(df$variant))>1) ind.col = c("variant",ind.col)
	}
	cols = unique(c(ind.col, vars, paste0("payoff_",1:5), paste0("util_",1:5), params, colnames(df)))
	cols = intersect(cols, colnames(df))

	ord = try(do.call(order,df[,cols]))
	if (is(ord,"try-error")) return(df[,cols])
	df[ord,cols]
}

get.gambit.solver = function(solver=NULL, mixed=FALSE, just.spe=TRUE, solvemode=NULL) {
	if (!is.null(solver)) {
		return(solver)
	}

  if (is.null(solver)) {
    if (!mixed) {
      solver = "gambit-enumpure -q"
      if (just.spe) {
        solver = paste0(solver," -P")
      }
    } else {
      solver = "gambit-logit -q -e"
    }
  }
	solver

}


gambit.output.to.eq.li = function(txt,tg) {
  restore.point("gambit.output.to.eq.li")

  # no equilibrium found
  if (length(txt)==0)
    return(NULL)

  # in large games, equilibria may be longer than one line
  txt = merge.lines(txt)
  txt = sep.lines(txt,"NE,")[-1]



  # compact equilibirum representation
  # One equilibrium is just a vector that first contains for each
  # information set move the probability that it ocurs
  # afterwards, we also have the probability of moves of nature
  # ordered like .info.set.move.ind
  ceq.li = lapply(strsplit(txt,",",fixed=TRUE), function(vec) as.numeric(vec))


  # We have to inject these probabilties in our equilibrium template
  # tg$et.mat to generate an equilibrium data.frame eq.df
  # eq.mat will have the same dimensions than oco.df
  # each cell describes the probability that the particluar move
  # takes place:
  # (eq. prob for actions, prob for move of nature, 1 for transformations)
  # rowSums(eq.mat) then give the probability distribution over outcomes
  # for a given equilibrium.

  # et.ind are the indices of et.mat
  # that denote information sets
  et.ind = which(tg$et.mat<0)
  i = 1
  eq.li = lapply(seq_along(ceq.li), function(i) {
  	ceq.to.eq.mat(ceq = ceq.li[[i]],eq.ind=i, et.ind=et.ind, tg=tg)
  })


  eq.li

}


# Just found out recently that
# Gambit equilibrium output is sorted by player
# first and then by order in the efg file using depth first
# traversel.
#
# In contrast, gtree inform sets are numbered by stages
# and breadth first.
#
# The following function maps gtree .info.set.move indices
# to the position of the move in the Gambit equilibrium output
compute.efg.move.inds = function(tg,efg.txt = readLines(efg.file), efg.file=file.path(get.efg.dir(tg$gameId), tg.efg.file.name(tg))) {
  restore.point("efg.move.inds")

  rows = str.starts.with(efg.txt, "p ")
  efg.txt = str.between(efg.txt[rows],'" ',' "')
  player = as.integer(str.left.of(efg.txt," "))
  info.set.ind = as.integer(str.right.of(efg.txt," "))

  df = as_data_frame(nlist(player, info.set.ind))

  # Only first encounter of an information set is relevant
  # for gambit's output order
  df = df[!duplicated(info.set.ind),]
  df$efg.pos = seq_len(NROW(df))
  ord = order(df$player, df$efg.pos)
  df = df[ord,]
  df$efg.pos = seq_len(NROW(df))

  ise.df = tg$ise.df
  info.set.ind = 1
  efg.move.inds = c(unlist(lapply(df$info.set.ind, function(info.set.ind){
    row = which(ise.df$.info.set.ind == info.set.ind)
    start = ise.df$.info.set.move.ind.start[row]
    start:(start+ise.df$.num.moves[row]-1)
  })))

  efg.move.inds
  # Example:
  # efg.move.inds
  # [1]  1  2  3  4  7  8  9  5  6 10 11 12 13 14 15
  # This means ceq[5] should be set where tg$et.mat == -7
  # Let us define ceq.move.order with
  #   ceq.move.order[7] = ceq[5]
  # We thus need
  #   ceq.move.order[efg.move.inds] = ceq
  #

  # We have
  # ceq.move.order = ceq
  # ceq.move.order[efg.move.inds] = ceq
  # We have ceq[efg.move.inds]

}



# ceq is the returned vector by gambit describing an equilibrium
# it is a vector with as many elements as
# information set moves and contains values between 0 and 1, describing the move probabilty for each information set. A pure strategy contains only 0s and 1s.
# We convert it to eq.mat by writing the returned info set move probabilities at the right postion of et.mat.
#
# efg.move.inds is used because Gambit orders the information
# sets in the computed equilibria by player first and then
# in order of appearance in the efg file, while
# gtree orders them by stage.
ceq.to.eq.mat = function(ceq,eq.ind=1, tg,et.ind=which(tg$et.mat<0), efg.move.inds = compute.efg.move.inds(tg)) {
  restore.point("ceq.to.eq.mat")
  eq.mat = tg$et.mat

  # Account for different ordering
  # of gambit output and gtree's
  # information set numbers
  if (!is.null(efg.move.inds)) {
    ceq.gtree.order = integer(length(ceq))
    ceq.gtree.order[efg.move.inds] = ceq
    ceq = ceq.gtree.order
  }
  eq.mat[et.ind] = ceq[-eq.mat[et.ind]]

  .prob = rowProds(eq.mat)
  eq.mat = cbind(eq.mat, .prob)
  attr(eq.mat,"eq.ind") = eq.ind
  attr(eq.mat,"info.set.probs") = ceq
  eq.mat

}
