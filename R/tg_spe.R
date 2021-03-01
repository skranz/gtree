# Own implementations to solve pure SPE
# Probably it is better to use Gambit, however.
# The spi can nevertheless be computed to get
# some information about the number of strategy profiles

examples.make.tg.spe = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame")

	gameId = "LureOfAuthorityReduced"
	gameId = "CournotSmall"
	gameId = "BunchedUltimatum"
	gameId = "GiftEff"

	tg = get.tg(gameId=gameId, never.load=TRUE)
	sort(unlist(lapply(as.list(tg), object.size)),decreasing = TRUE)

	make.tg.spo.li(tg)

	spo.df = tg$spo.li[[1]]

	sg.df = tg$sg.df
	make.sg.spo.df(.sg.ind=3, tg=tg)
	tg$spo.li

	solve.all.tg.spe(tg=tg)
	tg$spe.li
	eqo.df = tg$spe.li[[1]]$eqo.df
	eq.li = tg$eq.li
	eq.li

	eqo.df = eq.outcomes(eq.li, tg=tg)

	gambit.eq.li = gambit.solve.eq(tg)
	eq.li
	gambit.eq.li
	identical(eq.li, gambit.eq.li)

	eq.li = tg.spe.li.to.eq.li(tg)
}


gtree.solve.spe = solve.all.tg.spe = function(tg, eq.dir = get.eq.dir(tg$gameId), save.eq=FALSE, keep.weakly.dominated=TRUE, verbose=TRUE, max.sp = first.non.null(getOption("gtree.max.sp",1000000))) {
	restore.point("solve.all.tg.spe")


	compute.tg.fields.for.internal.solver(tg,verbose = verbose)

	start.time = Sys.time()


	# solve via backward induction
	.sg.inds = rev(unique(tg$sg.df$.sg.ind))

	tg$spe.li = vector("list", length(.sg.inds))

	for (.sg.ind in .sg.inds) {
		tg$spe.li[[.sg.ind]] = solve.sg.spe(.sg.ind = .sg.ind, tg=tg, keep.weakly.dominated=keep.weakly.dominated)
	}
	tg$eq.li = tg.spe.li.to.eq.li(spe.li=tg$spe.li, tg=tg)

	solve.time = Sys.time()-start.time
  attr(tg$eq.li,"solve.time") = solve.time


	if (save.eq) {
		eq.id = get.eq.id(tg=tg,solvemode="spe_xs")
		save.eq.li(eq.li=tg$eq.li, tg=tg, eq.id=eq.id, eq.dir = eq.dir)
	}

	invisible(tg$eq.li)
}


compute.tg.fields.for.internal.solver = function(tg, verbose=TRUE, add.sg = TRUE, add.spi = TRUE, add.spo = TRUE, max.sp = first.non.null(getOption("gtree.max.sp",1000000))) {
  restore.point("compute.tg.fields.for.internal.solver")

  if (is.null(tg$sg.df) & add.sg) {
  	if (verbose) cat("\nCompute subgames...")
    compute.tg.subgames(tg)
    if (verbose) cat("...", NROW(tg$sg.df), " subgames found.")
  }

  if (add.spi | add.spo) {
    num.sp = sum(tg$sg.df$.num.strats.without.desc)
    if (num.sp > max.sp) {
      stop(paste0("Even using subgames there are ", num.sp, " relevant strategy profile.\nYet as an upper bound the internal gtree solver only attempts to fully compile and solve the game\nif there are fewer than ", max.sp, " relevant strategy profiles.\n You can change this bound e.g. to 2 Mio by calling \n\noptions(gtree.max.sp = 2000000)\n\nSome Gambit solvers, like gambit-logit, use algorithms that can still find one equilibrium\nin reasonable time for certain but not all games with a very large number of strategy profiles.\nSo try out game_gambit_solve with a fitting Gambit command line tool (see Gambit documention)." ))
    }
  }

  if (is.null(tg$spi.li) & add.spi) {
    make.tg.spi.li(tg)
  }

  if (is.null(tg$spo.li) & add.spo) {


    start.time = Sys.time()
  	if (verbose) cat("\nCompute mapping between strategy profiles and outcome probabilities for each subgame...")
    make.tg.spo.li(tg)
    if (verbose)
      cat("done in", format(Sys.time()-start.time), " mapping size =", format(object.size(tg$spo.li),units="auto"),"\n")
  }
}



clear.tg.spi.li = function(tg) {
  tg$spi.li = NULL
}

make.tg.spi.li = function(tg) {
	restore.point("make.tg.spi.li")
	tg$spi.li = lapply(tg$sg.df$.sg.ind, make.sg.spi,tg=tg)
	invisible(tg)
}



clear.tg.spo.li = function(tg) {
  tg$spo.li = NULL
}


make.tg.spo.li = function(tg) {
	restore.point("make.tg.spi.li")
	tg$spo.li = lapply(tg$sg.df$.sg.ind, make.sg.spo.df,tg=tg)
	invisible(tg)
}


# strategy profiles info for a specified subgame
# we need this for fast computation of the spo.df
make.sg.spi = function(.sg.ind=1,tg, include.descendants=FALSE) {
  restore.point("make.sg.spi")

	# shall info sets from descendant subgames be included
	# if FALSE we solve for SPE by backward induction
	# if TRUE we will find all NE of the subgame
  if (include.descendants) {
  	.info.set.inds = tg$sgi.df$.info.set.ind[tg$sgi.df$.sg.ind==.sg.ind]
  } else {
  	.info.set.inds = tg$sgi.df$.info.set.ind[tg$sgi.df$.sg.ind==.sg.ind & tg$sgi.df$.in.descendant==FALSE]
  }


	spi = tg$ise.df %>%
		filter(.info.set.ind %in% .info.set.inds) %>%
		select(.player, .lev.num, .info.set.ind, .var, .num.moves) %>%
		rename(moves = .num.moves)

	# these indexes will be used for fast computation
	# of the spo table
	if (NROW(spi)>0) {
  	spi$iso.row.add = c(0, cumsum(spi$moves[-NROW(spi)]))
  	spi$move.mult = rev(c(1,cumprod(rev(spi$moves[-1]))))
	} else {
		spi$iso.row.add = spi$move.mult = integer(0)

	}

  spi
}

# Create a list with three columns:
# sp: index of the strategy profile in the subgame
# .outcome: an outcome row in oco.df
# .prob: the probability that this outcome
# will be reached.
make.sg.spo.df = function(.sg.ind = 1, sg.df = tg$sg.df, sgi.df = tg$sgi.df, spi.df  = tg$spi.li[[.sg.ind]], tg, chunk.size= first.non.null(getOption("gtree.spo.chunk.size"), 20000)
) {
	restore.point("make.sg.spo.df")

  finished = FALSE
  on.exit({
    if (!finished) {
      msg = "
      If you have run out of memory, you could try to use gambit as solver, by calling gambit.solve.eq. Gambit requires less memory than gtree's internal solver, but sometimes takes longer to run.\n
      Alternatively, try to call\n\noptions(gtree.spo.chunk.size=1000)\n\nto reduce the memory requirement of our internal algorithms.\nYou can also set another chunk size than 10000. Smaller chunk sizes require longer run-time though. This may help for moderately larger games. If the game is too large in terms of the number of strategy profiles (with backward induction), also reducing the chunk size won't help."
      message(msg)
    }
  })

	# we need to specify outcomes for each strategy profile
	# of each subgame
	sg.df = sg.df[sg.df$.sg.ind == .sg.ind,]
	sgi.df = sgi.df[sgi.df$.sg.ind == .sg.ind,]
	spi = tg$spi.li[[.sg.ind]]

	# a super subgame without any own
	# information sets
	if (NROW(spi)==0) return(NULL)

	.info.set.inds = spi$.info.set.ind

	ise.df = filter(tg$ise.df, .info.set.ind %in% .info.set.inds)


	# relevant outcomes for this subgame
	outcomes = sg.df$.outcomes[[1]]

	oco.df = tg$oco.df[outcomes,,drop=FALSE]


  n.sp = prod(spi$moves)

  if (n.sp>chunk.size) {
    spo = make.sg.chunked.spo(sg.df,sgi.df,spi,.info.set.inds,ise.df,outcomes,oco.df, tg=tg, chunk.size=chunk.size)
    finished=TRUE
    return(spo)
  }
  n.ise = NROW(spi)
  n.out = length(outcomes)

  moves.df = sp.to.moves(sp = 1:n.sp, spi)

  # This matrix can be quite big
  # If memory is a concern, we may split the matrix
  # and the move.df in different chunks
  # and apply the stuff for each chunk seperately
  feas.mat <- matrix(TRUE,n.sp,n.out )

  ise.ind = 1
  move.ind = 1
  for (ise.ind in seq_len(n.ise)) {
  	# outcome values of the variable
  	# that is decided at this info set
  	char.move.vals = as.character(ise.df$.move.vals[[ise.ind]])
  	var = ise.df$.var[ise.ind]
  	.char.oco.val = as.character(oco.df[[var]])
  	is.ise.oco = find.info.set.outcomes(.info.set.ind = ise.df$.info.set.ind[ise.ind],tg = tg, oco.df=oco.df,return.logical = TRUE)

  	#move.ind = 1
    for (move.ind in seq_len(spi$moves[ise.ind])) {
      .char.move.val = char.move.vals[move.ind]

      # infeasible outcomes have a different
      # value of the info set variable than
      # the value of the current move
      infeas = which(is.ise.oco & .char.oco.val != .char.move.val)

      rows = which(moves.df[,ise.ind]==move.ind)
      feas.mat[rows,infeas] = FALSE
    }
  }

  spo = which(feas.mat,arr.ind = TRUE)
  colnames(spo) = c("sp",".outcome")
  spo = as.data.frame(spo)

  spo$.outcome = outcomes[spo$.outcome]
  spo = arrange(spo,sp,.outcome)

  spo = inner_join(spo, select(tg$oco.df,.outcome,.prob), by=".outcome")

  finished = TRUE
  spo


}


# Create a list with three columns:
# sp: index of the strategy profile in the subgame
# .outcome: an outcome row in oco.df
# .prob: the probability that this outcome
# will be reached.
make.sg.chunked.spo = function(sg.df,sgi.df,spi,.info.set.inds, ise.df, outcomes, oco.df, tg, chunk.size=20000) {
	restore.point("make.sg.spo.df.chunked")

  n.sp = prod(spi$moves)
  n.ise = NROW(spi)
  n.out = length(outcomes)

  is.ise.oco.li = lapply(1:n.ise, function(ise.ind) {
    find.info.set.outcomes(.info.set.ind = ise.df$.info.set.ind[ise.ind],tg = tg, oco.df=oco.df,return.logical = TRUE)
  })

  chunk.starts = seq(1,n.sp, by=chunk.size)
  chunk.start = 1


  spo.li = lapply(chunk.starts, function(chunk.start) {
    sp = chunk.start:(min(chunk.start+chunk.size-1,n.sp))
    moves.df = sp.to.moves(sp = sp,spi = spi,ise.df=ise.df)

    feas.mat = matrix(TRUE,length(sp),n.out )

    ise.ind = 0
    move.ind = 1
    while(ise.ind < n.ise) {
      ise.ind = ise.ind+1
    	# outcome values of the variable
    	# that is decided at this info set
    	char.move.vals = as.character(ise.df$.move.vals[[ise.ind]])
    	var = ise.df$.var[ise.ind]
    	.char.oco.val = as.character(oco.df[[var]])
    	is.ise.oco = is.ise.oco.li[[ise.ind]]

    	# Go through ALL moves in the
    	# current information set
    	# even if the move is not part
    	# of the current chunk
    	# to set INFEASIBLE outcomes
    	moves = seq_len(spi$moves[ise.ind])
    	move.ind = moves[2]
      for (move.ind in moves) {
        .char.move.val = char.move.vals[move.ind]

        # infeasible outcomes have a different
        # value of the info set variable than
        # the value of the current move
        infeas = which(is.ise.oco & .char.oco.val != .char.move.val)
        rows = which(moves.df[,ise.ind]==move.ind)

        feas.mat[rows,infeas] = FALSE
      }
    }
    spo = which(feas.mat,arr.ind = TRUE)
    colnames(spo) = c("sp",".outcome")
    spo[,1] = sp[spo[,1]]
    spo
  })
  spo = do.call(rbind,spo.li)
  spo = as.data.frame(spo)
  spo$.outcome = outcomes[spo$.outcome]
  spo = arrange(spo,sp,.outcome)

  spo = inner_join(spo, select(tg$oco.df,.outcome,.prob), by=".outcome")

  spo


}


# matrix of moves at each information set to strategy profile index
moves.to.sp = function(moves,spi) {
  if (is.null(dim(moves)))
    return(sum(moves*spi$move.mult))

  sp = rep(1, NROW(moves))
  col = 2
  for (col in 1:NCOL(moves)) {
    sp = sp + (moves[,col]-1)*spi$move.mult[col]
  }
  sp
}

# strategy profile index to matrix of moves at each information set
sp.to.moves = function(sp, spi, ise.df, wide=TRUE) {
  restore.point("sp.to.moves")

  moves = matrix(0, NROW(sp), NROW(spi))

  col = NROW(spi)
  for (col in 1:NROW(spi)) {
    # integer division
    ind = ( (sp-1) %/% spi$move.mult[col])+1
    moves[,col] = ind
    # remainder
    sp = sp - (ind-1)*spi$move.mult[col]
  }

  if (wide) {
    colnames(moves) = as.character(spi$.info.set.ind)
    return(moves)
  }

  org.sp = sp
  moves.df = tibble(
    sp = rep(sp,times=NCOL(moves)),
  	.info.set.ind = rep(spi$.info.set.ind, each = length(org.sp)),
    .move.ind = as.vector(moves),
  	.info.set.move.ind = .move.ind - 1 + ise.df$.info.set.move.ind.start[.info.set.ind]
  )
  moves.df

}

# get indeces of player i's strategy
# given a vector of strategy profiles sp
# and a specific subgame specified spi
sp.to.s.i = function(player = 1,sp, spi) {
  restore.point("sp.to.s.i")

  ise=which(spi$.player == player)
	# player makes no moves
	if (length(ise)==0)
		return(rep(0L, NROW(sp)))

  move.mult = c(rev(cumprod(spi$moves[ise]))[-1],1)

	sp_i = integer(NROW(sp))
	for (col in 1:max(ise)) {
    # integer division
    move_1 = ( (sp-1) %/% spi$move.mult[col])
    if (col %in% ise) {
    	ise.ind = which(ise==col)
      sp_i = sp_i + move_1 * move.mult[ise.ind]
    }
    # remainder
    sp = sp - (move_1)*spi$move.mult[col]
  }
	sp_i+1
}


# get indeces of other player strategy profiles
sp.to.sp.not.i = function(player = 1,sp, spi, 	cols = which(spi$.player != player)
) {
  restore.point("sp.to.sp.not.i")

	# other players make no moves
	if (length(cols)==0)
		return(rep(0L, NROW(sp)))


	sp_i = integer(NROW(sp))
  col = cols[1]
	for (col in 1:max(cols)) {
    # integer division
    move_1 = ( (sp-1) %/% spi$move.mult[col])
    if (col %in% cols)
    	sp_i = sp_i + move_1 * spi$move.mult[col]

    # remainder
    sp = sp - (move_1)*spi$move.mult[col]
  }

	sp_i = id.to.index(sp_i, sort(unique(sp_i)))
	sp_i
}

# Create a matrix with all strategies of player i
# in the rows and all strategies /
# strategy profiles of the other players
# in the columns
#
# Each matrix cell contains the strategy number
#
# The matrix can be used to check whether we have
# (weakly) dominated strategies in the subgame
sg.matrix = function(player=1,spi) {
  restore.point("sg.matrix")

  row.ise = which(spi$.player == player)
  col.ise = which(spi$.player != player)

  nrows = prod(spi$moves[row.ise])
  ncols = prod(spi$moves[col.ise])

  sp.row = integer(nrows)
  move.mult = c(rev(cumprod(spi$moves[row.ise]))[-1],1)
  counter = 1
  for (counter in seq_along(row.ise)) {
    ise = row.ise[counter]
    moves = rep(rep(1:spi$moves[ise], each=move.mult[counter]), length.out=nrows)
    sp.row = sp.row + (moves-1)*spi$move.mult[ise]
  }

  sp.col = integer(ncols)
  move.mult = c(rev(cumprod(spi$moves[col.ise]))[-1],1)
  counter = 1
  for (counter in seq_along(col.ise)) {
    ise = col.ise[counter]
    moves = rep(rep(1:spi$moves[ise], each=move.mult[counter]), length.out=ncols)
    sp.col = sp.col + (moves-1)*spi$move.mult[ise]
  }

  mat = matrix(sp.col, nrows, ncols, byrow=TRUE)
  mat = mat + sp.row+1
  mat
}


#
spo.to.speu = function(spo.df, tg=NULL, add.outcomes = FALSE, as.data.table = FALSE) {
	restore.point("spo.to.eu.df")
	all.players = seq_len(tg$params$numPlayers)
	oco = tg$oco.df[,c(".outcome",paste0("util_",all.players))]
	spo.df = left_join(spo.df, oco, by=".outcome")

	code = paste0("Eutil_", all.players," =sum(util_",all.players," * .prob) / sum(.prob)", collapse=",")
	if (add.outcomes) {
		code = paste0(code, ", .outcomes = list(.outcome)")
	}

	# group by strategy profiles
	# perform computation with a
	# data.table because of substantial speed gains
	spo.df = setDT(spo.df)

	speu = spo.df %>%
	  lazy_dt() %>%
	  group_by(sp) %>%
		s_summarise(code)
	if (as.data.table)
	  return(as.data.table(speu))

	return(as_tibble(speu))

}

# solve all spe of subgame .sg.ind
# assumes that descendent subgames have already been solved
# and uses backward induction
solve.sg.spe = function(.sg.ind=1, tg, keep.weakly.dominated=TRUE) {

	# 1. We first generate a grid of all children
	#    subgame equilibrium outcome combinations
	# 2. We solve the subgame for each row of that grid


	restore.point("solve.sg.spe")

	if (.sg.ind == 1 & tg$sg.df$num.direct.info.sets[[.sg.ind]]==0) {
		return(solve.nature.super.sg(tg))
	}

	# all children subgames
	child.sg = get.child.subgames(.sg.ind, tg)

	# no children subgames => solve directly
	if (length(child.sg)==0) {
		eq = solve.sg.spe.given.remove(.sg.ind=.sg.ind, tg=tg, keep.weakly.dominated=keep.weakly.dominated)
		return(eq)
	}

	# list of equilibrium outcome indices
	# for all child-subgames
	eqo.li = lapply(child.sg, function(cind) {
		tg$spe.li[[cind]]$eqo.df$.eqo.ind
	})

	# grid of all child subgame eq. outcomes
	eqo.grid = expand.grid(eqo.li)
	names(eqo.grid) = child.sg

	tg$sg.df

	# vector of all possible outcomes (oco rows) of
	# child subgames
	child.sg.outcomes = unique(unlist(lapply(child.sg, function(cind) {
		tg$sg.df$.outcomes[[cind]]
	})))


	# loop through each child subgame eq. outcome combination
	# and compute corresponding spe of this subgame
	eq.li = lapply(seq_len(NROW(eqo.grid)), function(grid.row) {
		eqo.outcomes =  unique(unlist(lapply(seq_along(child.sg), function(i) {
			cind = child.sg[i]
			.eqo.ind = eqo.grid[grid.row,i]
			tg$spe.li[[cind]]$eqo.df$.outcomes[[.eqo.ind]]
		})))

		remove.outcomes = setdiff(child.sg.outcomes,eqo.outcomes)
		eq = solve.sg.spe.given.remove(.sg.ind = .sg.ind,tg = tg, remove.outcomes = remove.outcomes, child.eqo.inds = as.integer(eqo.grid[grid.row,]),  keep.weakly.dominated=keep.weakly.dominated)

		# to do: need to add subgame info to eq
		return(eq)
	})

	speq.df = bind_rows(lapply(eq.li, function(eq) eq$speq.df))
	eqo.df = bind_rows(lapply(eq.li, function(eq) eq$eqo.df))

	nlist(speq.df, eqo.df)
}

# internal function to solve sg.spe
# remove.outcomes depend on the equilibria
# of the child subgames
# backward induction simply works
# by removing from spo.df all outcomes from remove.outcomes
solve.sg.spe.given.remove = function(.sg.ind=1, tg, remove.outcomes=NULL, child.eqo.inds = NULL, keep.weakly.dominated=TRUE) {
	restore.point("solve.sg.spe.given.remove")

	spo.df = tg$spo.li[[.sg.ind]]

	#df = left_join(spo.df, tg$oco.df, by=".outcome")

	if (!is.null(remove.outcomes)) {
		rows = !spo.df$.outcome %in% remove.outcomes
		spo.df = spo.df[rows,,drop=FALSE]
	}

	spi = tg$spi.li[[.sg.ind]]

	players = unique(spi$.player) # players who pick action
	all.players = seq_len(tg$params$numPlayers)

	# compute expected utility for each player
	# and each strategy profile sp
	speu = spo.to.speu(spo.df = spo.df, tg=tg)

	# for each player who moves
	# mark best reply strategy profiles
	for (player in players) {
		speu[[paste0("sp_",player)]] = sp.to.sp.not.i(player, speu$sp, spi)
		speu = s_group_by(speu, paste0("sp_",player)) %>%
			s_mutate(paste0("is_br_",player," = Eutil_",player," == max(Eutil_",player,")"))
	}

	# a strategy profile is an equilibrium if it is
	# a best reply for each player
	cond = parse(text=paste0("speu$is_br_",players, collapse=" & "))
	speu$is_eq = eval(cond)

	eq.rows = which(speu$is_eq)
	if (!keep.weakly.dominated) {
    #restore.point("remove.weakly.dominated.eq")
	  i = 2
	  for (i in rev(players)) {
	    sg.mat = sg.matrix(i,spi)
	    weak.dom = sapply(eq.rows, function(eq.row) {
	      is.sg.strat.weakly.dominated(sp = speu$sp[eq.row],player=i,speu=speu, sg.mat=sg.mat,  spi=spi)
	    })
	    eq.rows = eq.rows[!weak.dom]
	  }
	}


	# only keep equilibria
	speu = speu[eq.rows,,drop=FALSE]



	# add outcomes and store them in a list .outcomes
	# we will need them for backward induction
	speq = speu %>%
		left_join(select(spo.df,sp,.outcome), by="sp") %>%
		s_group_by(c("sp",paste0("Eutil_",all.players))) %>%
		summarize(.outcomes = list(.outcome), .outcomes.id = paste0(.outcome, collapse=","))%>%
		ungroup()

	eqo.df = 	speq[!duplicated(speq$.outcomes.id),]
	speq$.eqo.ind = match(speq$.outcomes.id,eqo.df$.outcomes.id)
	eqo.df$.eqo.ind = seq_len(NROW(eqo.df))
	cols = setdiff(unique(c(".eqo.ind",colnames(eqo.df))),c("sp",".outcomes.id"))
	eqo.df = eqo.df[,cols]

	cols = setdiff(unique(colnames(speq)),c(".outcomes.id"))
	speq = speq[,cols]

	speq$child.eqo.inds = replicate(n = NROW(speq),child.eqo.inds,simplify = FALSE)
	eqo.df$child.eqo.inds = replicate(n = NROW(eqo.df),child.eqo.inds,simplify = FALSE)

	nlist(speq.df = speq, eqo.df)
}


is.sg.strat.weakly.dominated = function(s.i=sp.to.s.i(player,sp,spi), player,speu,spi, sp=NULL, sg.mat = sg.matrix(player=player, spi=spi)) {
  restore.point("is.sg.strat.weakly.dominated")

  speu.row = match(sg.mat, speu$sp)
  speu.mat = sg.mat
  speu.mat[] = speu[[paste0("Eutil_", player)]][speu.row]

  ceu.mat = matrix(speu.mat[s.i,],NROW(speu.mat), NCOL(speu.mat), byrow=TRUE)

  # Payoffs of chosen strategy
  # minus payoff of alternative strategies
  diff = ceu.mat - speu.mat

  maxs = rowMaxs(diff)
  mins = rowMins(diff)

  # If maxs <= 0 the chosen strategy is never better
  # if mins < 0 it is sometimes worse
  # Both conditions mean the strategy is weakly
  # dominated
  if (any(maxs<=0 & mins<0))
    return(TRUE)

  return(FALSE)
}

# the largest subgame for a game that
# starts with a move of nature
solve.nature.super.sg = function(tg) {
	restore.point("solve.nature.super.sg")

	.sg.ind = 1
	# all children subgames
	child.sg = get.child.subgames(.sg.ind, tg)

	# list of equilibrium outcome indices
	# for all child-subgames
	eqo.li = lapply(child.sg, function(cind) {
		tg$spe.li[[cind]]$eqo.df$.eqo.ind
	})

	# grid of all child subgame eq. outcomes
	eqo.grid = expand.grid(eqo.li)
	names(eqo.grid) = child.sg

	grid.row = 1
	li = lapply(seq_len(NROW(eqo.grid)), function(grid.row) {
		eqo.outcomes =  unique(unlist(lapply(seq_along(child.sg), function(i) {
			cind = child.sg[i]
			.eqo.ind = eqo.grid[grid.row,i]
			tg$spe.li[[cind]]$eqo.df$.outcomes[[.eqo.ind]]
		})))

		cei = as.integer(eqo.grid[grid.row,])
		names(cei) = NULL
		row.eqo.df = tibble(.eqo.ind=1,.outcomes=eqo.outcomes, child.eqo.inds=list(cei))
	})
	eqo.df = bind_rows(li)
	eqo.df$.eqo.ind = seq_len(NROW(eqo.df))
	speq.df = eqo.df %>%
		mutate(sp=NA) %>%
		select(sp,.outcomes,.eqo.ind, child.eqo.inds)

	list(speq.df=speq.df, eqo.df=eqo.df)
}


# An equilibrium when we solve a game by gambit an transform it, is specified by an eq.mat.
# It has one row for each outcome and
# one column for each variable that is an action or move of nature. The value is the probability that the variable takes in equilibrium the value it has in that row of oco.df.
tg.spe.li.to.eq.li = function(spe.li,tg, .sg.ind=1) {
	restore.point("tg.spe.li.to.eq.li")
	speq.df = spe.li[[.sg.ind]]$speq.df

	num.moves = sum(tg$ise.df$.num.moves * tg$ise.df$.num.nodes)
	ceq.start = matrix(0, nrow=1, ncol=num.moves)

	row = 1
	ceq.li = lapply(1:NROW(speq.df), function(row) {
		speq = speq.df[row,]
		recursive.speq.to.ceq(speq = speq,ceq = ceq.start,.sg.ind = .sg.ind,tg = tg, spe.li=spe.li)
	})
	ceq.mat = do.call(rbind,ceq.li)

	et.ind=which(tg$et.mat<0)

	# now use the same conversion as in
	# xs_gambit
	eq.li = lapply(seq_len(NROW(ceq.mat)), function(row) {
	  # We set pos.transform to NULL, since internal computation
	  # already orders the information set moves in ceq by information
	  # set number, not by player first, as Gambit does
		eq.mat = ceq.to.eq.mat(ceq=ceq.mat[row,], eq.ind=row, tg=tg, et.ind=et.ind,efg.move.inds =NULL)
	})

	eq.li
}


recursive.speq.to.ceq = function(ceq, speq, .sg.ind, tg, spe.li=tg$spe.li) {
	#restore.point("recursive.speq.to.ceq")
	#cat("\n.sg.ind = ", .sg.ind, " NROW(ceq) = ",NROW(ceq))

	spi = tg$spi.li[[.sg.ind]]

	# satisfied unless for nature super subgame
	if (NROW(spi)>0) {

		moves.df = sp.to.moves(speq$sp, spi=spi, ise.df = tg$ise.df, wide=FALSE)

		# insert a 1 for the info set moves taken
		# in equilibrium
		ceq[,moves.df$.info.set.move.ind] = 1

	}

	# all children subgames
	child.sg = get.child.subgames(.sg.ind, tg)

	# no more subgames => we are done
	if (length(child.sg)==0) {
		return(ceq)
	}

	# child.eqo
	# for each subgame we have a
	# single equilibrium outcome
	child.eqo = speq$child.eqo.inds[[1]]

	# yet there may be multiple child
	# equilibria associated with a
	# child outcome

	#restore.point("recursive.speq.to.ceq.2")
	# loop through all child.sg
	i = 1
	for (i in seq_along(child.sg)) {
		# get all speq (subgame equilibria)
		# of the current child sg
		cdf = spe.li[[ child.sg[i] ]]$speq.df
		cdf = cdf[cdf$.eqo.ind == child.eqo[i],]

		# loop through all subgame equilibria
		# of the current child subgame
		ceq.li = lapply(seq_len(NROW(cdf)), function(row) {
			#restore.point("recursive.speq.to.ceq.3")
			speq = cdf[row,]
			recursive.speq.to.ceq(speq=speq,ceq = ceq,.sg.ind = child.sg[i],tg = tg, spe.li=spe.li)

		})
		# for each subgame equilibrium
		# we get new ceq rows
		ceq = do.call(rbind,ceq.li)
	}
	ceq
}
