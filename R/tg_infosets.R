examples.make.tg.iso.df = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame")
	tg = get.tg(gameId="BunchedUltimatum", never.load=TRUE)
	tg = get.tg(gameId="GiftEff",variant=1,never.load = TRUE)
	tg = get.tg(gameId="StackelRandomCost",variant=1,never.load = TRUE)

	set.tg.util(tg)
	#make.tg.iso.df(tg)
	make.tg.ise.df(tg)
	ise.df = tg$ise.df
	compute.tg.subgames(tg)
	sg.df = tg$sg.df
	sgi.df = tg$sgi.df

	make.tg.spi.li(tg)
	spi = tg$spi.li[[2]]
	spi = tg$spi.li[[1]]

	make.tg.spo.li(tg)

	make.sg.spo.df(.sg.ind = 1,tg=tg)
	tg$spo.li

	solve.all.tg.spe(tg=tg)
	tg$spe.li
	eqo.df = tg$spe.li[[1]]$eqo.df

	solve.sg.spe(.sg.ind = 1, tg=tg)
}



# not clear whether we still need iso.df...
make.tg.iso.df = function(tg) {
	tg$iso.df = tg.to.iso.df(tg)
}

# information sets moves and outcomes
tg.to.iso.df = function(tg) {
  restore.point("tg.to.iso.df")
  lev.li = tg$lev.li

  levels = which(sapply(lev.li, function(lev) lev$type=="action"))
  li = lapply(levels, function(le) {
    lev.df.to.iso(lev=lev.li[[le]])
  })
  iso.df = bind_rows(li)
  #iso.df = select(iso.df, .player, .lev.num,.info.set.ind, .info.set,.move.ind,.move.name, .infeasible)
	iso.df = select(iso.df, .player, .lev.num,.info.set.ind, .info.set,.move.ind,.var,.char.move.val)

  iso.df = arrange(iso.df, .player, .lev.num, .info.set.ind, .move.ind)
  iso.df
}


# computes the iso.df from a level.df
lev.df.to.iso = function(lev) {
  restore.point("lev.df.to.iso")

	lev.df = lev$lev.df
	know.mat = lev$know.mat
  #know.var.group = identical.rows.groups(know.mat)
  know.var.group = lev.df$.know.var.group

  kv = 1
  li = lapply(unique(know.var.group), function(kv) {
    rows = which(know.var.group==kv)
    if (length(rows)==0) return(NULL)
    know.vars = lev$know.var.li[[kv]]

    by = unique(c(know.vars, lev$var))

    idf = lev.df[rows,c(by,".move.ind",".info.set",".info.set.ind", ".player"), drop=FALSE]

    idf$.char.move.val = as.character(lev.df[[lev$var]][rows])
    idf = unique(idf)

    df = select(idf,.player,.info.set,.info.set.ind,.move.ind, .char.move.val)
    return(df)

    # ommited compute infeasible
    jdf = left_join(idf, oco.df[,c(".outcome",by)], by=by)

    fun = function(idf) {
      outcomes = unique(idf$.outcome)
      summarise(group_by(idf,.move.ind, .move.name, .player), .infeasible = list(setdiff(outcomes, .outcome)))
    }
    inf.df = do(group_by(jdf,.info.set.ind,.info.set), fun(.)) %>% ungroup()
    inf.df
  })
  res = bind_rows(li)
  res$.lev.num = lev$lev.num
  res$.var = lev$var

  res
}

# computes the ise.df contains some summary statistics for information sets
# these will be helpful when defining subgames later on
make.tg.ise.df = function(tg) {
	restore.point("tg.to.ise.df")
	tg$action.levels = which(sapply(tg$lev.li, function(lev) lev$type=="action"))

	li = lapply(tg$lev.li[tg$action.levels], function(lev) {
		restore.point("sfhdhfhdu")
		lev.df = lev$lev.df
		lev.df$.val = lev.df[[lev$var]]
		lev.df$.var = lev$var
		lev.df %>%
			group_by(.info.set.ind, .info.set, .player, .var) %>%
			summarize(.num.moves = length(unique(.move.ind)), .num.nodes = sum(.move.ind==1), .lev.num = as.integer(lev$lev.num), .info.set.move.ind.start = min(.info.set.move.ind),.move.vals = list(unique(.val))) %>%
			ungroup()
	})
	tg$ise.df = bind_rows(li)
	invisible(tg)
}

# known.var groups:
# group multiple information sets on levels to know.var sets
# each information set in which the same variables are observed
# will belong to the same know.var group.
# Note that information sets from different players may belong
# to the same know.var group.
# know.var groups are only used to allow fast
# vectorized computation of the iso.df
make.tg.know.var.groups = function(tg) {
	tg$lev.li = lapply(tg$lev.li, make.tg.lev.know.var.groups)
}
make.tg.lev.know.var.groups = function(lev) {
	restore.point("make.tg.lev.know.var.groups")
	if (lev$type != "action") return(lev)

	lev.df = lev$lev.df
	players = unique(lev.df$.player)
	know.mat = lev$know.li[[players[1]]]
	know.mat[,] = NA

	for (player in players) {
		rows = lev.df$.player == player
		know.mat[rows,] = lev$know.li[[player]][rows,]
	}
	lev$know.mat = know.mat

	.know.var.group = identical.rows.groups(know.mat)
	lev$lev.df$.know.var.group = .know.var.group
	lev$know.var.li = lapply(unique(.know.var.group), function(kv) {
		row = which(.know.var.group == kv)[1]
		vars = colnames(know.mat)[know.mat[row]]
	})
	lev

}

clear.tg.subgames = function(tg) {
	tg$sgi.df = NULL
	tg$sg.df = NULL
}


compute.tg.subgames = function(tg) {
	restore.point("compute.tg.subgames")

	# no action level
	if (length(tg$action.levels)==0) {
		stop("Computation of subgames for games without actions not yet specified.")
	}


	# first take the last action level
	lev.num = rev(tg$action.levels)[1]

	ise.df = tg$ise.df

	# On the last level all singleton information sets are subgames
	rows = which(ise.df$.lev.num == lev.num & ise.df$.num.nodes == 1)

	if (length(rows)>0) {
		sgi.df1 = data_frame(.lev.num = lev.num, .root.info.set.ind = ise.df$.info.set.ind[rows],.info.set.ind = .root.info.set.ind)
	} else {
		sgi.df1 = NULL
	}

	#sgi.li = vector("list",length(tg$action.levels))
	#sgi.li[[1]] = sgi.df


	# to do for earlier levels
	# good vectorized method to find all descendant information sets

	lev.num = 3
	li = lapply(rev(tg$action.levels)[-1], function(lev.num) {
		restore.point("compute.subgame.inner")
		lev = tg$lev.li[[lev.num]]

		dis.df = find.lev.descendant.info.sets(lev=lev, tg=tg,add.roots = TRUE)
		sgi.df = dis.df %>%
			rename(.root.info.set.ind = pinfo.set.ind) %>%
			mutate(.lev.num = lev.num) %>%
			select(.lev.num, .root.info.set.ind, .info.set.ind)
	})

	sgi.df = bind_rows(c(list(sgi.df1),li)) %>%
		arrange(.root.info.set.ind, .info.set.ind) %>%
		ungroup()


	# if first subgame does not contain all
	# information set, we need to add the
	# super subgame (whole game)
	if (NROW(sgi.df)>0) {
		size1 = sum(sgi.df$.root.info.set.ind == sgi.df$.root.info.set.ind[1])
	} else {
		size1  = 0
	}
	# add super subgame
	if (size1<NROW(ise.df)) {
		super.df = data_frame(.lev.num = 0,.root.info.set.ind=0, .info.set.ind = ise.df$.info.set.ind)
		sgi.df = rbind(super.df, sgi.df)
	}



	# generate a subgame index
	# subgames starting at earlier information nodes
	# will have lower subgame indices
	roots = sort(unique(sgi.df$.root.info.set.ind))
	sgi.df$.sg.ind = match(sgi.df$.root.info.set.ind,roots)
	sgi.df$is.root = sgi.df$.root.info.set.ind == sgi.df$.info.set.ind

	# mark information sets that are member of a descendant
	# subgame
	sgi.df = sgi.df %>%
		group_by(.info.set.ind) %>%
		mutate(.in.descendant = c(rep(TRUE,n()-1),FALSE)) %>%
		ungroup()

	# create some summary information about subgames
	df = sgi.df %>%
		left_join(select(ise.df,.info.set,.info.set.ind,.player,.num.moves), by=".info.set.ind")

	sg.df = df %>%
		group_by(.sg.ind,.lev.num, .root.info.set.ind) %>%
		summarize(num.direct.info.sets = sum(!.in.descendant), .num.strats.without.desc = prod(ifelse(.in.descendant,1,.num.moves))*(num.direct.info.sets>0), .num.strats = prod(.num.moves), .num.players=length(unique(.player)))

	max.or.na = function(x) {
		if (length(x)==0) return(NA)
		max(x)
	}

	# find parent indices
	#sgi.df$.sg.ind = as.numeric(sgi.df$.sg.ind)
	sg.df = suppressWarnings(sg.df %>%
		group_by(.sg.ind, .root.info.set.ind) %>%
		mutate(parent.sg.ind = max.or.na(sgi.df$.sg.ind[sgi.df$.info.set.ind == .root.info.set.ind & !sgi.df$is.root])))

	# find possible outcomes for each subgame
	sg.df = compute.sg.outcomes(sg.df = sg.df, tg=tg)

	tg$sgi.df = sgi.df
	tg$sg.df = sg.df
	invisible(tg)

}

# find for each info set in the level
# all descendant info sets (all nodes must be descendants)
# uses know.var.groups for vectorization
find.lev.descendant.info.sets = function(lev, tg, subgames.only=TRUE, add.roots = FALSE) {
	restore.point("find.lev.descendant.info.sets")

	lev.df = lev$lev.df
	kvg = 1
	li = lapply(seq_along(lev$know.var.li), function(kvg) {
		restore.point("find.lev.descendant.info.sets.inner1")
		# find info sets of the current know.var group
		lev.rows = which(lev.df$.know.var.group == kvg & lev.df$.move.ind==1)

		.info.set.inds = lev.df$.info.set.ind[lev.rows]

		# may only consider information sets that are singletons
		# since only those can start subgames
		if (subgames.only) {
			singleton = tg$ise.df$.num.nodes[.info.set.inds] == 1
			.info.set.inds = .info.set.inds[singleton]
			lev.rows = lev.rows[singleton]
		}

		cols = setdiff(lev$know.var.li[[kvg]],c(lev$var,"variant","numPlayers"))
		if (length(cols)>0) {
			cols.id = paste.matrix.cols(lev.df[lev.rows,cols,drop=FALSE])
		} else {
			cols.id = rep("0",length(lev.rows))
		}

		# now we check all subsequent action levels to find descendant
		# information sets
		desc.levels = tg$action.levels[tg$action.levels > lev$lev.num]

		#dlev = tg$lev.li[[2]]
		li = lapply(tg$lev.li[desc.levels], function(dlev) {
			restore.point("find.lev.descendant.info.sets.inner2")
			dlev.df = dlev$lev.df
			if (length(cols)>0) {
				dcols.id = paste.matrix.cols(dlev.df[,cols,drop=FALSE])
			} else {
				dcols.id = rep("0",NROW(dlev.df))
			}

			# we try to match each row in dlev.df to our
			# lev.rows in lev.df
			dlev.df$.match = match(dcols.id, cols.id)

			# try to find those information sets
			# that are fully contained
			# this means dind must be the same
			# for all moves and
			# cannot contain any NA
			dlev.df = dlev.df %>%
				group_by(.info.set.ind) %>%
				mutate(.contained = sum(is.na(.match)) == 0 & length(unique(.match)) == 1) %>%
				ungroup(pinfo.set.ind=)

			# return a data_frame with the
			# indices of the parent info set and the
			# descendant info sets
			match.df = dlev.df %>%
				#filter(.contained, .move.ind==1) %>%
				mutate(pinfo.set.ind = .info.set.inds[.match]) %>%
				select(pinfo.set.ind, .info.set.ind, .contained) %>%
				unique()


			if (add.roots) {
				match.df = rbind(data_frame(pinfo.set.ind=.info.set.inds, .info.set.ind = .info.set.inds, .contained=TRUE),match.df) %>%
					arrange(pinfo.set.ind, .info.set.ind)
			}

			match.df
		})
		return(bind_rows(li))
	})

	df = bind_rows(li)

	df = df %>%
		group_by(pinfo.set.ind) %>%
		mutate(all.contained = all(.contained))

	# remove info.sets whose nodes are not fully contained
	if (subgames.only) {
		df = filter(df, all.contained)
	}
	df
}

# compute for each subgame the possible outcomes of the subgame
# use known.var matrices for vectorization
compute.sg.outcomes = function(sg.df, tg, add.to.sg.df = TRUE) {
	restore.point("compute.sg.outcomes")

	#lev = tg$lev.li[[3]]
	# loop through action levels for known.var vectorization
	li = lapply(tg$lev.li[rev(tg$action.levels)], function(lev) {
		#
		lev.df = lev$lev.df
		kvg = 1
		li = lapply(seq_along(lev$know.var.li), function(kvg) {
			restore.point("compute.sg.outcomes.inner2")
			# find info sets at which subgame starts of the current know.var group
			lev.rows = which(lev.df$.know.var.group == kvg & lev.df$.info.set.ind %in% sg.df$.root.info.set.ind & lev.df$.move.ind == 1)
			if (length(lev.rows)==0) return(NULL)

			.info.set.inds = lev.df$.info.set.ind[lev.rows]
			cols = setdiff(lev$know.var.li[[kvg]],c(lev$var,"variant","numPlayers"))
			# if no cols are observed the subgame is the whole game
			if (length(cols)==0) {
				all.df = data_frame(.root.info.set.ind = .info.set.inds, .outcomes=list(seq_len(NROW(tg$oco.df))) )
				return(all.df)
			}

			cols.id = paste.matrix.cols(lev.df[lev.rows,cols,drop=FALSE])
			ocols.id = paste.matrix.cols(tg$oco.df[,cols,drop=FALSE])

			# match each outcome rows to subgame root nodes
			# at the current level
			# non-contained outcomes will have an NA
			oco.match = match(ocols.id, cols.id)

			df = data_frame(.root.info.set.ind=.info.set.inds[oco.match],.outcome=tg$oco.df$.outcome)
			df %>%
				group_by(.root.info.set.ind) %>%
				summarize(.outcomes = list(.outcome))
		})
		bind_rows(li)
	})
	df = bind_rows(li)

	# manually add outcomes of super subgame
	if (sg.df$.root.info.set.ind[1]==0) {
		df0 = data_frame(.root.info.set.ind = 0, .outcomes=list(tg$oco.df$.outcome))
		if (NROW(df)>0) {
			df = rbind(df0, df)
		} else {
			df = df0
		}
	}

	# TO DO: .info.set.ind is so far not globally
	# defined by counter starts at each level
	# need global counter!!!
	if (add.to.sg.df) {
		if (".outcomes" %in% colnames(sg.df))
			sg.df = select(sg.df, -.outcomes)
		res = left_join(sg.df,df, by=".root.info.set.ind")

	}

}

find.info.set.outcomes = function(.info.set.ind, tg, oco.df = tg$oco.df, return.logical=FALSE) {
	restore.point("find.info.set.outcomes")

	.lev.num = tg$ise.df$.lev.num[.info.set.ind]
	lev = tg$lev.li[[.lev.num]]
	row = which(lev$lev.df$.info.set.ind == .info.set.ind)[1]
	keys = setdiff(colnames(lev$know.mat)[lev$know.mat[row,]],lev$var)
	vals = as.list(lev$lev.df[row, keys])

	code = paste0('oco.df[["', keys,'"]] == vals[["',keys,'"]]', collapse=" & ")
	if (!return.logical)
		code = paste0("oco.df$.outcome[ ",code," ]")
	call = parse(text=code)

	eval(call)
}

get.child.subgames = function(.sg.ind, tg) {
	rows = tg$sg.df$parent.sg.ind %in% .sg.ind
	tg$sg.df$.sg.ind[rows]
}

