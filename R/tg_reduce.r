# Try to reduce a tg game
# Moving backwards over levels
# Evaluate moves of nature
# Check for dominant actions

examples.reduce.tg = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame/")
	gameId = "BunchedUltimatum"
	gameId = "Cournot"
	gameId = "Centipede"
	tg = get.tg(gameId = gameId)
  rtg = reduce.tg(tg)

  eq.li = gambit.solve.eq(tg, just.spe=TRUE)
  eqo.df = eq.outcomes(eq.li, tg=tg)


  oco.df = tg$oco.df
  roco.df = rtg$oco.df

  tg$ise.df
  rtg$ise.dfstage.df

  stage.df = rtg$stage.df

  efg = tg.to.efg(rtg)
  eq.li = gambit.solve.eq(rtg, just.spe=TRUE)
  eqo.df = eq.outcomes(eq.li, tg=rtg)
  eqo.df

  efg = tg.to.efg(tg, path=getwd())
  eq.li = gambit.solve.eq(tg, just.spe=TRUE)
  eqo.df = eq.outcomes(eq.li, tg=tg)
  eqo.df

}

reduce.tg = function(tg) {
  # TO DO:
  # Compute expected payoffs for
  # each action profile (unique of paste.cols of et.mat)
  # and use those for dominance.
  # Allows better elimination even if there
  # are random moves of nature
  restore.point("reduce.tg")
  rtg = as.environment(as.list(tg))
  iteration = 0
  rtg$was.reduced = TRUE
	rtg$variant = paste0(rtg$variant,"-reduced")
  rtg$tg.id = get.tg.id(rtg)

  # store original level rows
  # to later adapt know.li
  for (lev.num in seq_along(tg$lev.li)) {
  	rtg$lev.li[[lev.num]]$lev.df$.org.row = seq_len(NROW(rtg$lev.li[[lev.num]]$lev.df))

  }


  # iteratievly eliminate strictly
  # dominated moves at action levels
  # stops when no elimination took place
  # in any action level
  while(rtg$was.reduced & iteration <2000) {
    iteration = iteration +1
    rtg$was.reduced = FALSE
    lev.num = length(tg$lev.li)+1
    while (lev.num >1) {
      lev.num = lev.num-1
      if (lev.num==1 & !rtg$was.reduced & iteration>1) {
        break
      }
      lev = rtg$lev.li[[lev.num]]
      if (lev$type == "action") {
        reduce.action.level(lev,rtg, tg, iteration=iteration)
      }
    }
  }


  # renumber .info.set.moves in all action lev.df
  # and adapt know.li
  info.set.offset = 0
  move.offset = 0

  for (lev.num in seq_along(rtg$lev.li)) {
		lev = rtg$lev.li[[lev.num]]
    lev.df = lev$lev.df

    if (lev$type == "action") {
      # adapt info sets and move indices for actions
      lev.df$.info.set.move.ind = id.to.index(lev.df$.info.set.move.ind) + move.offset
      lev.df$.info.set.ind = id.to.index(lev.df$.info.set.ind) + info.set.offset

      move.offset = max(lev.df$.info.set.move.ind)
      info.set.offset = max(lev.df$.info.set.ind)

      lev.df = lev.df %>%
        arrange(.info.set.move.ind) %>%
        group_by(.info.set.ind) %>%
        mutate(.move.ind = id.to.index(.info.set.move.ind)) %>%
        ungroup()
    } else if (lev$type == "nature") {
      # remove rows from nature
      join.cols = unique(sapply(tg$lev.li[1:lev$lev.num], function(ilev) ilev$var))
      lev.df = semi_join(lev.df,rtg$oco.df, by=join.cols)
    }
    # adapt know.li
    for (i in seq_along(lev$know.li)) {
    	lev$know.li[[i]] = lev$know.li[[i]][lev.df$.org.row,,drop=FALSE]
    }
    lev$lev.df = lev.df

    rtg$lev.li[[lev.num]] = lev
  }

  # create reduced stage.df
  stage.df = tg$stage.df

  lev.num = 0
  lev.num = lev.num+1
  for (lev.num in seq_along(rtg$lev.li)) {
	  lev.df = rtg$lev.li[[lev.num]]$lev.df
		key.col = paste0(".row.", lev.num)
  	mrows = match(stage.df[[key.col]],lev.df[[key.col]])
  	stage.rows = which(!is.na(mrows))
  	lev.rows = mrows[stage.rows]
  	if (length(stage.rows)>0) {
  		cols = intersect(colnames(stage.df), colnames(lev.df))
  		stage.df[stage.rows,cols] = lev.df[lev.rows,cols,drop=FALSE]
		}

		all.stage.rows = sort(unique(c(
  		stage.rows,
  		which(is.na(stage.df[[key.col]]))
  	)))
  	stage.df = stage.df[all.stage.rows,,drop=FALSE]
  }

  rtg$stage.df = stage.df

  #l1 = rtg$lev.li[[1]]$lev.df
  #l2 = rtg$lev.li[[2]]$lev.df
  #l3 = rtg$lev.li[[3]]$lev.df


  # adapt .row.1 .row.2. etc in all lev.df
  row.inds = lapply(seq_along(rtg$lev.li), function(lev.num) {
  	unique(rtg$lev.li[[lev.num]]$lev.df[[paste0(".row.",lev.num)]])
  })
  for (i in seq_along(rtg$lev.li)) {
  	lev.df = rtg$lev.li[[i]]$lev.df
  	for (j in 1:i) {
  		lev.df[[paste0(".row.",j)]] = match(lev.df[[paste0(".row.",j)]], row.inds[[j]])
  	}
  	rtg$lev.li[[i]]$lev.df = lev.df
  }
	# adapat stage.df .row.
  for (j in seq_along(rtg$lev.li)) {
  	rtg$stage.df[[paste0(".row.",j)]] = match(rtg$stage.df[[paste0(".row.",j)]], row.inds[[j]])
  }


	compute.tg.et.oco.etc(rtg)

  # know.var groups help to compute iso.df
  # later on
  make.tg.know.var.groups(rtg)
  make.tg.ise.df(rtg)
  #make.tg.iso.df(rtg)

  # set payoff utility as standard
  set.tg.util(tg=rtg,util.funs = rtg$util.funs)
  compute.tg.subgames(rtg)
	make.tg.spi.li(rtg)

  rtg
}



reduce.action.level = function(lev,rtg,tg, iteration=1) {
  restore.point("reduce.action.level")

  lev.df = remove.cols(lev$lev.df,".dominated")
  restore.point("reduce.action.level")
  if (NROW(lev.df)==1) return()

  # 1. join with oco.df (or with last lev.df)
  join.vars = unique(sapply(tg$lev.li[1:lev$lev.num], function(lev) lev$var))

  # oco.df may have been reduced in other levels
  # so we first reduce lev.df
  if (iteration > 1) {
    lev.df = semi_join(lev.df,rtg$oco.df, by=join.vars)
  }
  if (NROW(lev.df)==0) return()

  cols = c(join.vars, ".info.set.ind", ".info.set.move.ind",".player")

  oco.df = remove.cols(rtg$oco.df, c(".info.set.ind", ".info.set.move.ind",".player"))
  # compute oco.df
  odf = left_join(oco.df,lev.df[,cols,drop=FALSE], by=join.vars)


  # compute utility of relevant player
  player = odf$.player[[1]]
  odf[[".util"]] = odf[[paste0("util_",player)]]
  for (i in setdiff(na.omit(unique(odf$.player)),player)) {
    rows = isTRUE(odf.$player == i)
    odf[[".util"]][rows] = odf[[paste0("util_",odf$.player)]][rows]
  }

  # compute SOME dominated moves in odf
  # TO DO: Find all dominated moves.
  # Needs Rcpp implementation of pairwise comparison
  odf = odf %>%
    group_by(.info.set.move.ind) %>%
    mutate(.move.u.max = max(.util),.move.u.min = min(.util)) %>%
    group_by(.info.set.ind) %>%
    mutate(.move.u.max.min = max(.move.u.min), .dominated = .move.u.max < .move.u.max.min)

  # compute corresponding dominated moves in lev.df
  ldf = odf %>% group_by(.info.set.move.ind) %>% summarize(.dominated = first(.dominated))
  ldf = left_join(lev.df,ldf, by = ".info.set.move.ind" )

  # flag if any reduction took place for outer while loop
  if (!rtg$was.reduced) {
    rtg$was.reduced = any(ldf$.dominated)
  }

  rtg$oco.df = filter(odf, !.dominated)
  rtg$lev.li[[lev$lev.num]]$lev.df = filter(ldf, !.dominated)

}
