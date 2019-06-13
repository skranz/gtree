game_print = function(game,...) {
  print(game)
  invisible(game)
}

game_print_eq_tables = function(game, ...) {
  print.gtree_game(game=game, show.stages=FALSE, show.size=FALSE, show.eq=TRUE)
  invisible(game)
}
game_print_size_info = function(game, add.sg=TRUE, verbose=FALSE) {
  game_compile(game,add.sg = add.sg, verbose=verbose)
  print.gtree_game(game=game, show.stages=FALSE, show.size=TRUE, show.eq=FALSE)
  invisible(game)
}

print.gtree_game = function(game,..., show.stages = TRUE, show.size = TRUE, show.eq = TRUE) {
  restore.point("print.gtree_game")

  if (show.stages) {
    print.gtree_vg(game$vg)
  } else {
    cat(paste0("\n", game$gameId, if (!is.empty(game$vg$variant)) paste0("_",game$vg$variant)))
  }
  if (!is.null(game$tg) & show.size) {
    cat("\n\nSize Information:")
    print.gtree_tg(game$tg, show.title=FALSE)
  }
  if (!is.null(game$pref)) {
    cat(paste0("\nPreferences: ", paste0(game$pref$label), collapse=", "))
  }
  if (!is.null(game$eq.li) & show.eq) {
    cat(paste0("\n\n", length(game$eq.li), " Equilibria:\n"))
    #cat(knitr::kable(eq.li.tables(game$eq.li,tg = game$tg,combine = 2)))
    tabs = eq_tables(game)
    for (var in names(tabs)) {
      #cat(paste0("\n",var,":\n"))
      cat("\n")
      df = as.data.frame(tabs[[var]])
      if (has.col(df,".prob"))
        df$.prob = round(df$.prob,3)
      print(df,row.names = FALSE)
    }


  }

}



print.gtree_tg = function(tg,..., show.title=TRUE) {
  no.oco = format(NROW(tg$oco.df), big.mark=" ")
	no.ise = format(NROW(tg$ise.df), big.mark=" ")
	num.moves = format(sum(tg$ise.df$.num.moves), big.mark=" ")
	num.all.sp = format(prod(tg$ise.df$.num.moves), big.mark=" ")
	avg.moves = format(round(mean(tg$ise.df$.num.moves),1))

	num.strat = sapply(1:tg$numPlayers, function(i) {
	  prod(tg$ise.df$.num.moves[tg$ise.df$.player==i])
	})

	num.ise = sapply(1:tg$numPlayers, function(i) {
	  n_distinct(tg$ise.df$.info.set[tg$ise.df$.player==i])
	})

  if (show.title) {
    cat(paste0("\nTableform game: ", tg$tg.id, " (ca. ", format(object.size(as.list(tg))-object.size(tg$stage.df), units="auto"),")\n"))
  }
  cat(paste0("\n  - ",no.oco, " possible outcomes"))
  cat(paste0("\n  - ", no.ise, " information sets (", paste0(num.ise,collapse=" + "),")"))
  cat(paste0("\n  - " ,num.all.sp, " pure strategy profiles (",paste0(num.strat,collapse=" * "),")"))

  if (is.null(tg$sg.df)) {
    cat(paste0("\n\n  -- Subgames not yet computed ---"))
  } else {
		no.sg  = format(NROW(tg$sg.df), big.mark=" ")
		no.all.sp = format(tg$sg.df$.num.strats[1],big.mark = " ", scientific = 9)
		no.sp = format(sum(tg$sg.df$.num.strats.without.desc), big.mark=" ",scientific = 9)

		cat(paste0("\n  - ",no.sg, " subgames"))
		cat(paste0("\n  - ",no.sp, " relevant pure strategy profiles in subgames"))
  }
  cat("\n")
}

print.gtree_vg = function(vg) {
  cat(paste0("\n", vg$gameId, if (!is.empty(vg$variant)) paste0("_",vg$variant)))

  cat(paste0("\n\nParameters: ", paste0(names(vg$params),"=",vg$params, collapse=", ")))

  for (stage in vg$stages) {
    cat("\n")
    print.gtree_stage(stage)
  }
  cat("\n")

}

print.gtree_stage = function(stage) {
  #cat(paste0("\nStage: ", stage$name))
  cat(paste0("\n", stage$name))

  if (!is.empty(stage$player))
    cat(paste0("\n  Player: ", form2string(stage$player)))

  if (!is.empty(stage$condition))
    cat(paste0("\n  Condition: ", form2string(stage$condition)))

  if (!is.empty(stage$observe))
    cat(paste0("\n  Observe: ", form2string(stage$observe)))

  for (x in stage$compute)
    cat(paste0("\n  Compute ", x$name, if (!is.null(x$tables)) " specified by tables." else paste0(" = ", form2string(x$formula))))
  for (x in stage$nature) {
    if (!is.null(x$table)) {
      set = unique(x$table[[x$name]])
      cat(paste0("\n  Nature ", x$name, " \U2208 ", form2string(set,"{","}")," (table omitted) "))
    } else if (!is.null(x$fixed)) {
      cat(paste0("\n  Nature ", x$name, " \U2208 ", form2string(x$set,"{","}"), " (fixed to ", form2string(x$fixed),")"))
    } else if (is.null(x$probs)) {
      cat(paste0("\n  Nature ", x$name, " \U2208 ", form2string(x$set,"{","}"), " (uniform)"))
    } else {
      cat(paste0("\n  Nature ", x$name, " \U2208 ", form2string(x$set,"{","}")), ", Prob = ",form2string(x$probs,"(",")"))
    }
  }
  for (x in stage$actions)
    cat(paste0("\n  Action ", x$name, " \U2208 ", form2string(x$set,"{","}")))


}

form2string = function(x, set.start="", set.end="") {
  if (is.call(x)) {
    return(paste0(trimws(capture.output(print(x))), collapse=""))
  }
  paste0(set.start,paste0(as.character(x), collapse=", "),set.end)
}

