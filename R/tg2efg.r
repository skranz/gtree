# Convert a table form game into a gambit extensive form game


examples.vg.to.tg = function() {
  setwd("D:/libraries/gtree/myproject")
  gameId = "DelegationGiftExchange"
  tg = get.tg(gameId = gameId)
  efg = tg.to.efg(tg)
  compute.info.set.move.ind.gambit.order(tg=tg)

  oco.df = tg$oco.df
  lev.li = tg$lev.li
}

tg.efg.file.name = function(tg) {
	paste0(tg$tg.id,".efg")
}

get.tg.id = function(tg) {
	restore.point("get.tg.id")

	util.funs.name = get.util.funs.name(tg$util.funs)
	base.lab = paste0(tg$gameId,"_", tg$variant)
	if (isTRUE(tg$is.first.best))
	  base.lab = paste0(tg$gameId,"_FirstBest_", tg$variant)
	if (is.null(util.funs.name)) return(base.lab)
	paste0(base.lab,"__",paste0(util.funs.name,collapse="__"))
}

get.util.funs.name = function(util.funs) {
	restore.point("get.util.funs.name")
	if (is.null(util.funs)) return(NULL)

	names = names(util.funs)
	if (is.null(names)) {
		names = sapply(util.funs, function(util.fun) {
			if (is.character(util.fun)) return(util.fun)
			deparse1(util.fun)
		})
	}

	if (length(unique(names))==1) return(unique(names))
	if (all(str.starts.with(names,"payoff_"))) return("payoff")
	return(paste0(names, collapse="_"))
}

set.tg.pref = function(tg, pref) {
  utils = pref$utils
  names(utils) = rep(pref$label, length(utils))
  set.tg.util(tg, utils)
}

set.tg.util = function(tg,util.funs=payoffUtil(1:tg$params$numPlayers), symmetric=length(util.funs)==1) {
  restore.point("set.tg.util")

	tg$util.funs = util.funs
	tg$util.param = attr(util.funs, "util.param")
  for (i in tg$players) {
    util.fun = util.funs[[min(i, length(util.funs))]]
    if (is.character(util.fun)) util.fun = parse(text=util.fun)
    col = paste0("util_",i)
    tg$oco.df[[col]] = eval.on.df(util.fun, tg$oco.df)
  }
	tg$tg.id = get.tg.id(tg)

  invisible(tg)

}


tg.to.efg = function(tg, path=get.efg.dir(gameId=tg$gameId), file = paste0(tg$tg.id,".efg"), file.with.dir=NULL, verbose=TRUE, util.funs=NULL) {
  restore.point("tg2efg")

  oco.df = tg$oco.df
  if (is.null(util.funs) & is.null(tg[["util.funs"]])) {
  	util.funs = payoffUtil(1:tg$params$numPlayers)
  }
  if (!is.null(util.funs)) {
  	set.tg.util(tg,util.funs)
  }


  # make txt for all terminal nodes
  u.mat = select(oco.df, starts_with("util_"))
  #oco.df = cbind(oco.df, u.mat)
  oco.txt = util.df.to.gambit.txt(u.mat)

  # text vector that will be appended to (some) output nodes
  # describing the action and nature nodes before them
  # in the gambit tree format
  pre.txt = rep("", length(oco.txt))

  nature.info.set.start = 1+abs(min(tg$et.mat))
  counter = 0
  lev.vars = NULL
  lev.li = tg$lev.li
  while(counter< length(lev.li)) {
    counter = counter+1
    lev = lev.li[[counter]]
    lev.vars = unique(c(lev.vars, lev$var))
    lev.df = lev$lev.df
    if (lev$type=="action") {
      ltxt = action.level.to.gambit.txt(lev, oco.df)
    } else if (lev$type=="nature") {
      ltxt = nature.level.to.gambit.txt(lev,oco.df,info.set.start = nature.info.set.start)
      nature.info.set.start = nature.info.set.start + length(ltxt)
    }


    # Match outcome rows for each node at the current lev.df
    # Then add each ltext row before the smallest outcome row
    if (lev$lev.num==1) {
      text.row=1

    } else {
      df = filter(lev.df, .move.ind==1)
      df = s_select(df,".node.ind", lev.vars)


      mdf = left_join(df, s_select(oco.df,".outcome",lev.vars),by=lev.vars)
      sdf = summarise(group_by(mdf,.node.ind), text.row = min(.outcome) )
      text.row = sdf$text.row
    }

    pre.txt[text.row] = paste0(pre.txt[text.row],ltxt,"\n")
  }

  body.txt = paste0(pre.txt, oco.txt)

  player.names = paste0("pl",1:tg$n)
  header.txt = paste0('EFG 2 R "',tg$gameId,'" { ',paste0('"', player.names, '"', collapse=" "),' }')

  txt = c(header.txt, body.txt)



  if (!is.null(file.with.dir)) {
    writeLines(txt, file.with.dir)
    if (verbose)
      display("\nWritten to ", file.with.dir)
    return(invisible(file.with.dir))
  } else if (!is.null(file) & !is.null(path)) {
    if (!dir.exists(path))
      dir.create(path, recursive = TRUE)
    writeLines(txt, paste0(path,"/",file))
    if (verbose)
      display("\nWritten to ", path,"/", file )
    return(invisible(file.path(path, file)))
  } else {
    return(txt)
  }

}

util.df.to.gambit.txt = function(util.df=NULL) {


  #if (is.null(util.df))
  #  util.df = select(oco.df,starts_with("util_"))

  payoff.str = paste.matrix.cols(as.matrix(util.df),sep = ", ")
# t "" 1 "Outcome 1" { 10.000000 2.000000 }
# t "" 2 "Outcome 2" { 0.000000 10.000000 }
  txt = paste0('t ',

    # a text string, giving the name of the node
    '"" ',
    # a nonnegative integer specifying the outcome
    1:NROW(util.df),
    # (optional) the name of the outcome
    ' "" ',
    # the payoffs to each player for the outcome
    '{ ', payoff.str,' }'
  )
  txt
}


action.level.to.gambit.txt = function(lev, oco.df) {
  restore.point("action.level.to.gambit.txt")

  df = lev$lev.df
  df$.node = paste0(lev$var,"_", df$.node.ind)
  com = paste0('paste0("\\"',lev$var,'_",',lev$var,',"\\"", collapse=" ")')
  df = summarise_(group_by(df,.node.ind,.node,.info.set, .info.set.ind, .player),moves_str = com) %>% ungroup()

# p "" 1 1 "(1,1)" { "H" "L" } 0
# p "" 2 1 "(2,1)" { "h" "l" } 0
  txt = paste0('p "',
#     a text string, giving the name of the node
#    df$.node,'" ',
    "",'" ',
#     a positive integer specifying the player who owns the node
    df$.player,' ',
#     a positive integer specifying the information set
    df$.info.set.ind,' ',
#     (optional) the name of the information set
#    '"',df$.info.set,'" ',
    '"" ',
#     (optional) a list of action names for the information set
    '{ ', df$moves_str," }",
#     a nonnegative integer specifying the outcome
    " 0"
  )
  txt
}


nature.level.to.gambit.txt = function(lev, oco.df, info.set.start = 1) {
  restore.point("nature.level.to.gambit.txt")

  df = lev$lev.df
  df$.node = paste0(lev$var,"_", df$.node.ind,"_n")

# c "" 2 "(0,2)" { "2g" 0.500000 "2b" 0.500000 } 0

  com = paste0('paste0("\\"',lev$var,'_",',lev$var,',"\\" ",.move.prob, collapse=" ")')
  df = summarise_(group_by(df,.node.ind,.node),moves_str = com) %>% ungroup
  txt = paste0('c "',
#     a text string, giving the name of the node
#    df$.node,'" ',
    "",'" ',
#     a positive integer specifying the information set number
    1:NROW(df)+info.set.start-1,' ',
#     (optional) the name of the information set
#    '"',df$.node,'" ',
    '"" ',
#     (optional) a list of actions at the information set with their corresponding probabilities
    '{ ', df$moves_str," }",
#     a nonnegative integer specifying the outcome
    " 0"
  )
  txt
}


