# Find first best solution
#
# Steps:
#   1. Change the vg such that the player is 1 in all stages
#   2. Solve the single player game maximizing the sum of both
#      players utilities

example.first.best = function() {
  setwd("D:/libraries/gtree/myproject")
	gameId = "PG"
	vg = get.vg(gameId=gameId)
	tg.fb = vg.to.first.best.tg(vg)
  ise.df = tg.fb$ise.df
	oco.df = tg.fb$oco.df
	eq.li = compute.first.best(tg.fb, find.all.eq = TRUE)

  out = expected.eq.outcomes(eq.li=eq.li, tg=tg.fb)
  out
}

#' Given a tg that has been created with vg.to.first.best
#' compute one or all first best strategy profile(s)
compute.first.best = function(tg.fb, find.all.eq=FALSE) {
  restore.point("compute.first.best")
  tg.fb = set.tg.welfare(tg.fb)
  compute.single.player.eq(tg.fb, player=1, find.all.eq=find.all.eq,util.col = ".welfare")
}

#' Transform a vg to a tg that can be used to quickly compute a first best
#' strategy profile.
#'
#' Essentially the player in each stage will be set to player 1 and then
#' a tg is created. This means player 1 chooses all actions and she
#' knows what all player know.
vg.to.first.best.tg = function(vg,..., tg.id=paste0(vg$gameId,"_FirstBest_", vg$variant), add.sg=FALSE, add.spi=FALSE, add.spo=FALSE) {
  restore.point("vg.to.first.best.tg")

  vg = as.environment(as.list(vg))
  # Set all players to player 1
  for (s in seq_along(vg$stages)) {
    if (!is.empty(vg$stages[[s]]$player))
      vg$stages[[s]]$player = 1
  }

  #stage = vg$stages[[2]]
  # Transform to tg
  tg = vg.to.tg(vg,..., add.sg=add.sg, add.spi=add.spi, add.spo=add.spo)
  tg$tg.id = tg.id
  tg = set.tg.welfare(tg)
  tg$is.first.best.tg = TRUE
  class(tg) = c("gtree_fb_tg","gtree_tg","environment")
  tg
}

set.tg.welfare = function(tg) {
  code = paste0("util_", 1:tg$numPlayers, collapse="+")
  call = parse(text=code)
  tg$oco.df[[".welfare"]] = eval(call, tg$oco.df)
  tg
}

get.first.best.tg = function(variant=first.non.null(vg$variant,1), gameId = first.non.null(vg$gameId,jg$gameId,rg$gameId), jg.hash = get.jg.hash(jg=jg, rg=rg,vg=vg),jg=NULL,rg=NULL, vg=NULL, tg=NULL, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), save.new = TRUE,branching.limit = 10000,msg.fun=NULL, never.load = FALSE, filename=NULL, tg.id=NULL) {
	restore.point("get.first.best.tg")

	if (is.null(jg.hash)) {
		jg = get.jg(gameId = gameId, games.dir = games.dir)
		jg.hash = get.jg.hash(jg=jg)
	}
	if (is.numeric(variant)) {
		rg = get.rg(gameId=gameId, jg=jg, rg=rg, jg.hash=jg.hash, games.dir=games.dir)
		variant = rg$variants[[variant]]
	}

  if (is.null(tg.id)) {
    tg.id=paste0(gameId,"_FirstBest_", variant)
  }
	if (is.null(filename))
	  filename = paste0(tg.id, ".tg")

	file = file.path(games.dir,gameId, filename)
	if (file.exists(file) & !never.load) {
		# return old vg if jg.hash has not changed
		tg = readRDS(file)
		if (identical(tg$jg.hash, jg.hash) | is.null(jg.hash))
			return(tg)
	}

	vg = get.vg(variant=variant, gameId=gameId, jg=jg, rg=rg, vg=vg, jg.hash=jg.hash, games.dir=games.dir)

	tg = vg.to.first.best.tg(vg,branching.limit = branching.limit, msg.fun=msg.fun)
	if (tg$kel$count>0) {
		return(tg)
	}
	if (save.new) {
		if (!is.null(msg.fun)) msg.fun("Save first best decision tree in ",file,"...")
		saveRDS(tg, file)
		if (!is.null(msg.fun)) {
			msg.fun("First best decision tree saved as ",file, ".\n Size = ", file.size(file))
		}
	}
	tg
}
