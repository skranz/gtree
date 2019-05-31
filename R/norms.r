# Ability to compute norm equilibria as described in
#
# Moral norms in a partly compliant society, Sebastian Kranz,
# 2010, Games and Economic Behavior
example.norms = function() {
  setwd("D:/libraries/gtree/myproject")
	gameId = "UG2"
	vg.org = get.vg(gameId=gameId)
	tg = vg.to.complier.tg(vg.org, complierProb=0.90)
  oco.df = tg$oco.df

	norms = list(
	  norm.rule(1,"offer",round(cake/2)),
	  norm.rule(2,"accept",ifelse(offer>=round(0.5*cake),1,0))
	)

	tg.norm = fix.tg.actions(tg,fix.li=norms, tremble.prob = 1/10000)

	eq.li = get.eq(tg = tg.norm,never.load = TRUE)
	eo = eq.outcomes(eq.li=eq.li, tg=tg.norm) %>% filter(.prob > 0.001)
	eo = eo %>% filter(!isComplier1 & !isComplier2)
  eo %>% filter(eqo.ind==1)

}

norm.rule = function(player,var, formula, condition=NULL) {
  formula=substitute(formula)
  base.cond = parse.as.call(paste0(".player_",player," & isComplier",player))
  if (!is.null(condition)) {
    condition = substitute(condition)
    condition = substitute((a) & (b), list(a=base.cond, b=condition))
  } else {
    condition = base.cond
  }
  list(var=var, formula=formula, condition=condition)
}

vg.to.complier.tg = function(vg, complierProb, tg.id=paste0(vg$gameId,"_Complier_",vg$variant),...) {
  vg.comp = vg.to.complier.vg(vg, complierProb)
  tg = vg.to.tg(vg.comp,...,add.sg = FALSE,add.spi = FALSE, add.spo = FALSE)
  tg$tg.id = tg.id
  tg
}

vg.to.complier.vg = function(vg, complierProb = 0.5) {
  restore.point("vg.to.complier.vg")
  vg$params[["complierProb"]] = complierProb

  # Create for each player a complier stage
  complier.stages = lapply(1:vg$params$numPlayers, function(player) {
    stage.var = paste0("isComplier",player)
    nature = list(list(
      name = stage.var,
      set = c(FALSE,TRUE),
      probs = quote(c(1-complierProb,complierProb))
    ))

    stage = list(
      name = "drawComplierTypes",
      player = player,
      condition = "",
      observe = stage.var,
      nature = nature,
      compute = list(),
      actions = list(),
      waitForPlayers = "",
      ai.li = list(),
      stage.vars = list(stage.var),
      need.vars = stage.var
    )
    stage
  })
  vg$stages = c(complier.stages, vg$stages)
  vg
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
