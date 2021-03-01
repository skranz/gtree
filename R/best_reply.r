
example.fix.actions = function() {
  setwd("D:/libraries/gtree/myproject")
	gameId = "UltimatumGame"
	gameId = "EqTest"
	gameId = "UG2"
	tg = get.tg(gameId = gameId, never.load = !FALSE)
	eq.li = get.eq(tg = tg, never.load=TRUE)
  eq = eq.li[[1]]

  tg.br = make.best.reply.tg(tg, eq, player=2, tremble.prob = 1/100)
  tg.br = make.best.reply.tg(tg, eq, player=2, tremble.prob = 0)
  eq.li = get.eq(tg.br, never.load=TRUE)
  eo = eq.outcomes(eq.li, tg=tg.br)
  eeo = expected.eq.outcomes(eq.li=eq.li, tg=tg.br)
  eeo
}

# Add uniform tremble over all moves
add.tremble = function(move.prob, tremble.prob) {
  (1-tremble.prob)*move.prob + tremble.prob / length(move.prob)
}

make.best.reply.tg = function(tg, eq, player=1, info.set.probs=attr(eq,"info.set.probs"), omit.zero.prob = tremble.prob==0, tg.id = paste0(tg$gameId,"_", tg$variant, "_BR",player,"_eq",attr(eq,"eq.ind")), tremble.prob = 0) {
  restore.point("make.best.reply.tg")

  # Make a copy of tg
  tg = as.environment(as.list(tg))

  # Filter info sets of other players
  ise.df = tg$ise.df %>%
    filter(.player != player)



  # Create new level list
  # Change actions of other players into moves of nature
  lev.li.li = lapply(seq_along(tg$lev.li), function(lev.num) {
    restore.point("hdkfhuidfkdnf")
    rows = which(ise.df$.lev.num == lev.num)

    # No information set that must be transformed in
    # the current level
    if (length(rows)==0)
      return(list(tg$lev.li[[lev.num]]))


    lev = tg$lev.li[[lev.num]]
    lev.df = lev$lev.df

    # Get all rows of lev.df with info sets that we want to replace
    rel.lev.df = semi_join(lev.df, ise.df, by=".info.set.ind")

    fix.df = as_tibble(list(
      .info.set.move.ind = rel.lev.df$.info.set.move.ind,
      .var = rel.lev.df[[lev$var]],
      .move.prob=info.set.probs[rel.lev.df$.info.set.move.ind]
    ))
    colnames(fix.df)[2] = lev$var

    # Can add uniform trembles over moves
    # such that later information sets
    # will be reached with positive probabilities
    if (tremble.prob>0) {
      fix.df$.node.ind = rel.lev.df$.node.ind
      fix.df = fix.df %>% group_by(.node.ind) %>%
        mutate(.move.prob = add.tremble(.move.prob, tremble.prob)) %>%
        ungroup() %>%
        select(-.node.ind)
    }


    res.lev = lev.action.to.nature(lev, fix.df=fix.df, var=lev$var, omit.zero.prob = omit.zero.prob, params=tg$params)
    return(res.lev)
  })

  lev.li = do.call(c, lev.li.li)

  # Perform all necessary auxilliary computations to set the new lev.li
  # Information sets, oco.df, et.mat etc
  tg = set.new.tg.lev.li(tg, lev.li)

  # Need to set tg.id at bottom
  # since set.new.tg.lev.li will set automatic
  # tg.id
  tg$tg.id = tg.id

  tg
}
