# Recompute tg if only parameters have changed that affect payoffs
# or probabilities. Should be faster than computing completely new tg.

examples.tg_recompute = function() {
  library(gtree)

  game = new_game(
    gameId = "UG_RandomOffer",
    options = make_game_options(verbose=TRUE),
    params = list(numPlayers=2,natureProb = 0.9, highOfferProb = 0.01),
    stages = list(
      stage("who_offers",
        nature = list(
          natureMove("offerType",c("nature","player"), probs = ~c(natureProb, 1-natureProb))
        )
      ),
      stage("proposerStage",
        player=1,
        condition = ~ offerType == "player",
        actions = list(
          action("offerPlayer",0:1)
        )
      ),
      stage("randomOffer",
        condition = ~ offerType == "nature",
        nature = list(
          natureMove("offerNature",0:1,~c(1-highOfferProb, highOfferProb))
        )
      ),
      stage("computeOffer",
        compute = list(
          offer ~ ifelse(offerType=="nature", offerNature, offerPlayer)
        )
      ),
      stage("responderStage",
        player=2,
        observe = "offer",
        actions = list(
          action("accept",c(FALSE,TRUE))
        )
      ),
      stage("PayoffStage",
        player=1:2,
        compute=list(
          payoff_1 ~ ifelse(accept, 2-offer,0),
          payoff_2 ~ ifelse(accept, offer,0)
        )
      )
    )
  )
  game %>%
    game_solve_spe() %>%
    game.eq.outcomes() -> res

  game %>%
    game_change_param(natureProb = 0.99,highOfferProb = 0.99) %>%
    game_solve_spe(mixed=TRUE, efg.dir=getwd()) %>%
    game.expected.eq.outcomes() -> eqo.df




  game %>%
    game_solve_spe() %>%
    game.eq.tables()

  game %>%
    game_gambit_solve(efg.dir = "D:/libraries/gtree") %>%
    game.eq.tables()

  game.eq.li(game)
  game.eq.outcomes(game)

  game %>% game.eq.li() %>% .[[1]]
  game.outcomes(game)


  game %>% game.eq.tables()

  # lev.df of random offer stage and acceptStage
  # Note that rows that don't satisfy cond are missing
  # from lev.df3 and indicated by NA in .row.3 in lev.df4
  lev.df3 = game$tg$lev.li[[3]]$lev.df
  lev.df4 = game$tg$lev.li[[4]]$lev.df

  which(lev.df4$.row.3 == 1)

  tg = game$tg
  old.stage.df = tg$stage.df

  tg$params$reject_cost = 5
  recompute.tg.transformations(tg)
  identical(tg$stage.df, old.stage.df)
  sapply(colnames(tg$stage.df), function(col) {
    identical(old.stage.df[[col]], tg$stage.df[[col]])
  })
  old.stage.df$offer_times_reject_cost
  tg$stage.df$offer_times_reject_cost
  tg$oco.df$offer_times_reject_cost

  # probs and payoff
  game$tg$oco.df
  game$tg$stage.df
  game$tg$lev.li[[2]]$lev.df
  game$tg$lev.li[[3]]$lev.df


  # probs no payoff
  game$tg$spo.li # probs are just mapped from oco.df


  # No probs no payoffs
  game$tg$ise.df
  game$tg$sg.df
  game$tg$sgi.df
  game$tg$spi.li
  game$tg$know.li
  game$tg$lev.li[[3]]$know.li



  tg = game$tg
}

# Recompute probablilities in a tg after a change of parameters
# that only affects the probabilities of moves of nature
recompute.tg.probs = function(tg,vg, params = tg$params, changed.par = names(params)) {
  restore.point("recompute.tg.probs")
  tg$params = params
  new.probs = FALSE

  lev.num = 0
  while(lev.num < length(tg$lev.li)) {
    lev.num = lev.num+1
    lev = tg$lev.li[[lev.num]]
    if (!lev$type == "nature") next

    stage = vg$stages[[lev$stage.num]]
    probs = stage$nature[[lev$var]]$probs
    if (!is.call(probs)) next

    vars = find.variables(probs)
    if (length(intersect(vars, changed.par))==0) next

    lev.df = lev$lev.df
    call.list = list.call.to.call.list(probs)
    if (is.list(call.list)) {
      for (move.ind in seq_along(call.list)) {
        rows = which(lev.df$.move.ind == move.ind)
        lev.df$.move.prob[rows] = eval.on.df(call.list[[move.ind]], lev.df[rows,, drop=FALSE], tg$params)
      }

      lev.df$.prob = lev.df$.prev.prob * lev.df$.move.prob
      tg$lev.li[[lev.num]]$lev.df = lev.df
    } else {
      stop("Sorry, can so far only change probabilities given by a formula if the probabilities are specified as a vector starting with c(). Need to recompile complete game.")
    }
    # Now we need to propagete the changed probabilities forward

    for.lev.num = lev.num
    while (for.lev.num < length(tg$lev.li)) {
      for.lev.num = for.lev.num + 1

      mprob = get.lev.df.col.from.prev.levs(tg,".prob", for.lev.num, start.lev.num = lev.num)
      mrows = !is.na(mprob)

      for.lev = tg$lev.li[[for.lev.num]]
      for.lev.df = for.lev$lev.df
      if (for.lev$type == "nature") {
        for.lev.df[[".prev.prob"]][mrows] = mprob[mrows]
        for.lev.df[[".prob"]][mrows] = mprob[mrows] * for.lev.df[[".move.prob"]][mrows]
      } else {
        for.lev.df[[".prob"]][mrows] = mprob[mrows]
      }
      tg$lev.li[[for.lev.num]]$lev.df = for.lev.df
    }
    # Note that stage df has a different row order than lev.df[[4]]
    # and may have different number of rows
    mprob = get.lev.df.col.from.prev.levs(tg,".prob",length(tg$lev.li)+1, start.lev.num = lev.num)
    tg$stage.df$.prob =mprob
    # oco.df has same ordering as stage.df
    tg$oco.df$.prob = tg$stage.df$.prob
  }
  compute.tg.et.mat(tg)
  invisible(tg)
}

# Get a column of a lev.df from previous lev.df
# we may need to search backwards multiple levels
# to find missing rows for which stage$cond evaluated to FALSE
get.lev.df.col.from.prev.levs = function(tg, col, lev.num, start.lev.num) {
  restore.point("get.lev.df.col.from.prev.levs")
  if (lev.num <= length(tg$lev.li)) {
    lev.df = tg$lev.li[[lev.num]]$lev.df
  } else {
    lev.df = tg$stage.df
  }
  matched = rep(FALSE, NROW(lev.df))
  vals = rep(NA, NROW(lev.df))
  prev.lev.num = lev.num
  while(prev.lev.num > start.lev.num) {
    prev.lev.num = prev.lev.num-1
    prev.lev.df = tg$lev.li[[prev.lev.num]]$lev.df
    prev.row = lev.df[[paste0(".row.", prev.lev.num)]]
    do.match = !matched & !is.na(prev.row)
    vals[do.match] = prev.lev.df[[col]][prev.row[do.match]]
    matched = matched | do.match
    if (sum(!matched)==0) break
  }
  vals
}

# Recompute transformation in a tg after a change of parameters
# that affects computed transformations without changing
# the structure of the game
recompute.tg.transformations = function(tg, params = tg$params, changed.par = names(params)) {
  tg$params = params
  for (trans in tg$transformations) {
    tg = recompute.tg.transformation(tg, trans)
  }
}

recompute.tg.transformation = function(tg, trans) {
  restore.point("recompute.tg.transformation")
  lev.num = org.lev.num = trans$lev.num

  if (lev.num > length(df$lev.li)) {
    df = tg$stage.df
    mod.df = recompute.transformation.on.df(df, trans, tg$params)
    #identical(df, new.df)
    tg$stage.df = mod.df
  } else {
    df = tg$lev.li[[lev.num]]$lev.df
    mod.df = recompute.transformation.on.df(df, trans, tg$params)
    tg$lev.li[[lev.num]]$lev.df = mod.df
  }

  prev.df = mod.df

  # Now we map the new values to later levels
  while (lev.num < length(tg$lev.li)) {
    lev.num = lev.num + 1
    df = tg$lev.li[[lev.num]]$lev.df
    key.col = paste0(".row.", org.lev.num)

    prev.rows = match(df[[key.col]], prev.df[[key.col]])
    df[[trans$var]][!is.na(prev.rows)] = prev.df[[trans$var]][prev.rows[!is.na(prev.rows)]]

    tg$lev.li[[lev.num]]$lev.df = df
    prev.df = df

  }
  if (lev.num <= length(tg$lev.li)+1) {
    key.col = paste0(".row.", org.lev.num)
    prev.rows = match(tg$stage.df[[key.col]], prev.df[[key.col]])
    tg$stage.df[[trans$var]][!is.na(prev.rows)] = prev.df[[trans$var]][prev.rows[!is.na(prev.rows)]]
  }
  tg$oco.df[[trans$var]] = tg$stage.df[[trans$var]]
  tg
}

recompute.transformation.on.df = function(df, tr, params) {
  restore.point("recompute.transformation.on.df")
  if (is.null(tr$cond)) {
    if (is.null(tr$tables)) {
      df[[tr$var]] = eval.on.df(call = tr$formula,df, params=params)
    } else {
      df = eval.key.tables.to.df(df, tr$tables, var=tr$var)
    }
  # Transformation applies only to a subset of rows
  } else {
    rows = eval.on.df(tr$cond, df, params=params)
    if (!tr$var %in% colnames(df))
      df[[tr$var]] = NA
    if (is.null(tr$tables)) {
      df[rows,tr$var] = eval.on.df(tr$cond, df[rows,,drop=FALSE], params=params)
    } else {
      df = eval.key.tables.to.df(df, tr$tables, var=tr$var, rows=rows)
    }
  }
  df
}


