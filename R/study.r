game_study = function(game, param.grid=NULL, pref.list=NULL, verbose=game$options$verbose,...) {
  game_define_study(game, param.grid, pref.list)
  game_run_study(game, verbose=verbose)
  invisible(game)
}

game_define_study = function(game, param.grid=NULL, pref.list=NULL,...) {
  restore.point("game_define_study")

  # If no preference list is specified
  # use current game preferences or if not specified pref_payoff
  if (length(pref.list)==0) {
    if (is.null(game$pref)) {
      game_set_preferences(game, pref_payoff(game$players))
    }
    pref.list = list(game$pref)
  }

  if (is.null(names(pref.list))) {
    names(pref.list) = sapply(pref.list, function(pref) pref$label)
  }
  if (!has.col(param.grid,"pref")) {
    study.grid = cbind(
      quick.df(pref = rep(names(pref.list), each=NROW(param.grid))),
      param.grid
    )
  }

  if (length(param.grid)>0) {
    game$vars.info = compute.vg.vars.info(game$vg)
    par.info = game$vars.info %>% filter(is.param)

    study.par = colnames(param.grid)

    # Parameters for which a complete new tg must be computed
    struc.par = par.info$var[par.info$affects.set | par.info$affects.structure] %>% intersect(study.par)

    # Parameters for which at least probabilities must be newly computed
    prob.par = par.info$var[par.info$affects.prob] %>% intersect(study.par) %>% setdiff(struc.par)

    # Parameters that change some transformation
    transform.par = par.info$var[par.info$affects.compute] %>% intersect(study.par) %>% setdiff(struc.par)

    # Parameters probably relevant for preferences
    pref.par = c("pref",setdiff(study.par, names(game$vg$params)))

    # Sort study grid
    study.grid = arrange_at(study.grid, c(struc.par, prob.par, transform.par, pref.par))

  } else {
    study.par = struc.par = prob.par = transform.par = NULL
    pref.par = "pref"
  }

  game$study = list(
    study.grid = study.grid,
    pref.list = pref.list,
    study.par = study.par,
    struc.par = struc.par,
    prob.par = prob.par,
    transform.par = transform.par,
    pref.par = pref.par
  )
  invisible(game)
}

game_run_study = function(game, verbose=game$options$verbose,...) {
  restore.point("game_run_study")
  st = game$study

  study.grid = st$study.grid
  pref.list = st$pref.list

  n = NROW(study.grid)
  # Simple implementation without any
  # optimization yet
  res.li = vector("list", n)

  game.par = setdiff(st$study.par, st$pref.par)
  change.pref = length(pref.list)>1 | length(st$pref.par)>1
  if (!change.pref) {
    pref = pref.list[[1]]
    game_set_preferences(game, pref)
  } else {
    old.pref.par = NA
  }

  row = 0
  while (row < n) {
    row = row+1
    if (verbose == 1) {
      cat(paste0("\nSolve scenario ", row, " / ",n))
    } else if (verbose == 2) {
      cat(paste0("\n\n************************************\nSolve scenario ", row, " / ",n,"\n"))
    }

    # Change game parameters
    if (length(game.par)>0) {
      params = as.list(study.grid[row, game.par, drop=FALSE])
      game_change_param(game, params=params)
      if (game$needs.recompile & verbose==1) {
        cat(" (recompile)")
      }
    }
    # Change preferences
    if (change.pref) {
      pref = pref.list[[study.grid$pref[row]]]
      if (length(st$pref.par)>1) {
        pref.par = as.list(study.grid[row, st$pref.par]) %>% remove.from.list("pref")
        pref = pref_change_params(pref, params = pref.par)
      }
      game_set_preferences(game, pref)
    }
    game_solve_spe(game, verbose= (verbose>=2))
    res = eq_outcomes(game)
    add = as.list(study.grid[row,])
    add$pref = game$pref$label
    add$scenario.ind = row

    res.li[[row]] = as_tibble(cbind(res, add))
  }
  game$study.eq.outcomes = suppressWarnings(bind_rows(res.li))
  invisible(game)
}

game.study.eq.outcomes = function(game) {
  game$study.eq.outcomes
}
