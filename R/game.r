example.game = function() {
  setwd("D:/libraries/gtree/myproject")

  game = new_game(
    gameId = "UG",
    params = make_game_params(numPlayers=2, cake=4),
    options = make_game_options(),
    stages = list(
      stage("ProposerStage",
        player=1,
        actions = list(
          action("offer",~0:cake)
        )
      ),
      stage("ResponderStage",
        player=2,
        observe="offer",
        actions = list(
          action("accept",c(FALSE,TRUE))
        )
      ),
      stage("PayoffStage",
        player=1:2,
        observe=c("offer","accept"),
        compute=list(
          payoff_1 ~ (cake-offer)*accept,
          payoff_2 ~ offer*accept
        )
      )
    )
  )
  game_solve_spe(game)
  game

  game_add_types(game, typeNames = c("TypeA", "TypeB"))
  game

  game.eq.li(game)
  game.eq.outcomes(game)
  game.eq.li.tables(game, combine=TRUE)


  tg = vg.to.tg(vg)
  tg
  eq.li = gtree.solve.spe(tg=tg)
  expected.eq.outcomes(eq.li=eq.li, tg=tg)
  eq.tables(eq.li[[1]], tg=tg)


  vg$stages
  setwd("D:/libraries/gtree/myproject")

  game = new_game(
    gameId = "RandomCostCournot",
    params = game_params(a=100, qMax=40,qMin=10,
      c2=0, c1Low=0, c1High=10),
    stages = list(
      stage("drawCostStage",
        nature = list(
          natureMove("c1",~c(c1Low,c1High))
        )
      ),
      stage("q1Stage",
        player=1,
        observe="c1",
        actions = list(
          action("q1",~qMin:qMax)
        )
      ),
      stage("q2Stage",
        player=2,
        #observe="c1",
        actions = list(
          action("q2",~qMin:qMax)
        )
      ),
      stage("PayoffStage",
        player=1:2,
        compute=list(
          Q ~ q1+q2,
          P ~ a-Q,
          payoff_1 ~ (P-c1)*q1,
          payoff_2 ~ (P-c2)*q2
        )
      )
    )
  )


  tg = vg.to.tg(vg,add.sg = TRUE)
  tg
  #View(memory.list(tg))

  options(gtree.spo.chunk.size = 10000)
  compute.tg.fields.for.internal.solver(tg)

  # Internal solver
  eq.li = gtree.solve.spe(tg=tg)

  # Gambit
  eq.li = gambit.solve.eq(tg=tg, save.eq=FALSE)


  eqo.li = expected.eq.outcomes(eq.li=eq.li, tg=tg)
  eqo.li

  eq.tables(eq.li[[1]], tg=tg)
  eq.table.rules(eq.li[[1]], tg=tg)



  # Test make.sg.spo
  vg = new.vg(
    gameId = "TestStackelberg",
    params = list(numPlayers=2, a=100, qMax=100),
    stages = list(
      stage("q1Stage",
        player=1,
        actions = list(
          action("q1",~0:qMax)
        )
      ),
      stage("q2Stage",
        player=2,
        actions = list(
          action("q2",~0:qMax)
        )
      ),
      stage("PayoffStage",
        player=1:2,
        compute=list(
          Q ~ q1+q2,
          P ~ a-Q,
          payoff_1 ~ (P)*q1,
          payoff_2 ~ (P)*q2
        )
      )
    )
  )
  tg = vg.to.tg(vg,add.sg = TRUE, verbose=FALSE)
  tg

  options(gtree.spo.chunk.size = 10000)
  compute.tg.fields.for.internal.solver(tg)

  eq.li = gtree.solve.spe(tg=tg)
  eq.tables(eq.li[[1]], tg=tg)


  eq.li = gambit.solve.eq(tg=tg, save.eq=FALSE)

  gambit.solve.eq(tg)
}

#' Create a new game in stage form
new_game = function(gameId, params=game_params(), options=make_game_options(), stages, variant="") {
  restore.point("new.game")
  vg = as.environment(list(
    gameId = gameId,
    variant = variant,
    params = params,
    stages = stages
  ))
  class(vg) = c("gtree_vg","environment")

  game = as.environment(list(
    gameId = gameId,
    players = 1:vg$params$numPlayers,
    vg = vg,
    options = options,
    needs.recompile = TRUE
  ))
  class(game) = c("gtree_game","environment")
  game
}


game_compile = function(game,branching.limit = 10000, for.internal.solver=FALSE, add.sg=for.internal.solver, add.spi=for.internal.solver, add.spo=for.internal.solver, force=FALSE, verbose=game$options$verbose,...) {
  # Create all required addition
  restore.point("game_compile")

  compile = is.null(game[["tg"]]) | isTRUE(game$needs.recompile) | force
  if (compile) {
    game$tg = vg.to.tg(game$vg,branching.limit = branching.limit, add.sg=add.sg, add.spi=add.spi, add.spo=add.spo,verbose = verbose)
    game$needs.recompile = FALSE
  }

  if (!is.null(game[["pref"]])) {
    set.tg.pref(game$tg, game$pref)
  } else {
    set.tg.util(game$tg)
  }

  invisible(game)
}


game_solve_spe = function(game, verbose=isTRUE(game$options$verbose>=1),...) {
  restore.point("game_solve_spe")
  game_compile(game,verbose=verbose)

  compute.tg.fields.for.internal.solver(game$tg, verbose=verbose)

  eq.li = gtree.solve.spe(tg = game$tg, verbose=verbose)
  game$eq.li = eq.li
  game$eqo.df = eq.li.outcomes(eq.li = eq.li, tg=game$tg)
  game$eeqo.df = eq.li.expected.outcomes(eq.li = eq.li, tg=game$tg)
  invisible(game)
}

#' Set players preferences
#'
#' This function sets players preferences to a parametrized preference
#' type. To specify completely custom preferences use game_set_util_fun
#' instead.
#'
#' @param game The game object
#' @param type The preference type "payoff", "ineqAv","envy","lossAv", "unifLossAv"
#' @param ... Parameters of that preference type
#' @param players which players shall have this preference. By default all players
game_set_preferences = function(game, pref) {
  restore.point("game_set_preferences")
  game$pref = pref
  invisible(game)
}


game_copy = function(game) {
  ngame = as.environment(as.list(game))
  class(ngame) = class(game)
  ngame$vg = as.environment(as.list(ngame$vg))
  ngame$tg = as.environment(as.list(ngame$tg))
  ngame
}

game_add_tremble = function(game, action=NULL, stage=NULL, tremble.prob = 0.0001) {
  clear.non.vg(game)
  game$vg = vg.add.tremble(game$vg, action=action, stage=stage, tremble.prob = tremble.prob)
  invisible(game)
}

#' Write game as a Gambit efg file
#'
#' @param game The game object
#' @param file The file with full path. If NULL create a default name
#' @param
game_write_efg = function(game, file=NULL, dir=getwd()) {
  game_compile(game)
  if (is.null(file)) {
    file = file.path(dir, paste0(game$tg$tg.id,".efg"))
  }
  game$efg.file =  tg.to.efg(game$tg, file.with.dir = file, verbose=!isTRUE(!game$options$verbose))
  invisible(game)
}

game.eq.tables = function(game,reduce.tables = TRUE, combine=2, eq.ind=seq_along(game$eq.li), ignore.cols = NULL, ...) {
  if (is.null(game$unknown.vars.at.actions)) {
    game$unknown.vars.at.actions = find.unknown.vars.at.actions(game$tg)
  }

  eq.li.tables(game$eq.li[eq.ind], tg = game$tg, combine=combine,reduce.tables = reduce.tables, ignore.keys = union(names(game$vg$params), ignore.cols), ignore.li = game$unknown.vars.at.actions, ...)
}

game.outcomes = function(game,..., reduce.cols=FALSE) {
  oco.df = game$tg$oco.df
  if (reduce.cols) {
    ignore.cols = c(names(game$vg$params),".prob",".outcome")
    cols = setdiff(colnames(oco.df), ignore.cols)
    oco.df = oco.df[, cols]
  }
  oco.df
}
game.eq.li = function(game,...) {
  game$eq.li
}
game.eq.outcomes = function(game,...) {
  game$eqo.df
}
game.expected.eq.outcomes = function(game,...) {
  game$eeqo.df
}




# Specify the game parameters
make_game_params = function(numPlayers=2,...) {
  list(numPlayers=numPlayers,...)
}

#' Specify the game options in new_game
make_game_options = function(verbose=TRUE,...) {
  list(verbose=verbose,...)
}

#' Change options of the game object
#'
#' See make_game_options for a description of the
#' available options.
game_set_options = function(game, ...) {
  args = list(...)
  restore.point("game_set_options")

  if (is.null(game$options)) {
    game$options = args
  } else {
    game$options[names(args)] = args
  }
  game
}


#' Specify an action in a stage
#' @param name The variable name of the action
#' @param set The set of different action values. Can be a formula that depends on other game variables.
#' @param strategyMethodDomain if not NULL the action shall be specified via strategy method in an experiment. State the variable name upon which the action conditions
#' Only used when running an experiment or analysing experimental data
action = function(name, set, strategyMethodDomain=NULL, ...) {
  if (is.null(strategyMethodDomain)) {
    list(name=name,set=f2c(set),...)
  } else {
    list(name=name,set=f2c(set),strategyMethodDomain=strategyMethodDomain,...)
  }
}

#' Specify a random move of nature in a stage
#' @param name The variable name of the variable
#' @param set The set of different values. Can be a rhs only formula.
#' @param probs The probability of each element in set. If NULL all moves are equally likely. Can be a rhs formula
natureMove = function(name, set, probs=NULL,...) {
  list(name=name,set=f2c(set),probs=f2c(probs),...)
}

#' Specify a stage for a game
#' @param name Name of the stage
#' @param player The player who acts in this stage. Can be a rhs formula. If an action is chosen in the stage, there must be a unique player. If it is a stage in which no actions take place, the player variable multiple players can be set. Each player observes the variables specified under observe.
#' @param condition A logical condition specifying whether the stage will be run. Can be a rhs formula. If it evaluates to FALSE the stage will not be shown, i.e. no observations are made and no actions are chosen. Also no computations in this stage take place.
#' @param observe A vector of variable names specifying which variables are observed by the player(s) at this stage. Is relevant to correctly specify the information sets in the extensive form game.
#' @param compute A list of formulas like 'compute=list(payoff_1 ~ x-5)'. The lhs specifies a variable name and the rhs a DETERMINISTIC formula. The variables are computed at the beginning of the stage before actions and moves of nature take place. This means they can be used e.g. in formulas for action sets of the same stage.
#' @param nature A list of moves of nature, i.e. random variables from a finite set. E.g. nature=list(natureMove("proposer",c(1,2),prob=c(0.4,0.6)).
#' @param actions A list of actions. E.g. actions=list(action("offer",~0:cake_size)
stage = function(name, player=NULL, condition=NULL, observe=NULL, compute=NULL, nature=NULL, actions=NULL,...) {
  restore.point("stage")

  player = f2c(player)
  condition = f2c(condition)
  observe=f2c(observe)
  compute = name.by.name(lapply(seq_along(compute), function(i) {
    trans = compute[[i]]
    restore.point("stage385")
    if (is(trans,"formula")) {
      if (length(trans)==3)
        return(list(name=as.character(trans[[2]]),formula=trans[[3]]))
      return(list(name = names(compute)[[i]], formula=trans[[2]]))
    }
    if (is.null(trans$name)) trans$name = names(compute)[[i]]
    trans$formula = f2c(trans$formula)
    trans
  }))
  nature = name.by.name(lapply(seq_along(nature), function(i) {
    x = nature[[i]]
    if (is.null(x$name)) x$name = names(nature)[[i]]
    x$set = f2c(x$set)
    x$prob = f2c(x$prob)
    x
  }))
  actions = name.by.name(lapply(seq_along(actions), function(i) {
    x = actions[[i]]
    if (is.null(x$name)) x$name = names(actions)[[i]]
    x$set = f2c(x$set)
    x
  }))
  nlist(name,player,condition,observe, compute,nature, actions,...)
}

copy.game = function(game) {
  as.environment(as.list(game))
}


update.vg.stage = function(vg, name, player, condition, observe, compute, nature, actions,...) {
  restore.point("update.vg.stage")

  stage = vg$stages[[name]]

  if (!missing(player))
    stage$player = f2c(player)
  if (!missing(observe))
    stage$observe=f2c(observe)
  if (!missing(condition))
    stage$condition = f2c(condition)


  if (!missing(compute)) {
    stage$compute = name.by.name(lapply(seq_along(compute), function(i) {
      trans = compute[[i]]
      restore.point("stage385")
      if (is(trans,"formula")) {
        if (length(trans)==3)
          return(list(name=as.character(trans[[2]]),formula=trans[[3]]))
        return(list(name = names(compute)[[i]], formula=trans[[2]]))
      }
      if (is.null(trans$name)) trans$name = names(compute)[[i]]
      trans$formula = f2c(trans$formula)
      trans
    }))
  }

  if (!missing(nature)) {
    stage$nature = name.by.name(lapply(seq_along(nature), function(i) {
      x = nature[[i]]
      if (is.null(x$name)) x$name = names(nature)[[i]]
      x$set = f2c(x$set)
      x$prob = f2c(x$prob)
      x
    }))

  }
  if (!missing(actions)) {
    stage$actions = name.by.name(lapply(seq_along(actions), function(i) {
      x = actions[[i]]
      if (is.null(x$name)) x$name = names(actions)[[i]]
      x$set = f2c(x$set)
      x
    }))
  }

  vg$stages[[name]] = stage
  vg
}

clear.non.vg = function(game, keep=c("gameId","vg","players", "options","pref")) {
  restore.point("clear.non.vg")
  fields = setdiff(ls(game), keep)
  remove(list=fields,pos = game)
  invisible(game)
}

#' Return for each action all variables that are never
#' known when choosing the action
#'
find.unknown.vars.at.actions = function(tg) {
  vars = setdiff(tg$vars, names(tg$params))
  lev.actions = sapply(tg$lev.li[tg$action.levels], function(lev) lev$var)

  actions = unique(lev.actions)
  res = lapply(actions, function(action) {
    levs =tg$lev.li[tg$action.levels][lev.actions == action]
    known = NULL
    for (lev in levs) {
      cs = colSums(lev$know.mat)
      known = union(known, names(cs)[cs>0])
    }
    setdiff(vars, known)
  })
  names(res) = actions
  res
}
