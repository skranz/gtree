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

  eq_li(game)
  eq_outcomes(game)
  eq_li.tables(game, combine=TRUE)


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

#' Create a new gtree game
#'
#' See the examples on gtree website for detailed explanation.
#'
#' @family Create Game
new_game = function(gameId, params=game_params(), options=make_game_options(), stages, variant="", check=TRUE) {
  restore.point("new.game")
  vg = as.environment(list(
    gameId = gameId,
    variant = variant,
    params = params,
    stages = stages,
    kel = keyErrorLog(stop=TRUE)
  ))
  # Extract additional information
  # and check vg
  if (check) check.vg(vg)
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

#' Compile a game defined with \code{new_game}
#'
#' @family Build Game
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

  if (add.sg | add.spi | add.spo)
    compute.tg.fields.for.internal.solver(game$tg, add.sg=add.sg,add.spi=add.spi,add.spo=add.spo)


  invisible(game)
}

#' Solve equilibria of a game
#'
#' With the default arguments the internal gtree solver is used
#' to find all pure strategy subgame perfect equilibria of the game.
#'
#' @param game the game object created with new_game
#' @param verbose
#' @param use.gambit solve via Gambit. Changing \code{mixed} or \code{just.spe} or specifying a \code{gambit.command} has only impact if \code{use.gambit=TRUE}.  See \code{\link{game_gambit_solve}} for details.
#' @family eq
#' @export
game_solve_spe = game_solve = function(game, mixed=FALSE, just.spe=TRUE, use.gambit = mixed | !just.spe, verbose=isTRUE(game$options$verbose>=1), gambit.command = NULL,...) {
  restore.point("game_solve_spe")
  if (use.gambit) {
    return(game_gambit_solve(game, mixed=mixed, just.spe=just.spe, verbose=verbose, ...))
  } else if (mixed | !just.spe) {
    stop("You must set use.gambit=TRUE or call game_gambit_solve if you want to solve for mixed stratetgy equilibria or for all pure NE, including NE that are not subgame perfect.")
  }

  game_compile(game,verbose=verbose)

  eq.li = gtree.solve.spe(tg = game$tg, verbose=verbose)
  game$eq.li = eq.li
  game$eqo.df = game$eeqo.df = NULL
  invisible(game)
}

#' Solve equilibria of a game using Gambit
#'
#' You need to install Gambit \url{http://www.gambit-project.org} to
#' use this function.
#'
#' @param game the game object created with new_game
#' @param gambit.command  A Gambit command line command with options but not file name. For example \code{"gambit-enummixed -q"} to compute all extreme point mixed equilibria. The different Gambit command line solvers are described here:
#' \url{http://www.gambit-project.org/gambit16/16.0.0/tools.html}
#' If left as NULL a default gambit command line solver with appropriate arguments will be chosen, depending on your arguments for mixed and just.spe
#' @param mixed relevant if no explicit gambit.command is given. If FALSE (default) only pure strategy equilibria will be computed, otherwise try to compute one mixed equilibrium.
#' @param just.spe if TRUE compute only SPE. If FALSE all NE will be computed.
#' @param add.q.flag The gambit command line solver should always be called with the option "-q" for gtree to be able to parse the returned output. If add.q.flag is TRUE we will add this flag if you have not yet added it to your \code{gambit.command}
#' @param gambit.dir The directory where to find the Gambit command line solvers. Ideally, you put this directory into the search path of your system and can keep the default \code{gambit.dir = ""}. To globally change the default directory adapt the following code \code{options(gtree.gambit.dir = "/PATH/TO/GAMBIT")}
#' @param efg.dir To solve via Gambit we first write the game tree into an .efg file. If \code{efg.dir} is NULL (default), the file will be written to a temporary directory. But you can also specify a custom directory here, e.g. if you want to take a look at the file.
#' @param efg.file If NULL a default file name for the efg file will be generated based on the name of the game and the specified preferences. But you can specify a custom name here.
#' @param verbose if TRUE show some extra information
#' @family eq
#' @family Gambit
#' @export
game_gambit_solve = function(game,gambit.command = NULL, mixed=FALSE, just.spe=TRUE,gambit.dir=first.non.null(getOption("gtree.gambit.dir"),""),efg.dir = NULL, efg.file=NULL,  verbose=isTRUE(game$options$verbose>=1), add.q.flag = TRUE,  ...) {
  restore.point("game_solve_with_gambit")
  game_compile(game, verbose=verbose)

  if (is.null(efg.file)) {
    efg.file = tg.efg.file.name(game$tg)
  }
  if (!is.null(efg.dir)) {
    game_write_efg(game,file.with.dir = file.path(efg.dir, efg.file))
  }

  if (!is.null(gambit.command)) {
    if (!has.substr(gambit.command,"-q") & add.q.flag) {
      gambit.command = paste0(gambit.command, " -q")
    }
  }

  game$eq.li = gambit.solve.eq(tg=game$tg, mixed=mixed, just.spe=just.spe,efg.file=efg.file, efg.dir=efg.dir, gambit.dir=gambit.dir, solver=gambit.command, verbose=verbose)
  game$eqo.df = game$eeqo.df = NULL
  invisible(game)
}

#' Solve for quantal response equilibria using Gambit
#'
#' This function computes logit agent quantal response equilibria using the Gambit solver gambit-logit. For a short description see the \href{https://en.wikipedia.org/wiki/Quantal_response_equilibrium}{Wikipedia article} and the gambit-logit solver's \href{documentation}{https://gambitproject.readthedocs.io/en/latest/tools.html#gambit-logit-compute-quantal-response-equilbria}. Details are in the article \href{Using Quantal Response to Compute Nash and Sequential Equilibria}{https://link.springer.com/article/10.1007/s00199-009-0443-3} by Theodore Turocy. But unfortunately, the article can only be found behind a pay wall.
#'
#' For a description of the arguments see \code{\link{game_gambit_solve}}
#' @family eq
game_gambit_solve_qre = function(game, gambit.command = "gambit-logit -q -l",gambit.dir="", efg.file=NULL, efg.dir = NULL, verbose=isTRUE(game$options$verbose>=1)) {

}

#' Set players' preferences
#'
#' This function sets players preferences to a parametrized preference
#' type. To specify completely custom preferences use game_set_util_fun
#' instead.
#'
#' @param game The game object
#' @param pref A preference created with a function starting with \code{pref_}, like e.g. \code{pref_ineqAv(alpha=1, beta=0.5)}. Use \code{pref_custom} to specify custom preferences.
#' @family Preferences
#' @family Modify Game
game_set_preferences = function(game, pref) {
  restore.point("game_set_preferences")
  game$pref = pref
  invisible(game)
}

#' Make a deep copy of a game
#'
#' @family Modify Game
game_copy = function(game) {
  ngame = as.environment(as.list(game))
  class(ngame) = class(game)
  ngame$vg = as.environment(as.list(ngame$vg))
  ngame$tg = as.environment(as.list(ngame$tg))
  ngame
}

# Note the tremble code must be changed since action names
# must be unique in the game. Also a pure uniform tremble is
# not very insightful...
game_add_tremble = function(game, action=NULL, tremble.prob = 0.0001) {
  clear.non.vg(game)
  game$vg = vg.add.tremble(game$vg, action=action, tremble.prob = tremble.prob)
  invisible(game)
}

#' Write game as a Gambit efg file
#'
#' @param game The game object
#' @param file.with.dir The file with full path. If NULL create a default name
#' @param file The file name without directory
#' @param dir The directory of a file
#' @family Gambit
game_write_efg = function(game,file.with.dir = file.path(dir, file), file=tg.efg.file.name(game$tg), dir=getwd(),  verbose = !isTRUE(game$options$verbose==0)) {
  game_compile(game)
  game$efg.file = file.with.dir
  tg.to.efg(game$tg, file.with.dir = file.with.dir, verbose=verbose)
  invisible(game)
}


#' Fix move probabilities of actions
#'
#' The function corresponds the provided actions into moves of nature with specified move probabilities. Can be a useful step when checking for existence of equilibria with particular structure.
#'
#' For fixing pure strategies \code{\link{game_fix_action_preferences}} is preferable when using the \code{gambit-logit} solver that can find sequential equilibria, by using logit trembles.
#'
#' @param actions a named list. The names correspond to action names. The default value to fix mixed strategies is a table that specifies conditional move probabilities (see example). If you want to fix pure actions you can also provide arguments as in \code{\link{game_fix_action_preferences}}.
#' @param ... directly the named arguments from which \code{actions} will be constructed
#' @param tremble.prob If a positive number, we assume that with this probability the player trembles and then chooses a random action with uniform probability. Trembles can be useful to enforce some sequential rationality in continuation play, but note that uniform trembles are not neccessarily the correct form of trembles to find sequential equilibria or trembling hand perfect equilibria.
#' @family Fix Actions
game_fix_actions = function(game, ..., actions=list(...), tremble.prob = NULL) {
  restore.point("game_fix_actions")
  game = clear.non.vg(game)
  game$vg = fix.vg.actions(game$vg,fix.li=actions, tremble.prob = tremble.prob)
  invisible(game)
}


#' Return a data frame of all possible outcomes
#' @param game the game object defined with \code{new_game} and being compiled with \code{game_compile} or after a call of \code{game_solve}.
#' @param reduce.cols if TRUE remove some technical columns
get_outcomes = function(game,reduce.cols=TRUE) {
  if (is.null(game[["tg"]])) {
    game_compile(game)
  }
  oco.df = game$tg$oco.df
  if (reduce.cols) {
    ignore.cols = c(names(game$vg$params),".prob",".outcome")
    cols = setdiff(colnames(oco.df), ignore.cols)
    oco.df = oco.df[, cols]
  }
  oco.df
}


#' Return solved equilibrium in a table format
#'
#' Best take a look at the Vignettes to understand this format.
#'
#' @family eq
#' @param reduce.tables (default = TRUE). Shall we try to reduce the rows and columns of the key tables be reduced to get a subset of neccessary keys that perfectly predict the chosen value of an action?
#' @param combine if 0 generate separate tables for each equilibrium. If 1 bind the tables of each variable over all equilibria. If 2 (default) also collapse the rows that are the same for different equilibria and add a column eq.inds that contains all equilibrium numbers as a comma separated string
#' @param eq.ind Vector of integers specifying the indices of all equilibria that shall be considered. By default all equilibria.
#' @param ignore.keys A character vector of variables that will always be removed from the key variables, without any check whether they are neccessary or not.
eq_tables = function(game,reduce.tables = TRUE, combine=2, eq.ind=seq_along(game$eq.li), ignore.keys = NULL, ...) {
  if (is.null(game$eq.li))
    stop("Please first solve your game.")

  if (is.null(game$unknown.vars.at.actions)) {
    game$unknown.vars.at.actions = find.unknown.vars.at.actions(game$tg)
  }

  eq.li.tables(game$eq.li[eq.ind], tg = game$tg, combine=combine,reduce.tables = reduce.tables, ignore.keys = union(names(game$vg$params), ignore.keys), ignore.li = game$unknown.vars.at.actions,  ...)
}

#' Return the computed equilibria using the internal representation
#' @family eq
eq_li = function(game,...) {
  game$eq.li
}

#' Return a data frame of all equilibrium outcomes
#'
#' If we have mixed strategies or moves of nature an
#' equilibrium outcome will consist of several rows. One row
#' for each pure outcome that occurs with positive probability.
#'
#' Typically \code{\link{eq_expected_outcomes}} will
#' deliver a version that is easier to read.
#' It will take expected values and reduce
#' each outcome to one row.
#'
#' Yet \code{eq_outcomes} may be more useful for automatical
#' analysis.
#'
#' @family eq
#' @param game the game object for which previously equilibria were computed e.g. with \code{game_solve}.
#' @param add.move.probs if \code{TRUE} add for each action (and move of nature) the probability that the actual value has been chosen in the corresponding information set (node).
#' @family eq
eq_outcomes = function(game, add.move.probs=FALSE) {
  if (is.null(game$eq.li))
    stop("Please first solve your game.")

  res = eq.li.outcomes(eq.li = game$eq.li, tg=game$tg)
  cols = setdiff(colnames(res), c("numPlayers", ".outcome","eqo.ind"))
  res = res[,c(cols,"eqo.ind")]
  res
}

#' Return a data frame of expected equilibrium outcomes
#'
#' Each row will describe a possible expected
#' equilibrium outcome. For numerical variables like
#' \code{payoff_1} the expected value on the equilibrium
#' path is returned.
#'
#' For qualitative variables, we generate a string like
#' \code{"accept(0.3),reject(0.2)"} describing the moves
#' that occur with positive probability and those
#' probabilities on the equilibrium path.
#' @family eq
#' @param game the game object for which prevoiously equilibria were computed e.g. with \code{game_solve}.
#' @param like.factor an optional character vector of names of numerical variables that shall be presented like qualitative variables.
eq_expected_outcomes = function(game, like.factor = NULL) {
  if (is.null(game$eq.li))
    stop("Please first solve your game.")

  res = eq.li.expected.outcomes(eq.li = game$eq.li, tg=game$tg, ignore.NA = TRUE, factor.vars = like.factor)
  cols = setdiff(colnames(res), c("numPlayers",".prob", ".outcome","eqo.ind"))
  res = res[,c(cols,"eqo.ind")]
  res
}

#' Return conditional equilibrium outcomes
#'
#' @family eq
#' @param game the game object for which equilibria were computed e.g. with \code{game_solve}.
#' @param ... variable names and their assumed value. We set the probabilities of the conditioned variable values to 1. These correspond to equilibrium outcomes given an unexpected tremble that makes the variables take the specified values. Variables can take multiple values. We then compute conditional equilibrium outcomes for each combination of values
#' @param fixed.list Alternativly to ... a named list with values to fix.
#' @param fixed.vars Alternative to ... or fixed.list, a vector of variable names. If provided, we compute the conditional expected outcomes holding fixed every possible combination of the variables stated in fixed.vars
eq_cond_outcomes = function(game,...,fixed.list=list(...),fixed.vars = NULL) {
  if (is.null(game$eq.li))
    stop("Please first solve your game.")

  if (is.null(names(fixed.list))) {
    fixed.vars = unlist(fixed.list)
  }
  if (!is.null(fixed.vars)) {
    fixed.list = unique(game$tg$oco.df[,fixed.vars]) %>%
      arrange_at(fixed.vars)
  }

  res = eq.li.cond.outcomes(game$eq.li,cond = fixed.list, tg = game$tg)
  cols = setdiff(colnames(res), c("numPlayers", ".outcome","cond.ind"))
  res = res[,c(cols,"cond.ind")]
  res
}

#' Return conditional expected equilibrium outcomes
#'
#' @family eq
#' @inheritParams eq_cond_outcomes
#' @param like.factor an optional character vector of names of numerical variables that shall be presented like qualitative variables.
eq_cond_expected_outcomes = function(game,...,fixed.list=list(...), fixed.vars = NULL, like.factor = NULL) {
  restore.point("eq_cond_expected_outcomes")
  if (is.null(game$eq.li))
    stop("Please first solve your game.")


  if (is.null(names(fixed.list))) {
    fixed.vars = unlist(fixed.list)
    fixed.list = NULL
  } else {
    has.name = names(fixed.list)!=""
    fixed.vars = unlist(fixed.list[!has.name])
    fixed.list = fixed.list[has.name]
  }
  if (!is.null(fixed.vars)) {
    if (length(fixed.list)>0) {
      stop("Sorry cannot yet combine arguments that are just variables names with arguments that fix values.")
    }
    fixed.list = unique(game$tg$oco.df[,fixed.vars]) %>%
      na.omit() %>%
      arrange_at(fixed.vars)
  }

  ceqo.df = eq.li.cond.outcomes(game$eq.li,cond = fixed.list, tg = game$tg)
  res = cond.expected.outcomes(ceqo.df, factor.vars = like.factor)
  cols = setdiff(colnames(res), c("numPlayers", ".prob",".outcome","cond.ind"))
  res = res[,c(cols,"cond.ind")]
  res
}


#' Specify the game parameters
#' This function is only to be used inside \code{new_game}. To change the parameters of an existing game call \code{game_change_params}.
#'
#' @family Build Game
#' @family Game Parameters
make_game_params = function(numPlayers=2,...) {
  list(numPlayers=numPlayers,...)
}

#' Specify the game options inside \code{new_game}
#'
#' @family Build Game
#' @family Game Options
#' @seealso game_set_options
make_game_options = function(verbose=TRUE,...) {
  list(verbose=verbose,...)
}

#' Change options of an already created game object
#'
#' See \code{make_game_options} for a description of the
#' available options.
#'
#' @family Game Options
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
#'
#'
#' @param name The variable name of the action
#' @param set The set of different action values. Can be a formula that depends on other game variables.
#' @param strategyMethodDomain if not NULL the action shall be specified via strategy method in an experiment. State the variable name upon which the action conditions
#' Only used when running an experiment or analysing experimental data
#' @family Define Stage
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
#' @family Define Stage
natureMove = function(name, set, probs=NULL, table=NULL, fixed=NULL, tremble.prob = NULL,...) {
  if (!is.null(table)) {
    list(name=name,table=table)
  } else if (!is.null(fixed)) {
    list(name=name,set=f2c(set),probs=NULL,fixed=fixed, tremble.prob = tremble.prob)
  } else {
    list(name=name,set=f2c(set),probs=f2c(probs))
  }
}

#' Specify a stage for a game
#' @param name Name of the stage
#' @param player The player who acts in this stage. Can be a rhs formula. If an action is chosen in the stage, there must be a unique player. If it is a stage in which no actions take place, the player variable multiple players can be set. Each player observes the variables specified under observe.
#' @param condition A logical condition specifying whether the stage will be run. Can be a rhs formula. If it evaluates to FALSE the stage will not be shown, i.e. no observations are made and no actions are chosen. Also no computations in this stage take place.
#' @param observe A vector of variable names specifying which variables are observed by the player(s) at this stage. Is relevant to correctly specify the information sets in the extensive form game.
#' @param compute A list of formulas like 'compute=list(payoff_1 ~ x-5)'. The lhs specifies a variable name and the rhs a DETERMINISTIC formula. The variables are computed at the beginning of the stage before actions and moves of nature take place. This means they can be used e.g. in formulas for action sets of the same stage.
#' @param nature A list of moves of nature, i.e. random variables from a finite set. E.g. nature=list(natureMove("proposer",c(1,2),prob=c(0.4,0.6)).
#' @param actions A list of actions. E.g. actions=list(action("offer",~0:cake_size)
#' @family Define Stage
#' @family Build Game
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


# Function to change a stage of an existing game
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

# Delete all fields except those of a vg
clear.non.vg = function(game, keep=c("gameId","vg","players", "options","pref")) {
  restore.point("clear.non.vg")
  fields = setdiff(ls(game), keep)
  remove(list=fields,pos = game)
  invisible(game)
}

# Return for each action all variables that are never
# known when choosing the action
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
