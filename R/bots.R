#' Convenience function to create a list of bots for all players
#'
#' Every player gets the same bot type
#'
#' @param game the game object
#' @param bot_fun a bot function like e.g. \code{\link{bot_eq}}
#' @param ... additional arguments passed to \code{bot_fun}
#'
#' @family Bots
make_bots = function(game,bot_fun, ..., players=game$players) {
  lapply(players, function(player) {
    do.call(bot_fun, list(player=player, game=game,...))
  })
}

#' Bot that chooses all actions randomly
#'
#' Always picks each possible move with equal probability
#'
#' @param game the game object
#' @param player the player number of this bot
#' @family Bots
bot_random = function(game,player,...) {
  bot = list(
    name = "random_bot",
    player = player,
    choose_action = function(set,...) {
      sample(set,1)
    }
  )
  bot
}

#' Bot that plays according to a specified equilibrium
#'
#' @param game the game object
#' @param player the player number of this bot
#' @family Bots
bot_eq = function(game, player, eq=game$eq.li[[1]], eq.tables=gtree::eq.tables(eq,tg=game$tg,reduce.tables = TRUE), name="eq_bot") {
  restore.point("bot_eq")
  bot_tables(game, player, tables = eq.tables, name=name)
}

#' Bot whose actions are determined by key-action tables
#'
#' @param game the game object
#' @param player the player number of this bot
#' @param tables a list of tables for each action. The result of \code{eq_tables} is
#' @family Bots
bot_tables = function(game, player, tables, name="table_bot") {
  restore.point("bot_tables")
  bot = list(
    name = name,
    player = player,
    choose_action = choose_action_bot_tables,
    extra.arg = list(tables=tables)
  )
  bot
}
choose_action_bot_tables = function(play, stage, action,set,tables,...) {
  #restore.point("bot_eq$choose_action")
  var = action$name
  table = tables[[var]]
  hist.df = as_tibble(play$hist)
  keys = setdiff(colnames(table), c(var, ".prob", "eq.inds", "eq.ind"))
  if (has.col(table, ".prob")) {
    # Mixed strategy table
    table = semi_join(table, hist.df, by=keys)
    val = sample(table[[var]],1, prob = table$.prob)
  } else {
    # Pure strategy table
    val = semi_join(table, hist.df, by=keys)[[var]]
  }
  return(val)
}
