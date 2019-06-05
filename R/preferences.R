#' Change the parameters of a preference object
pref_change_params = function(pref, ..., params=list(), label=NULL, players=1:2, numPlayers=length(players)) {
  new.params = c(list(...), params)
  restore.point("pref_change_param")
  type = pref$type
  if (type == "heterogeneous_players") {
    stop("Cannot yet change preference parameter for heterogeneous players.")
  }
  if (is.null(pref$utils_general)) {
    stop("Can only change parameters for preferences that have specified  general formulas in the field utils_general.")
  }
  use = intersect(names(new.params), names(pref$params))
  pref$params[use] = new.params[use]

  if (is.null(label) & !is.null(pref$label.fun)) {
    pref$label = pref$label.fun(pref$params)
  }

  pref$utils = lapply(pref$utils_general, function(u) substitute.call(u, pref$params))
  pref
}


#' Combine preferences for different players
#'
#' @param ... all preferences ordered by players
#' @param prefs alternatively the preferences as a list object
#' @param label optional label of preferences. If NULL the individual labels will be pasted together
#' @param type label of the combined preference type
pref_heterogeneous_players = function(..., prefs = list(...), label=NULL) {
  utils = lapply(prefs, function(pref) pref$utils)
  utils = do.call(c, utils)

  if (is.null(label))
    label = sapply(prefs, function(pref) pref$label) %>% unique %>% paste0(collapse="_")
  type = "heterogeneous_players"
  list(utils=utils, params=NULL, players = seq_along(utils), label=label, type=type)

}

#' Utility is equal to monetary payoff.
#'
#' This means the player is simply a risk
#' neutral expected payoff maximizer.
#'
#' @param player player(s) for which the preferences apply. Per default 1:2
pref_payoff = function(player=1:2,...) {
  restore.point("pref_payoff")
  res = list(
    utils = paste0("payoff_", player),
    params = list(),
    label = "payoff",
    type = "payoff"
  )
  class(res) = c("preferences","list")
  res
}


#' Create a custom preference
#'
#' @param ... Unquoted that describe the utility as a function of the parameters of the game and possible preference parameters. Should be ordered by players. Names are irrelevant.
#' @param params An optional list of parameters that are used in the formulas above
#' @param label A label for the preference, should contain info about the parameters
#' @param type A general type label independet of the parameters
pref_custom = function(..., params=NULL, label="custom") {
  utils_general = eval(substitute(alist(...)))
  restore.point("pref_custom")
  if (is.null(players)) {
    players = seq_along(utils_general)
  }
  utils = lapply(utils_general, function(u) substitute.call(u, params))
  res = list(utils_general = utils_general, utils=utils, params=params, label=label, type="custom")
  class(res) = c("preferences","list")
  res
}

#' Fehr-Schmidt inequality aversion.
#'
#' @param alpha the degree of envy
#' @param beta the degree of guilt
#' @param player player(s) for which the preferences apply. Per default 1:2
#' @param numPlayers number of players in game per default 2
pref_ineqAv = function(alpha=0.75,beta=0.5,player=1:numPlayers, numPlayers=2,...) {
  restore.point("pref_ineqAv")

  utils_general = vector("list", length(player))
  n = numPlayers
  for (counter in seq_along(player)) {
    i = player[counter]
    j = (1:n)[-i]
    utils_general[[counter]] = parse.as.call(paste0("payoff_",i,
      # envy
      " - (alpha /",n-1,")*(",
        paste0("pmax(payoff_",j,"-payoff_",i,",0)",collapse="+"),
      ")",
      # guilt
      " - (beta/",n-1,")*(",
        paste0("pmax(payoff_",i,"-payoff_",j,",0)",collapse="+"),
      ")"
    ))
  }
  params = list(alpha=alpha, beta=beta)
  utils = lapply(utils_general, function(u) substitute.call(u, params))
  label.fun = function(params) paste0("ineq",params$alpha*100,"_",params$beta*100)
  label = paste0("ineq",alpha*100,"_",beta*100)

  pref = list(utils_general = utils_general, utils=utils, params=params, label=label, label.fun=label.fun, type="ineqAv")
  class(pref) = c("preferences","list")
  pref
}

#' Fehr-Schmidt inequality aversion with envy only
#'
#' @param alpha the degree of envy
#' @param player player(s) for which the preferences apply. Per default 1:2
#' @param numPlayers number of players in game per default 2
pref_envy = function(alpha=0.75,player=1:numPlayers, numPlayers=2,...) {
  restore.point("pref_envy")
  pref = pref_ineqAv(alpha=alpha, beta=0, player=player, numPlayers=numPlayers)
  pref$type = "envy"
  pref$label = paste0("envy_", alpha*100)
  pref$label.fun = function(params) paste0("envy",params$alpha*100,"_",params$beta*100)

  pref
}
