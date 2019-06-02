#' Combine preferences for different players
#'
#' @param ... all preferences ordered by players
#' @param prefs alternatively the preferences as a list object
#' @param label optional label of preferences. If NULL the individual labels will be pasted together
#' @param type label of the combined preference type
pref_combine = function(..., prefs = list(...), label=NULL, type="multi") {
  utils = do.call(c,prefs)
  if (is.null(label))
    label = sapply(prefs, function(pref) pref$label) %>% unique %>% paste0(collapse="_")
  list(utils=utils, params=NULL, players = seq_along(utils), label=label, type=type)

}

#' Create a custom preference
#'
#' @param ... Unquoted that describe the utility as a function of the parameters of the game and possible preference parameters. Should be ordered by players. Names are irrelevant.
#' @param params An optional list of parameters that are used in the formulas above
#' @param label A label for the preference, should contain info about the parameters
#' @param type A general type label independet of the parameters
pref_custom = function(..., params=NULL, label="custom", type="custom") {
  utils_general = eval(substitute(alist(...)))
  restore.point("pref_custom")
  if (is.null(players)) {
    players = seq_along(utils_general)
  }
  utils = lapply(utils_general, function(u) substitute.call(u, params))
  res = list(utils_general = utils_general, utils=utils, params=params, label=label, type=type)
  class(res) = c("preferences","list")
  res
}

#' Fehr-Schmidt inequality aversion.
#'
#' @param alpha the degree of envy
#' @param beta the degree of guilt
#' @param player player(s) for which the preferences apply. Per default 1:2
#' @param numPlayers number of players in game per default 2
pref_ineqAv = function(alpha=0.75,beta=0.5,player=1:numPlayers, numPlayers=2) {
  restore.point("pref_ineqAv")
  utils = ineqAvUtil(alpha = alpha, beta=beta, player=player, n=numPlayers)
  res = list(
    utils = as.character(utils),
    params = list(alpha=alpha, beta=beta),
    label = names(utils)[1],
    type = "ineqAv"
  )
  class(res) = c("preferences","list")
  res
}

#' Fehr-Schmidt inequality aversion with envy only
#'
#' @param alpha the degree of envy
#' @param player player(s) for which the preferences apply. Per default 1:2
#' @param numPlayers number of players in game per default 2
pref_envy = function(alpha=0.75,player=1:numPlayers, numPlayers=2) {
  restore.point("pref_ineqAv")
  utils = envyUtil(alpha = alpha, player=player, n=numPlayers)
  list(
    utils = as.character(utils),
    params = list(alpha=alpha),
    label = names(util)[1],
    type = "envy"
  )
  class(res) = c("preferences","list")
  res
}
