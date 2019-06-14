
#' Changes one or several parameters of a game
#'
#' For an already compiled game, we try to change parameters
#' in a fashion that is faster than a complete recompilation.
#'
#' @family Modify Game
#' @family Game Parameters
game_change_param = function(game, ..., params=list(), verbose=isTRUE(game$options$verbose>0)) {
  args = list(...)

  restore.point("game_change_param")
  #stop()

  # So far no speed ups implemented
  # everything will be newly compiled
  params = c(args, params)
  unknown = setdiff(names(params), names(game$vg$params))
  if (length(unknown)>0) {
    stop(paste0("The parameter(s) ", paste0(unknown, collapse=", "), " are not specified in the game."))
  }

  old.par = game$vg$params[names(params)]
  changed = sapply(names(params), function(par.name) {
    !identical(old.par[[par.name]], params[[par.name]])
  })
  params = params[changed]
  if (length(params)==0) {
    if (verbose)
      cat("\nNo parameter changed.")
    return(invisible(game))
  }

  game$vg$params[names(params)] = params
  if (is.null(game[["tg"]])) {
    if (verbose)
      cat("\nParameter changed. Game was not yet compiled.")
    return(invisible(game))
  }

  game$tg$params[names(params)] = params

  if (is.null(game$vars.info))
    game$vars.info = compute.vg.vars.info(game$vg)

  par.info = game$vars.info %>% filter(is.param, var %in% names(params))
  struc.change = any(par.info$affects.set | par.info$affects.structure)
  if (struc.change) {
    if (verbose)
      cat("\nStructural parameter(s) changed. Game will be recompiled next time an equilibrium is computed.")
    game$needs.recompile = TRUE
    return(invisible(game))
  }
  # Parameters for which at least probabilities must be newly computed
  prob.change = any(par.info$affects.prob)
  compute.change = any(par.info$affects.compute)
  if (!prob.change & !compute.change) {
    if (verbose) {
      cat("\nThe changed parameters have no effect on the game.")
      return(invisible(game))
    }
  }
  if (verbose) {
    cat(paste0("\nOnly parameters that affect ", if (prob.change) "probabilities ", if (prob.change & compute.change) "and ", if (compute.change) "computed variables ","have been changed. Update compiled game."))
  }
  if (compute.change) {
    recompute.tg.transformations(tg=game$tg, changed.par = names(params))
  }
  if (prob.change) {
    recompute.tg.probs(tg = game$tg, vg=game$vg,changed.par = names(params))
  }
  return(invisible(game))
}
