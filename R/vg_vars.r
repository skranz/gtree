# Classify variables of a vg
# Check whether they affect payoffs
# or other transformations, probabilities, sets
# or other stuff like conditions, players, etc


example.classify.vg.vars = function() {
  vg = new.vg(
    gameId = "RandomCostCournot",
    params = list(numPlayers=2, a=100, qMax=40,qMin=10,
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

  vg$vars.info = compute.vg.vars.info(vg)

}

compute.vg.vars.info = function(vg) {
  restore.point("compute.vg.vars.info")

  all.stage.vars = unlist(lapply(vg$stages, function(stage) {
    c(names(stage$compute), names(stage$nature), names(stage$actions))
  }))

  duplicated.vars = all.stage.vars[duplicated(all.stage.vars)]

  stage.vars = unique(all.stage.vars)


  compute.vars = unique(unlist(lapply(vg$stages, function(stage) {
    c(names(stage$compute))
  })))
  nature.vars = unique(unlist(lapply(vg$stages, function(stage) {
    c(names(stage$nature))
  })))
  action.vars = unique(unlist(lapply(vg$stages, function(stage) {
    c(names(stage$actions))
  })))


  vars = c(names(vg$params), stage.vars)


  var.info = as_data_frame(list(
    var = vars,
    is.param = vars %in% names(vg$params),
    is.compute = vars %in% compute.vars,
    is.nature = vars %in% nature.vars,
    is.action = vars %in% action.vars,
    single.definition = !(vars %in% duplicated.vars)
  ))


  # Determine whether variables affect payoffs
  # probabilities or the game structure
  # (sets, conditions, observations, players)
  n = length(vars)
  affects.payoff = rep(FALSE, n)
  names(affects.payoff) = vars
  affects.prob = affects.set = affects.structure = affects.payoff

  affects.payoff[str.starts.with(vars, "payoff_")] = TRUE

  # Has row a direct influence via formula or set on col?
  imat = matrix(FALSE,n,n)
  rownames(imat) = colnames(imat) = vars

  # Go in reverse order through all stages
  # Build influence graph and save direct influences
  # on prob or structure
  for (stage.num in length(vg$stages):1) {
    stage = vg$stages[[stage.num]]
    for (x in rev(stage$actions)) {
      ivars = find.vg.formula.vars(x$set)
      if (length(ivars)>0) {
        affects.set[ivars] = TRUE
        imat[ivars,x$name] = TRUE
      }
    }
    for (x in rev(stage$nature)) {
      ivars = find.vg.formula.vars(x$set)
      if (length(ivars)>0) {
        affects.set[ivars] = TRUE
        imat[ivars,x$name] = TRUE
      }
      ivars = find.vg.formula.vars(x$probs)
      affects.prob[ivars] = TRUE
    }
    for (x in rev(stage$compute)) {
      ivars = find.vg.formula.vars(x$formula)
      if (length(ivars)>0) {
        imat[ivars,x$name] = TRUE
      }
    }
    ivars = unique(c(
      find.vg.formula.vars(stage$condition),
      find.vg.formula.vars(stage$player),
      find.vg.formula.vars(stage$observe)
    ))
    affects.structure[ivars] = TRUE
  }

  affects.structure["numPlayers"] = TRUE
  # Find all indirectly connected variables
  max.path = length(stage.vars)

  dmat = imat
  for (i in 1:max.path) {
    imat = imat + imat %*% imat
    imat = imat > 0
  }

  # Update affect
  for (var in stage.vars) {
    ivars = vars[imat[,var]]

    if (length(ivars)>0) {
      affects.payoff[ivars] = affects.payoff[ivars] | affects.payoff[var]
      affects.prob[ivars] = affects.prob[ivars] | affects.prob[var]
      affects.set[ivars] = affects.set[ivars] | affects.set[var]
      affects.structure[ivars] = affects.structure[ivars] | affects.structure[var]
    }
  }

  var.info = cbind(var.info, affects.payoff, affects.prob, affects.set, affects.structure)
  var.info

}

find.vg.formula.vars = function(formula, return.string=FALSE) {
  if (!is.call(formula) & !is.name(formula)) {
    if (return.string) {
      if (is.character(formula)) return(formula)
    }
    return(NULL)
  }
  find.variables(formula)
}
