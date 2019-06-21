# Note the tremble code must be changed since action names
# must be unique in the game. Also a pure uniform tremble is
# not very insightful...
vg.add.tremble = function(vg, action = NULL, stage = NULL, tremble.prob = 0.0001) {
  restore.point("vg.add.tremble")

  stages.li = lapply(vg$stages, function(s) {
    if (!is.null(stage)) {
      if (! s$name %in% stages) return(list(s))
    } else if (!is.null(action)) {
      if (length(intersect(action, names(s$action)))==0)
        return(list(s))
    }
    return(add.tremble.to.stage(s, tremble.prob))
  })
  vg$stages = name.by.name(do.call(c, stages.li))
  vg
}

# TO DO: Need to change names. Action name must be different
add.tremble.to.stage = function(stage, tremble.prob=0.0001) {
  if (length(stage$actions)==0)
    return(list(stage))
  restore.point("add.tremble.to.stage")

  player = stage$player
  will.name = paste0(stage$name,"_willTremble")
  perform.name = paste0(stage$name,"_performTremble")

  org_cond_str = deparse1(stage$condition)


  will_stage = stage(
    name = will.name,
    player = stage$player,
    nature = list(
      natureMove(will.name,c(TRUE, FALSE), c(tremble.prob, 1-tremble.prob))
    ),
    observe = will.name
  )
  tremble_stage = stage(
    player = stage$player,
    name = perform.name,
    nature = lapply(stage$actions, function(action) {
      natureMove(action$name, action$set)
    }),
    observe = c(stage$observe,names(stage$actions))
  )
  if (is.empty(stage$condition)) {
    tremble_stage$condition = parse.as.call(will.name)
  } else {
    cond_str = paste0("(",org_cond_str,") & ", will.name)
    tremble_stage$condition = parse.as.call(cond_str)
  }

  if (is.empty(stage$condition)) {
    stage$condition = parse.as.call(paste0("!", will.name))
  } else {
    cond_str = paste0("(",org_cond_str,") & !", will.name)
    stage$condition = parse.as.call(cond_str)
  }
  list(will_stage, tremble_stage, stage)
}
