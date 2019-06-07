
# a vector of the classes of all vg variables
# useful for otree export without need to
# compute the whole table game tg representation
check.vg = function(vg) {
	restore.point("check.vg")

  # All action variables
  action.vars = unlist(lapply(vg$stages, function(stage) {
    get.names(stage$actions)
  }))
  nature.vars = unlist(lapply(vg$stages, function(stage) {
    get.names(stage$nature)
  }))
  compute.vars = unlist(lapply(vg$stages, function(stage) {
    get.names(stage$compute)
  }))
  vars = c(action.vars, nature.vars, compute.vars)
  dupl.vars = vars[duplicated(vars)]

  if (does.intersect(action.vars, dupl.vars)) {
    stop(paste0("Actions can only specified once in a single stage. The same variable name cannot be used elsewhere. Your actions(s) ", paste0(intersect(action.vars, dupl.vars), collapse=", ")," violate(s) this condition."))
  }

  if (does.intersect(names(vg$params), vars)) {
    stop(paste0("You cannot name an actions, move of natures or computed variable like a parameter. This is violated for your variable(s) ", paste0(intersect(names(vg$params), vars), collapse=", "),"."))
  }


  vg$kel = keyErrorLog(stop = FALSE)
  # I was so far to lazy to adapt the checking code in the function
  # below. So I just call it.
  try(extract.vg.vars.info(vg))
  if (length(vg$kel$log)>0) {
    txt = vg$kel$log
    txt = gsub("<br>","\n",txt,fixed = TRUE)

    txt = paste0("Errors found when checking your game:\n",txt)
    stop(txt, call.=FALSE, domain=NA)
  }

}
