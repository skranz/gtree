# Try to check in how far a proposed strategy profile described by a set of
# rules constitutes a Nash equilibrium in a vg game.
#
# Goal: No need to describe the whole game tree, such that also large games
# can be analysed.
example.check.vg.eq = function() {
  setwd("D:/libraries/gtree/myproject")

  gameId = "UltimatumGame"
	#gameId = "UG2"
	vg = get.vg(gameId = gameId)
  tg = get.tg(vg = vg, never.load = TRUE)
  tg
  eq.li = get.eq(tg)
  rules = eq.table.rules(eq.li[[1]], tg=tg,add.stage = TRUE)
  rules[[2]]$tables[[1]]$accept[2] = 0
  check.vg.rules.eq(vg, rules, check.all=TRUE)

  rvg = set.vg.rules(vg, rules[2])
  rtg = vg.to.tg(rvg,branching.limit = 10000)
  req.li = get.eq(rtg, never.load = TRUE,save.new = FALSE)
  eq.li.tables(req.li,rtg)


	rules = list(
    action.rule("offer",1, fixed=!TRUE),
    action.rule("accept",(offer >= 2)*1, fixed=!TRUE)
  )



  check.vg.rules.eq(vg, rules, check.all=TRUE)



	gameId = "Cournot"
	#gameId = "UG2"
	vg = get.vg(gameId = gameId)
	tg = vg.to.tg(vg, branching.limit = 10000)
	tg
	vg$params$maxCost = 5
  rules = list(
    action.rule("q1",(100-2*c1)/3+2,fixed=FALSE),
    action.rule("q2",(100+c1)/3, fixed=TRUE)
  )
  #rules = list(
  #  action.rule("q1",(100-2*c1)/3),
  #  action.rule("q2",(100-q1)/2)
  #)

  check.vg.rules.eq(vg, rules,check.all = TRUE)

  disable.restore.points(TRUE)

  Rprof(tmp <- tempfile())
  check.vg.rules.eq(vg, rules)
  Rprof()
  summaryRprof(tmp)


}


example.check.vg.eq = function() {
  setwd("D:/libraries/gtree/myproject")

	gameId = "LargeCournot"
	#gameId = "UG2"
	vg = get.vg(gameId = gameId)
	vg$params$maxCost = 5
  rules = list(
    action.rule("q1",(100-2*c1)/3+2,fixed=FALSE),
    action.rule("q2",(100+c1)/3, fixed=TRUE)
  )
  #rules = list(
  #  action.rule("q1",(100-2*c1)/3),
  #  action.rule("q2",(100-q1)/2)
  #)

  check.vg.rules.eq(vg, rules,check.all = TRUE)

  disable.restore.points(TRUE)

  Rprof(tmp <- tempfile())
  check.vg.rules.eq(vg, rules)
  Rprof()
  summaryRprof(tmp)


  gameId = "UltimatumGame"
	#gameId = "UG2"
	vg = get.vg(gameId = gameId)
  rules = list(
    action.rule("offer",1, fixed=!TRUE),
    action.rule("accept",(offer >= 2)*1, fixed=!TRUE)
  )

  check.vg.rules.eq(vg, rules, check.all=TRUE)
}

check.vg.rules.eq = function(vg, rules, util.funs = NULL, check.all=FALSE, find.best.deviation=TRUE, order=c("random","top-down","bottom-up")[check.all+1], progress.bar=TRUE, return.value=c("info","gain","details")[1]) {
  restore.point("check.vg.rules.eq")

  # Play according to specified rules
  rp = play.vg.rules(vg, rules, make.stage.li=TRUE)

  # Compute expected utility of rule play
  rules.util = vg.play.expected.utility(rp$play,util.funs = NULL, numPlayers=vg$params$numPlayers)


  stage.li = rp$stage.li

  # Compute a list that contains for each stage
  # a data frame of all possible deviations at that stage
  dev.li = lapply(seq_along(vg$stages), vg.stage.deviations, stage.li=stage.li, vg=vg)

  # Create a table that references to all possible deviations
  dev.ref = bind_rows(lapply(dev.li, function(dev) {
    if (NROW((dev))==0) return(NULL)
    as_data_frame(list(stage.num = dev$.stage.num,.info.set=dev$.info.set,row=1:NROW(dev), group=paste0(dev$.stage.num,"-",dev$.info.set)))
  }))


  # Set order in which deviations are checked
  if (order=="random" & !find.best.deviation) {
    dev.ref = sample_frac(dev.ref)
  } else if (order=="bottom-up") {
    dev.ref = arrange(dev.ref, stage.num, .info.set, row)
    dev.ref = dev.ref[rev(seq_len(NROW(dev.ref))),]
  }

  found.dev = FALSE
  if (!find.best.deviation & !check.all) {
    if (progress.bar)
      pb = utils::txtProgressBar(min=0,max=NROW(dev.ref),initial = 0)

    for (i in seq_len(NROW(dev.ref))) {
      if (progress.bar)
        utils::setTxtProgressBar(pb,i)

      dev = dev.li[[ dev.ref$stage.num[i] ]][dev.ref$row[i],]


      dev.play = play.vg.rules.with.deviation(dev=dev,vg=vg, rules=rules, stage.li=stage.li)
      dev.util = vg.play.expected.utility(dev.play, util.funs, numPlayers=vg$params$numPlayers)

      found.dev = dev.util[dev$.player] > rules.util[dev$.player]
      if (found.dev) break
    }
  } else if (find.best.deviation & !check.all) {
    # Split dev.ref into groups
    group.li = split(dev.ref, dev.ref$group)
    if (order=="random") {
      inds = sample.int(length(group.li))
    } else {
      inds = seq_along(group.li)
    }

    if (progress.bar)
      pb = utils::txtProgressBar(min=0,max=NROW(group.li),initial = 0)

    counter = 0
    for (group.ind in inds) {
      counter = counter+1
      best.util = -Inf
      best.dev = NULL
      if (progress.bar)
        utils::setTxtProgressBar(pb,counter)
      dev.ref = group.li[[group.ind]]
      dev.stage = dev.li[[ dev.ref$stage.num[1] ]]
      for (i in seq_len(NROW(dev.ref))) {
        dev = dev.stage[dev.ref$row[i],]
        dev.play = play.vg.rules.with.deviation(dev=dev,vg=vg, rules=rules, stage.li=stage.li)
        dev.util = vg.play.expected.utility(dev.play, util.funs,players=dev$.player)
        if (dev.util > best.util) {
          best.util = dev.util
          best.dev = dev
        }
      }
      found.dev = best.util > rules.util[best.dev$.player]
      if (found.dev) {
        dev = best.dev
        break
      }
    }
  } else if (check.all) {
    # Split dev.ref into groups
    group.li = split(dev.ref, dev.ref$group)
    devs = vector("list", length(group.li))
    devs.util = rep(0, length(group.li))
    if (progress.bar)
      pb = utils::txtProgressBar(min=0,max=NROW(group.li),initial = 0)

    num.devs = 0
    for (group.ind in seq_along(group.li)) {
      best.util = -Inf
      best.dev = NULL
      found.dev = FALSE
      if (progress.bar)
        utils::setTxtProgressBar(pb,group.ind)
      dev.ref = group.li[[group.ind]]
      dev.stage = dev.li[[ dev.ref$stage.num[1] ]]
      for (i in seq_len(NROW(dev.ref))) {
        dev = dev.stage[dev.ref$row[i],]
        dev.play = play.vg.rules.with.deviation(dev=dev,vg=vg, rules=rules, stage.li=stage.li)
        dev.util = vg.play.expected.utility(dev.play, util.funs, players=dev$.player)
        if (dev.util > best.util) {
          best.util = dev.util
          best.dev = dev
        }
      }
      found.dev = best.util > rules.util[best.dev$.player]
      if (found.dev) {
        num.devs = num.devs+1
        devs[[num.devs]] = best.dev
        devs.util[num.devs] =  best.util
      }
    }

    if (num.devs>0) {
      infos = lapply(1:num.devs, function(dev.ind) {
        dev = devs[[dev.ind]]
        get.play.deviation.info(dev,vg=vg, stage.li=stage.li,dev.util = devs.util[dev.ind], rules.util=rules.util[dev$.player])
      })
      info.block = sapply(infos, function(info) paste0(colnames(info), collapse=";"))
      blocks = unique(info.block)
      binfos = lapply(blocks, function(block) {
        restore.point("hdhfkdfhdkfhf")
        inds = which(info.block == block)
        bind_rows(infos[inds])
      })
      return(list(
        info = binfos,
        is.eq=FALSE
      ))
    } else {
      return(nlist(rules.util, is.eq=TRUE))
    }
  }
  if (progress.bar) close(pb)

  if (found.dev) {
    return(list(
      info=get.play.deviation.info(dev, vg=vg, dev.util=best.util,rules.util = rules.util[dev$.player], stage.li=stage.li),
      is.eq=FALSE
    ))
  } else {
    return(nlist(rules.util, is.eq=TRUE))
  }
}

get.play.deviation.info = function(dev, stage.df = stage.li[[dev$.stage.num]], vg, dev.util=NULL, rules.util=NULL, stage.li=NULL) {
  restore.point("get.play.deviation.info")
  actions = setdiff(colnames(dev),c(".stage.num",".info.set",".player"))

  rows = which(stage.df$.info.set == dev$.info.set)
  org = stage.df[rows[1],]

  dev.action = actions[which(unlist(org[,actions]) != unlist(dev[actions]))]

  key.actions = intersect(setdiff(names(org), c(dev.action,names(vg$params))), vg$vars)

  res = cbind(dev[,c(".stage.num",".player")], org[,key.actions], org[,dev.action], dev[,dev.action])

  colnames(res)[(-1:0)+NCOL(res)] = paste0(dev.action, c(".rule",".dev"))
  if (!is.null(dev.util)) {
    res = cbind(res, list(util.rule=rules.util,util.dev=dev.util))
  }
  res
}


action.rule = function(var, formula, condition=NULL, stage=NULL, fixed=FALSE) {
  formula=substitute(formula)
  condition=substitute(condition)
  nlist(var, formula, condition, stage, fixed)
}


play.vg.rules.with.deviation = function(dev, rules, stage.li, vg, stage.num=dev$.stage.num, make.stage.li=FALSE, extra.par=NULL,pretty.play=FALSE,...) {
  restore.point("play.vg.rules.with.deviation")

  # Actions for which we might deviate
  actions = names(dev)[!str.starts.with(names(dev),".")]

  # Original play under rules
  play = stage.li[[stage.num]]

  # Rows corresponding to information set in which deviation
  # takes place
  replace.rows = which(play$.info.set == dev$.info.set)

  # Change chosen action to the deviation
  for (action in actions) {
    play[replace.rows,action] = dev[[action]]
  }

  # Now play the deviation
  play.vg.rules(vg=vg, rules=rules,play=play,start.stage = stage.num+1, make.stage.li=make.stage.li,extra.par=extra.par,pretty.play=pretty.play,...)
}

vg.stage.deviations = function(stage.num, play=stage.li[[stage.num]], vg, stage.li, ignore.fixed=TRUE) {
  vg.stage.action.sets.by.info.set(stage.num, play, vg, only.deviations=TRUE, ignore.fixed=ignore.fixed)
}

vg.stage.action.sets.by.info.set = function(stage.num, play, vg, only.deviations=FALSE, ignore.fixed=FALSE) {
  restore.point("vg.stage.action.sets.by.info.set")

  if (is.null(play)) return(NULL)

  play$.stage.num = stage.num
  ise.play = play %>% group_by(.info.set) %>%
    summarise_all(first)


  stage = vg$stages[[stage.num]]


  res = bind_rows(lapply(stage$actions, function(action) {
    df = remove.cols(ise.play, action$name)
    eval.set.to.df(action$set,df=df,var=action$name)
  }))
  if (ignore.fixed) {
    res = filter(res, !.fixed)
  }


  res = res[,c(".stage.num", ".info.set", ".player", names(stage$actions))]
  if (only.deviations) {
    res = anti_join(res, ise.play, by=colnames(res))
    return(res)
  }

  ise.play$.rule.play = TRUE
  res = left_join(res, ise.play, by=colnames(res)) %>%
    mutate(
      .rule.play = ifelse(is.na(.rule.play), FALSE, TRUE)
    )


  res = res[,c(".stage.num", ".info.set",".player", ".rule.play", names(stage$actions))]
  res
}

play.vg.rules  = function(vg, rules=vg$rules, extra.par = list(), make.stage.li = FALSE, add.info.sets = make.stage.li, start.stage=1, play=stage.li[[start.stage]], stage.li=NULL, pretty.play=TRUE) {
  restore.point("play.vg.rules")
  #rules.vars = sapply(rules, function(rule) rule$var)

  if (is.null(play))
    play = as_data_frame(c(list(.prob=1),vg$params, extra.par))

  numPlayers = vg$params$numPlayers


  add.info.sets = make.stage.li
  if (make.stage.li) {
    stage.li = vector("list", length(vg$stages))
  }

  # In order to check for equilibrium
  # we need to compute information sets
  # and add the to stage.li
  if (add.info.sets) {
    # Initialize know.li elements with one column
    know.li = lapply(1:numPlayers, function(i) {
      as_data_frame(list(numPlayers=rep(TRUE, NROW(play))))
    })
  }

  stage.num = start.stage-1
  while (stage.num < length(vg$stages)) {
    stage.num = stage.num+1
    stage = vg$stages[[stage.num]]

    if (add.info.sets) {
      play$.info.set = NA
    }
    if (make.stage.li) play$.fixed = FALSE
    if (!is.empty(stage$condition)) {
      play$.omit = !eval.on.df(stage$condition, play)
      omit.play = filter(play,.omit)

      if (add.info.sets){
        omit.know.li = lapply(1:numPlayers, function(i) {
          know.mat[play$.omit,,drop=FALSE]
        })
        know.li = lapply(1:numPlayers, function(i) {
          know.mat[!play$.omit,,drop=FALSE]
        })
      }

      play = filter(play,!.omit)
    } else {
      omit.play = NULL
      omit.know.li = NULL
    }

    if (NROW(play)==0) {
      stop("PLAY HAS ZERO ROWS: NEED TO IMPLEMENT HANDLING")
    }

    # Compute transformations
    for (trans in stage$compute) {
      play[[trans$name]] = eval.on.df(trans$formula, play)
    }

    # Add observed variables to know.li and compute info.sets
    if (add.info.sets) {
      restore.point("jnsjfnjnfjd")
      play = add.call.players.to.df(call=stage$player, df=play, numPlayers=numPlayers)
      if (!all(is.empty(stage$observe))) {
        for (i in 1:numPlayers) {
          know.mat = know.li[[i]]
          if (!is.character(stage$observe))
            stop(paste0("Stage ", stage$name, " has specified a formula for observe. We can currently only deal with a fixed set of variables specified in observe. Best split up the stage in several stage, with different conditions and a fixed observed variables for each stage."))
          for (var in stage$observe) {
            rows = play[[paste0(".player_",i)]]
            if (!has.col(know.mat,var)) know.mat[[var]] = FALSE
            know.mat[rows,var] = TRUE
          }
          know.li[[i]] = know.mat
        }
      }

      # Create info set index for this level
      if (length(stage$actions)>0)
        play$.info.set = compute.info.sets(play, know.li, stage.num, just.index=TRUE)
      play$.ROW = seq_len(NROW(play))
    }

    # Run moves of nature
    for (nature in stage$nature) {
      play = eval.randomVar.to.df(nature$set, nature$probs,df = play,var=nature$name,kel=NULL)
      play$.prob = play$.prob * play$.move.prob
    }

    # Expand know.li given that additional rows have been generated
    if (add.info.sets & length(stage$nature)>0) {
      know.li = lapply(1:numPlayers, function(i) {
        know.li[[i]][play$.ROW,,drop=FALSE]
      })
    }

    # Play actions
    for (action in stage$actions) {
      var = action$name
      for (rule in rules) {
        if (rule$var != var) next
        if (is.character(rule$stage))
          if (rule$stage != stage$name) next

        if (is.numeric(rule$stage))
          if (rule$stage != stage$name) next

        if (!is.empty(rule$condition)) {
          rows = which(eval.on.df(rule$condition, play))
          if (length(rows)==0) next
          if (length(rows)==NROW(play)) rows=NULL
        } else {
          rows = NULL
        }

        # Evaluate rule for all p
        if (is.null(rows)) {
          if (!is.null(rule[["tables"]])) {
            play = eval.key.tables.to.df(play,rule$tables,var=rule$var)
          } else {
            play[[var]] = eval.on.df(rule$formula, play)
          }
          if (make.stage.li & isTRUE(rule$fixed))
            play$.fixed = TRUE
        } else {
          if (!is.null(rule[["tables"]])) {
            play = eval.key.table.to.df(play,rule$tables,var=rule$var, rows=rows)
          } else {
            play[[var]][rows] = eval.on.df(rule$formula, play[rows,,drop=FALSE])
          }

          if (make.stage.li & isTRUE(rule$fixed))
            play$.fixed[rows] = TRUE

        }
      }

      # Add knowledge of chosen action
      if (add.info.sets) {
        for (i in 1:numPlayers) {
          know.mat = know.li[[i]]
          player.rows = play$.player == i
          if (!has.col(know.mat,var)) know.mat[[var]] = FALSE
          know.mat[player.rows, var] = TRUE
          know.li[[i]] = bind_rows(know.mat, omit.know.li[[i]])
        }
      }
    }

    if (NROW(omit.play)>0) {
      play = bind_rows(play, omit.play)
    }

    if (make.stage.li & length(stage$actions)>0) {
      stage.li[[stage.num]] = play
    }

  }
  if (pretty.play) {
    play = remove.cols(play, c(paste0(".player_",1:numPlayers), ".player",".ROW",".info.set"))
  }
  if (make.stage.li) {
    return(list(stage.li=stage.li, play=play))
  }

  return(play)

}


add.call.players.to.df =  function(call,df,numPlayers) {
  restore.point("add.call.players.to.df")
  # compute player set for each node
  if (NROW(df)==0) return(df)
  players = 1:numPlayers

  # fixed player sets
  if (!is(call, "call") & !is(call,"name")) {
    for (i in players) {
      df[[paste0(".player_",i)]] = i %in% call
    }
    if (length(call)>0)
      df$.player = call[1]

    return(df)
  }

  # players is a call
  df$.ROW.FOR.PLAYER = seq.int(NROW(df))
  # reduce df to unique combination of used variables
  vars = find.variables(call)

  if (length(vars)==0) {
    stop("Please only use a formula in players if it depends on some earlier defined parameter or variable.")
  }

  if (length(unknown <- setdiff(vars, colnames(df)))>0) {
    stop("Your observe formula depends on variables, which have not been defined earlier.")
  }



  sdf = as_data_frame(unique(df[,vars,drop=FALSE]))

  for (i in players) {
    sdf[[paste0(".player_",i)]] = FALSE
    df[[paste0(".player_",i)]] = FALSE
  }

  for (row in seq.int(NROW(sdf))) {
    rdf = sdf[row,,drop=FALSE]
    call.players = eval(call,rdf)
    if (length(call.players)==0) next
    if (length(unknown <- setdiff(call.players, players))>0) {
        stop("Your evaluated formula states to observe variable(s), which have not been defined earlier.")
      }
    cols = paste0(".player_",call.players)

    # get rows in original df
    mdf = left_join(rdf,df, by=vars)
    rows = mdf$.ROW.FOR.PLAYER

    # Set all found players to TRUE
    df[rows,cols] = TRUE

    # Set player just to first player
    # if an action is chosen, there
    # must be a unique player in the stage
    df[rows,".player"] = players[1]
  }
  return(df)
}


add.util.cols = function(df, util.funs, players=NULL) {
  if (is.null(players)) {
    numPlayers = str.starts.with(colnames(play),"payoff_")
    players = 1:numPlayers
  }
  for (i in players) {
    util.fun = util.funs[[min(i, length(util.funs))]]
    if (is.character(util.fun)) util.fun = parse(text=util.fun)
    col = paste0("util_",i)
    df[[col]] = eval.on.df(util.fun, df)
  }
  df
}

vg.play.expected.utility = function(play, util.funs=NULL, numPlayers = sum(str.starts.with(colnames(play),"payoff_")), players=1:numPlayers) {


  if (is.null(util.funs)) {
    util.cols = paste0("payoff_",players)
  } else {
    play = add.util.cols(play, util.funs, players=players)
    util.cols = paste0("util_",players)
  }

  utils = unlist(lapply(util.cols, function(col) {
    sum(play$.prob*play[[col]])

  }))
  names(utils) = util.cols
  utils
}


#' Replace actions by transformations described by rules
#'
#'
#' If we have a rule for a stage all actions with the rule variable
#' will be replaced. There can be at most one rule for each
#' stage-action pair
#'
#' Note that only rules that specify a stage are considered.
set.vg.rules = function(vg, rules, warn=TRUE) {
  restore.point("set.vg.rules")

  rule = rules[[1]]
  for (rule in rules) {
    if (is.null(rule$stage)) {
      if (warn) warning(paste0("Rule for action ", rule$var, " does not specify a stage and is ignored."))
      next
    }
    stage = vg$stages[[rule$stage]]
    if (is.null(stage)) stop("Stage ", rule$stage, " does not exist in the vg.")

    if (length(stage$nature)>0) stop(paste0("Unfortunately, it is not possible to replace an action by a rule in a stage that has move of natures (here stage ", stage$name,"). Please change your game such that you add an additional stage before the current stage, where you specify the move of nature."))

    action.ind = match(rule$var, names(stage$actions))
    stage$actions = stage$actions[-action.ind]
    comp.add = list(list(name=rule$var, formula=rule$formula, tables=rule$tables))
    names(comp.add) = rule$var
    stage$compute = c(stage$compute, comp.add)
    vg$stages[[rule$stage]] = stage
  }
  vg
}
