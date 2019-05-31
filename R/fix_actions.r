# We take a tg game and can fix some actions
#
# We transform these actions into moves of nature
# with specified move probabilities that can depend
# on specific values of previous variables

# Applications:
#
# 1. Find best-reply against data: Fix moves
#    of all but one player to the probabilities
#    coming from a data set.
#
# 2. Check for best replies. Fix some strategies
#    to their proposed equilibrium values.
#    Now let us find best replies against them
#    Can be used to test equilibrium computations.
#
# 3. Compute level-k equilibria
#
# 4. Part of analyzing norm equilibria in a game.


example.fix.actions = function() {
  setwd("D:/libraries/gtree/myproject")
	gameId = "UltimatumGame"
	gameId = "UG2"
	tg = get.tg(gameId = gameId, never.load = !FALSE)

	fix.df = data_frame(offer=c(0:1), accept=0)
  fix.df2 = data_frame(offer=3, accept=1)
  fix.li = list(fix.df, fix.df2)

  tg.fix = fix.tg.actions(tg, fix.li=fix.li)
  tg.fix$tg.id
  lev.li = tg.fix$lev.li


  save.tg(tg.fix)

  efg = tg.to.efg(tg.fix)

  eq.li = get.eq(tg.fix)
  eeo = expected.eq.outcomes(eq.li=eq.li, tg=tg.fix)
}

fix.tg.actions = function(tg, fix.df=NULL, var=NULL, fix.li=NULL, tg.id = paste0("fixed",tg$tg.id), omit.zero.prob=TRUE, tremble.prob=0) {
  restore.point("fix.tg.actions")

  # Make copy of tg
  old.tg = tg
  tg = as.environment(as.list(tg))

  # Compute lev.li in which actions are transformed to moves of nature
  # as specified by fix.df
  li = lapply(tg$lev.li,function(lev) {
    lev.action.to.nature(lev=lev, fix.df=fix.df, var=var, fix.li=fix.li, omit.zero.prob = omit.zero.prob, tremble.prob=tremble.prob, params=tg$params)
  })
  lev.li = do.call(c, li)

  # Perform all necessary auxilliary computations to set the new lev.li
  # Information sets, oco.df, et.mat etc
  tg = set.new.tg.lev.li(tg, lev.li)

  # Need to set tg.id at bottom
  # since set.new.tg.lev.li will set automatic
  # tg.id
  tg$tg.id = tg.id

  tg
}


# Set a modified lev.li for a tg
# and updated other required parameters
set.new.tg.lev.li = function(tg,lev.li, transformations=tg$transformations, add.sg = FALSE, add.spi=FALSE, add.spo=FALSE) {
  restore.point("set.new.tg.lev.li")

  tg = as.environment(as.list(tg))

  tg$ok = FALSE
  # 1. a) Correctly set node and information set indices
  #       in lev.li.
  #    b) Compute .row.1, .row.2 etc
  #    c) Compute stage.df

  acols = cols = c(".node.ind",".info.set.move.ind",".info.set.ind")
  ncols = c(".node.ind")
  ind = rep(0, length(cols))
  names(ind) = cols

  lev.num = 0

  stage.df = NULL
  prev.vars = NULL
  while (lev.num < length(lev.li)) {
    lev.num = lev.num+1
    lev = lev.li[[lev.num]]
    lev.df = lev$lev.df

    # Set .node.ind etc
    cur.cols = if(lev$type=="action") acols else ncols
    for (col in cur.cols) {
      uni = unique(lev.df[[col]])
      lev.df[[col]] = match(lev.df[[col]], uni) + ind[col]
      ind[col] = ind[col] + length(uni)
    }


    # create .row.1., .row.2, ... columns
    # that link lev.df to the rows in earlier lev.df
    lev.df[[paste0(".row.",lev.num)]] = seq_len(NROW(lev.df))
    if (lev.num == 1) {
      if (lev$type=="nature") {
        lev.df$.prob = lev.df$.move.prob
      } else {
        lev.df$.prob = 1
      }
      stage.df = lev.df
    } else if (lev.num > 1) {
      # Join stage.df with current lev.df

      # Don't join on current action var
      join.cols = setdiff(prev.vars, lev$var) %>%
        intersect(colnames(lev.df))


      ommited.stage.df = anti_join(stage.df, lev.df, by=join.cols)
      used.stage.df = semi_join(stage.df, lev.df, by=join.cols)

      left.df = select(lev.df, - .prob)

      # Columns that will be added from stage.df to lev.df
      stage.cols = c(join.cols,setdiff(colnames(used.stage.df), colnames(left.df)))
      stage.cols = stage.cols[(!str.starts.with(stage.cols,".")) | str.starts.with(stage.cols, ".row.")]
      stage.cols = unique(c(stage.cols, ".prob"))

      right.df = used.stage.df[, stage.cols]
      lev.df = left_join(left.df,right.df, by=join.cols)
      if (lev$type=="nature") {
        lev.df$.prob = lev.df$.prob * lev.df$.move.prob
      }
      stage.df = bind_rows(lev.df, ommited.stage.df)
    }


    lev$lev.num = lev.num
    lev$lev.df = lev.df

    lev.li[[lev.num]] = lev
    prev.vars = unique(c(prev.vars,lev$var))
  }

  # Add all transformations
  for (tr in transformations) {
    # Formula applies to all rows
    if (is.null(tr$cond)) {
      if (is.null(tr$tables)) {
        stage.df[[tr$var]] = eval.on.df(call = tr$formula,stage.df, params=tg$params)
      } else {
        stage.df = eval.key.tables.to.df(stage.df, tr$tables, var=tr$var)
      }
    # Transformation applies only to a subset of rows
    } else {
      rows = eval.on.df(tr$cond, stage.df, params=tg$params)
      if (!tr$var %in% colnames(stage.df))
        stage.df[[tr$var]] = NA
      if (is.null(tr$tables)) {
        stage.df[rows,tr$var] = eval.on.df(tr$cond, stage.df[rows,,drop=FALSE], params=tg$params)
      } else {
        stage.df = eval.key.tables.to.df(stage.df, tr$tables, var=tr$var, rows=rows)
      }

    }
  }

  restore.point("set.new.tg.lev.li2")

  tg$lev.li = lev.li
  tg$stage.df = stage.df
  tg$lev.vars = prev.vars

  # 2. Compute oco.df, ise.df etc
 	# compute et.mat, oco and other variables...
 	compute.tg.et.oco.etc(tg)

  # know.var groups help to compute iso.df
  # later on
 	make.tg.know.var.groups(tg)
  make.tg.ise.df(tg)

  # Set previous utility function
  set.tg.util(tg=tg, util.funs = tg$util.funs)

  if (add.sg) {
  	compute.tg.subgames(tg)
  } else {
    clear.tg.subgames(tg)
  }
	if (add.spi) {
		make.tg.spi.li(tg)
  } else {
    clear.tg.spi.li(tg)
  }
	if (add.spo) {
  	make.tg.spo.li(tg)
  } else {
    clear.tg.spo.li(tg)
  }

	tg$ok = TRUE
  return(tg)
}


lev.action.to.nature.fix.li = function(lev,fix.li=NULL, var = NULL,omit.zero.prob=TRUE, lev.li = NULL, tremble.prob=0, params=tg$params, tg=NULL, ...) {
  restore.point("lev.action.to.nature.fix.li")
  # We start with an action level
  act.lev = lev
  nat.lev = NULL
  fix.df = fix.li[[1]]
  for (fix.df in fix.li) {
    lev.li = lev.action.to.nature(act.lev, fix.df=fix.df, var=var, omit.zero.prob = omit.zero.prob, tremble.prob=tremble.prob, params=params)
    if (lev.li[[1]]$type == "action") {
      act.lev = lev.li[[1]]
      lev.li = lev.li[2]
    } else {
      act.lev = NULL
    }

    # Combine nature levels to reduce number of levels
    if (length(lev.li)>0) {
      nat.lev = merge.fix.actions.nat.levs(nat.lev, lev.li[[1]])
    }

    # All action nodes are transformed into a move of nature
    if (is.null(act.lev)) break
  }
  if (is.null(act.lev)) return(list(nat.lev))
  if (is.null(nat.lev)) return(list(act.lev))
  return(list(act.lev,nat.lev))
}



merge.fix.actions.nat.levs = function(lev1, lev2) {
  restore.point("merge.nat.levs")

  if (is.null(lev1)) return(lev2)
  if (is.null(lev2)) return(lev1)

  lev = lev1
  lev$lev.df = rbind(lev1$lev.df, lev2$lev.df)
  lev$know.li = lapply(seq_along(lev$know.li), function(player) {
    rbind(lev1$know.li[[player]],lev2$know.li[[player]])
  })
  return(lev)
}

# Transforms some or all nodes of an action level
# to moves of nature
#
# Does not adapt information set or node numbers
lev.action.to.nature = function(lev, fix.df,var = NULL,omit.zero.prob=TRUE, lev.li = NULL,fix.li=NULL,tremble.prob=0,params=NULL, tg=NULL, ...) {

  # Only actions can be fixed
  if (lev$type != "action")
    return(list(lev))

  # Vectorized version if fix.li or lev.li is given
  if (!is.null(fix.li))
    return(lev.action.to.nature.fix.li(lev, var=var, omit.zero.prob=omit.zero.prob, fix.li=fix.li, tremble.prob=tremble.prob))


  restore.point("lev.action.to.nature")

  # Called with a rule instead of a fix.df
  if (!is.data.frame(fix.df)) {
    if ("formula" %in% names(fix.df))
      return(lev.action.to.nature.by.rule(lev, rule=fix.df, tremble.prob=tremble.prob, params=params))
  }

  if (is.null(var))
    var = get.fix.df.var(fix.df)


  # A different action than the one fixed
  if (var!=lev$var)
    return(list(lev))

  lev.df = lev$lev.df
  cols = !str.starts.with(colnames(lev.df),".row.")
  lev.df = lev.df[,cols]
  lev.df$.ORG.ROW = seq_len(NROW(lev.df))

  by.cols = setdiff(intersect(colnames(lev.df), colnames(fix.df)),".move.prob")
  key.cols = setdiff(by.cols, var)

  # Nodes that remain as actions
  act.df = anti_join(lev.df, fix.df, by=key.cols)

  # Nodes that will become move of nature
  lev.df = semi_join(lev.df, fix.df, by=key.cols)

  # filter.df does not pick up any rows
  if (NROW(lev.df)==0) {
    return(list(lev))
  }


  # If fix.df assigns deterministic actions
  # specify it as a move of nature with probability 1
  fix.has.prob = ".move.prob" %in% colnames(fix.df)
  if (!fix.has.prob) {
    fix.df$.move.prob = 1
    if (!omit.zero.prob) {
      # Match to all values of var
      fix.df = left_join(lev.df[,by.cols], fix.df[,c(by.cols,".move.prob")], by=by.cols)
      # Set probability to 0 for non-played values of var
      fix.df$.prob[is.na(fix.df$.move.prob)] = 0
    }
  }


  # Create data frame for move of nature
  join.cols = setdiff(colnames(lev.df), c(".info.set.ind",".info.set.move.ind", ".info.set",".move.ind",".move.prob"))
  join.cols = join.cols[!str.starts.with(join.cols,".row.")]
  nat.df = fix.df %>% left_join(lev.df[, union(join.cols,by.cols)], by=by.cols)

  # Add move.ind based on .node.ind
  nat.df = nat.df %>% group_by(.node.ind) %>%
    mutate(.move.ind = seq_len(n())) %>%
    ungroup()

  # Order columns in original order
  cols = intersect(colnames(lev.df), colnames(nat.df)) %>%
      setdiff(c(".prob",".info.set.ind",".info.set.move.ind", ".info.set")) %>%
      c(".move.prob", ".prob")
  nat.df = nat.df[,cols]


  # Select original rows from know.li
  # This means the knowledge structure
  # is assumed not to change.
  #
  # The player who has chosen an action before
  # will also know the outcome of the move of nature
  #
  # That is for example neccessary when looking
  # at norm equilibria
  nat.know.li = lev$know.li
  if (NROW(nat.df) < NROW(lev.df)) {
    nat.know.li = lapply(nat.know.li, function(know.mat) {
      know.mat[nat.df$.ORG.ROW,,drop=FALSE]
    })
  }

  nat.lev = nlist(
    type="nature",
    var = var,
    lev.num = lev$lev.num,
    stage.num = lev$stage.num,
    player=0,
    lev.df=nat.df,
    know.li = nat.know.li
  )

  # All nodes where transformed to a move of nature
  if (NROW(act.df)==0) {
    return(list(nat.lev))
  }

  # Some nodes remain actions
  act.know.li = lapply(lev$know.li, function(know.mat) {
    know.mat[act.df$.ORG.ROW,,drop=FALSE]
  })
  act.lev = lev
  act.lev$lev.df = act.df
  act.lev$know.li = act.know.li

  # Return both new nature level
  # and an level for remaining action nodes
  return(list(act.lev, nat.lev))
}

get.fix.df.var = function(fix.df) {
  var = attr(fix.df,"fix.var")
  if (!is.null(var)) return(var)

  cols = setdiff(colnames(fix.df),".prob")
  return(cols[length(cols)])
}


# Transforms some or all nodes of an action level
# to moves of nature
#
# Does not adapt information set or node numbers
lev.action.to.nature.by.rule = function(lev, rule,omit.zero.prob=FALSE,tremble.prob = 0, params=tg$params, tg=NULL, ...) {
  # Only actions can be fixed
  if (lev$type != "action")
    return(list(lev))


  var = rule$var
  # A different action than the one fixed
  if (var!=lev$var)
    return(list(lev))

  restore.point("lev.action.to.nature.by.rule")

  lev.df = lev$lev.df
  cols = !str.starts.with(colnames(lev.df),".row.")
  lev.df = lev.df[,cols]
  lev.df$.ORG.ROW = seq_len(NROW(lev.df))

  if (!is.null(rule$condition)) {
    use.row = eval.on.df(rule$condition, lev.df, params=params)
  } else {
    use.row = rep(TRUE, NROW(lev.df))
  }

  # Nodes that remain as actions
  act.df = lev.df[!use.row,, drop=FALSE]

  # Nodes that will become move of nature
  lev.df = lev.df[use.row,, drop=FALSE]

  # rule does not pick up any rows
  if (NROW(lev.df)==0) {
    return(list(lev))
  }

  cols = setdiff(colnames(lev.df), c(".info.set.ind",".info.set.move.ind", ".info.set",".move.ind",".move.prob"))
  nat.df = lev.df[,cols]

  value = eval.on.df(rule$formula, lev.df, params=tg$params)
  nat.df$.move.prob = 0
  nat.df$.move.prob[nat.df[[var]] == value] = 1

  if (tremble.prob > 0) {
    nat.df = nat.df %>% group_by(.node.ind) %>%
      mutate(.move.prob = (1-tremble.prob)*.move.prob + tremble.prob / n())  %>%
      ungroup()
  }

  if (omit.zero.prob) {
    lev.df = filter(lev.df, .move.prob >0)
  }
  # Add move.ind based on .node.ind
  nat.df = nat.df %>% group_by(.node.ind) %>%
    mutate(.move.ind = seq_len(n())) %>%
    ungroup()

  # Order columns in original order
  cols = intersect(colnames(lev.df), colnames(nat.df)) %>%
      setdiff(c(".prob",".info.set.ind",".info.set.move.ind", ".info.set")) %>%
      c(".move.prob", ".prob")
  nat.df = nat.df[,cols]


  # Select original rows from know.li
  # This means the knowledge structure
  # is assumed not to change.
  #
  # The player who has chosen an action before
  # will also know the outcome of the move of nature
  #
  # That is for example neccessary when looking
  # at norm equilibria
  nat.know.li = lev$know.li
  if (NROW(nat.df) < NROW(lev.df)) {
    nat.know.li = lapply(nat.know.li, function(know.mat) {
      know.mat[nat.df$.ORG.ROW,,drop=FALSE]
    })
  }

  nat.lev = nlist(
    type="nature",
    var = var,
    lev.num = lev$lev.num,
    stage.num = lev$stage.num,
    player=0,
    lev.df=nat.df,
    know.li = nat.know.li
  )

  # All nodes where transformed to a move of nature
  if (NROW(act.df)==0) {
    return(list(nat.lev))
  }

  # Some nodes remain actions
  act.know.li = lapply(lev$know.li, function(know.mat) {
    know.mat[act.df$.ORG.ROW,,drop=FALSE]
  })
  act.lev = lev
  act.lev$lev.df = act.df
  act.lev$know.li = act.know.li

  # Return both new nature level
  # and an level for remaining action nodes
  return(list(act.lev, nat.lev))
}
