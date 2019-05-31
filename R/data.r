# Example

example.data.import = function() {
  library(gtreeCore)
  setwd("D:/libraries/gtree/work")
  set.project.dir("D:/libraries/gtree/myproject")
  gameId="LureOfAuthorityReduced"
  rg = get.rg(gameId = gameId)
  tg = get.tg(gameId = gameId)
  vg = get.vg(gameId = gameId)
  variants = rg$variants
  oco.df = tg$oco.df

  odat = foreign::read.dta("D:/libraries/gtree/source/LureOfAuthority/LureofAuthority.dta")

  dat = odat %>%
    filter(treatmentnumber == "Group") %>%
    mutate(variant = tolower(alignment)) %>%
    select(-(HIGH:PHIGH50)) %>%
    select(variant, everything())

  readr::write_excel_csv(dat,"lure_data.csv")


  dat = dat %>%
    select(variant, everything()) %>%
    filter(variant %in% variants)

  #readr::write_excel_csv(dest,"lure_data.csv")

  dat = dat %>%
    filter(treatmentnumber=="Group") %>%
    arrange(Session, group, period)

  unique(dat$type)

  # Merge agent and principal rows
  adat = filter(dat, type=="Agent")
  pdat = filter(dat, type=="Principal")

  mdat = pdat %>%
    rename(pinformed=informed) %>%
    select(-searchtransfer,-searchnotransfer,-search_right, -search_noright) %>%
    left_join(select(adat,Session,group,period, ainformed=informed,searchtransfer, searchnotransfer,search_right, search_noright), by=c("Session","group","period"))




  #gtreeDataMapping(dest,tg)


  gdat = transmute(mdat,
    variant = variant,
  	delegate = recode(delegation,"delegated"="del" ,"kept" = "nodel"),
  	searchP = searchprincipal,
  	searchA__del = ifelse(!is.na(search_right), search_right, searchagent),
    searchA__nodel = ifelse(!is.na(search_noright), search_noright, searchagent),
  	informedP = recode2(pinformed,TRUE~"yes",FALSE~"no"),
  	informedA = recode2(ainformed,TRUE~"yes",FALSE~"no")
  )

  rdat = get.all.variables.from.moves(gdat,gameId=gameId)
}

gtreeDataMapping = function(data,tg=get.tg(gameId,...),vg=get.vg(gameId=gameId,...), gameId=tg$gameId,...) {
  library(shinyDataMapping)
  move.vars = c("variant", move.names.inclusive.strat.method(tg,vg))
  # No strategy method domain
  base.vars = str.left.of(move.vars, "__")

  dest = oco.df[,base.vars]
  colnames(dest) = move.vars
  shinyDataMapping(source=data, dest=dest)
}



gtree.mutate.code = function(oco.df = tg$oco.df,tg=get.tg(...),...) {
  cols = colnames(oco.df)
  cols = setdiff(cols, "numPlayers")
  ignore = str.starts.with(cols,".") | str.starts.with(cols, "util_")
  cols = cols[!ignore]

  oco.df = oco.df[,cols, drop=FALSE]

  is.char = sapply(oco.df, is.character)
  code = paste0("\t",cols, " = ", cols)
  for (col in which(is.char)) {
    vals = unique(oco.df[[col]])
    code[col] = paste0("\t",cols[col], " = remap(", cols[col],",",
      paste0('"',vals,'"~"',vals,'"', collapse=", "),")")
  }
  res = paste0("mutate(\n", paste0(code, collapse=",\n"),"\n)")
  writeClipboard(res)
  cat(res)
  invisible(res)
}

recode2 = function(values, ...) {
  maps = list(...)
  restore.point("recode2")

  if (length(maps)==0) return(values)

  # Check classes
  m = maps[[1]]
  dest.class = class(m[[2]])
  # Switch in classes
  if (!is(values,dest.class)) {
    org.values = values
    values = as(values, dest.class)
    for (m in maps) {
      lhs = m[[2]]
      rhs = m[[3]]
      values[org.values==rhs] = lhs
    }
  # Same class
  } else {
    for (m in maps) {
      lhs = m[[2]]
      rhs = m[[3]]
      values[values==rhs] = lhs
    }
  }
  values
}

dummies.to.factor = function(data, dummy.cols=NULL) {
  restore.point("dummies.to.factors")
  if (!is.null(dummy.cols)) {
    data = data[,dummy.cols, drop=FALSE]
  }
  col = max.col(data)
  colnames(data)[col]
}

get.all.variables.from.moves = function(data, variants= if (!is.null(tg)) tg$variant else unique(data$variant), tg=NULL, tg.li=NULL, gameId=NULL,...) {
  restore.point("get.all.variables.from.moves")
  if (!is.null(tg) & length(variants)>1) {
    stop("You provide a single tg object but specified several variants.")
  }
  loc.tg = tg
  #stop()
  .variant = variants[[1]]
  li = lapply(variants, function(.variant) {
    df = filter(data, variant==.variant)
    if (is.null(tg)) {
      if (!is.null(tg.li)) {
        loc.tg = tg.li[[.variant]]
      } else {
        #loc.tg = get.tg(variant=.variant,gameId=gameId,...)
        loc.tg = get.tg(variant=.variant,gameId=gameId)

      }
    }
    move.vars = game.move.vars(loc.tg)
    oco.df = loc.tg$oco.df

    # Adapt for strategy method
    smd = game.strat.method.domain.vars(gameId=gameId, variant=.variant)

    row = 1
    for (row in seq_along(NROW(smd))) {
      domainvar = smd$domainvar[row]
      action = smd$action[row]
      if (!domainvar %in% colnames(data)) {
        stop(paste0("Your data set is missing the variable '",domainvar,"', that specifies the strategy-method domain for the action '", action,"'."))
      }
      action_col = paste0(action,"__", df[[domainvar]])
      action_col = match(action_col,colnames(df))
      df[[action]] = df[cbind(seq_len(NROW(df)),action_col)]
      class(df[[action]]) = class(oco.df[[action]])
    }

    cols = unique(c("variant", move.vars, setdiff(colnames(df), colnames(oco.df))))
    df = df[,cols]
    ndat = left_join(df, oco.df, by=c("variant",move.vars))
    ndat
  })
  bind_rows(li)
}


game.var.set = function(var, tg) {
  unique(tg$oco.df[[var]])
}

game.move.vars = function(tg) {
  tg$lev.vars
}


game.strat.method.domain.vars = function(vg=get.vg(variant=variant,gameId=gameId,...), gameId=NULL, variant=NULL,...) {
  stage = vg$stages[[1]]
  res = bind_rows(lapply(vg$stages, function(stage){
    bind_rows(lapply(stage$actions, function(action) {
      if (!is.empty(action$strategyMethodDomain))
        as_data_frame(list(action=action$name, domainvar=action$strategyMethodDomain))
    }))
  }))
  unique(res)
}

move.names.inclusive.strat.method = function(tg=get.tg(gameId,...), vg=get.vg(gameId,...), gameId=tg$gameId,...) {
  restore.point("move.names.inclusive.strat.method")

  stage = vg$stages[[1]]
  moves = unique(unlist(lapply(vg$stages, function(stage){
    actions = lapply(stage$actions, function(action) {
      if (is.empty(action$strategyMethodDomain))
        return(action$name)
      domain = game.var.set(action$strategyMethodDomain,tg=tg)
      return(paste0(action$name,"__", domain))
    })
    c(names(stage$nature),actions)
  })))
  moves
}

