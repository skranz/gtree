example.eval.set.on.df = function() {
  call = quote(1:cake)
  T = 100000
  df = data.frame(a=1:T, cake=sample.int(6,T, replace=TRUE))
  res = eval.set.to.df(call, df,"x") %>% arrange(a)

}

unnest_ = function(df, var, ...) {
  code = paste0("unnest(df,",var,")")
  eval(parse(text=code))
}

eval.set.to.df = function(call, df, var, expand=TRUE, params=NULL) {
  restore.point("my.eval.set.on.df")

  if (!is(call,"call")) {
    set = as.vector(call)
    df[[var]] = replicate(NROW(df),set,simplify = FALSE)
    if (!expand) return(df)
    return(unnest_(df,var))
  }
  #df$.ORG.ROW = seq.int(NROW(df))

  if (length(params)>0)
    call = substitute.call(call, params)

  # reduce df to unique combination of used variables
  vars = find.variables(call)
  if (length(vars)==0) {
    set = eval(call)
    df[[var]] = replicate(NROW(df),set,simplify = FALSE)
    if (expand)
      df = unnest_(df,var)
    return(df)
  }

  sdf = as_tibble(unique(df[,vars,drop=FALSE]))

  # compute set for each row of df
  sets = lapply(seq.int(NROW(sdf)), function(i) {
    eval(call,sdf[i,,drop=FALSE])
  })

  sdf[[var]] = sets

  if (expand) {
    sdf = unnest_(sdf,var)
  }
  res = right_join(df,sdf,by=vars)
  res
}

# Eval a call on a data.frame df
#
# params can be a list of parameters (each of length 1)
# that will be substituted into the call
# before the evaluation takes place
eval.on.df = function(call, df, params=NULL) {
  if (length(params)>0)
    call = substitute.call(call, params)

  eval(call, df)
}


eval.key.tables.to.df = function(df, tables, rows=NULL,...) {
  restore.point("eval.key.tables.on.df")
  for (table in tables) {
    df = eval.key.table.to.df(df, table, rows=rows,...)
  }
  return(df)
}

# The last column of table is the variable that shall be assigned to df
#
# The other columns (possible none) are keys that are used to match the rows
# of df, where the table variable shall be assigned to.
eval.key.table.to.df = function(df, table, var = colnames(table)[NCOL(table)], rows=NULL) {
  restore.point("eval.key.table.on.df")
  if (!is.null(rows))
    stop("table rows with condition are not yet implemented.")
  if (!has.col(df,var))
    df[[var]] = NA

  keys = setdiff(colnames(table), var)
  if (is.null(rows)) {
    if (length(keys)==0) {
      df[[var]] = table[[var]][1]
      return(df)
    } else if (length(keys)==1) {
      tab.rows = match(df[[keys]], table[[keys]])
    } else {
      df.id = paste.matrix.cols(df, keys)
      table.id = paste.matrix.cols(table, keys)
      tab.rows = match(df.id, table.id)
    }
    use.rows = !is.na(tab.rows)
    df[[var]][use.rows] = table[[var]][ tab.rows[use.rows] ]
  } else {
    if (length(keys)==0) {
      df[rows,var] = table[[var]][1]
      return(df)
    } else if (length(keys)==1) {
      tab.rows = match(df[[keys]][rows], table[[keys]])
    } else {
      df.id = paste.matrix.cols(df, keys)[rows]
      table.id = paste.matrix.cols(table, keys)
      tab.rows = match(df.id, table.id)
    }
    use.rows = !is.na(tab.rows)
    df[[var]][rows[use.rows]] = table[[var]][ tab.rows[use.rows] ]
  }


  return(df)
}
