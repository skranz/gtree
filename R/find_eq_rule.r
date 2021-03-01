# Try to transform an equilibrium into a simple rule
#
# Almost nothing implemented yet.
# Not clear that we get large value from this route


example.table.rule = function() {
  setwd("D:/libraries/gtree/myproject")

  gameId = "UltimatumGame"
	tg = get.tg(gameId = gameId,never.load = FALSE)
  ise.df = tg$ise.df

	eq.li = gambit.solve.eq(tg)
  eq = eq.li[[1]]
  eq.tables(eq, tg)
  eq.table.rules(eq, tg)

}



# Specify the key variables for each action that can be played in a tg
make.tg.action.keys = function(tg) {
  actions = unique(tg$ise.df$.var)

}

example.find.perfect.predictor.cols = function() {
  T = 100
  dat = tibble(a=sample(0:1, T, replace = TRUE), b=runif(T,-1,1), c=b^2, y=(1-c)>0.5)
  df = select(df,b,y)
  is.perfect.predictor(df = select(dat,b,y))
  is.perfect.predictor(df = select(dat,c,y))

  is.monotone.predictor(df = select(dat,b,y))
  is.monotone.predictor(df = select(dat,c,y))

  accept = quote((payoff_2 - alpha*(payoff_1-payoff_2))>0)

}

find.perfect.predictor.cols = function(df, var) {
  restore.point("find.perfect.predictor.cols")

}

# Is a numeric x a monotone predictor for y
is.monotone.predictor = function(x,y, df = as_tibble(list(x=x,y=y))) {
  ord = order(df[[1]])
  df = df[ord,]
  is.highest = which(!is.true(lead(df[[2]]) == df[[2]]))
  n_distinct(df[[2]][is.highest]) == length(is.highest)
}
