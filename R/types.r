# Functionality to analyse equilibria with multiple preference types

examples.weakly.dominated = function() {
  library(gtreeCore)
  vg = new.vg(
    gameId = "MiniUG",
    params = list(numPlayers=2),
    stages = list(
      stage("nonceStage",
        nature = list(natureMove("nonce", 0:1))
      ),
      stage("proposerStage",
        player=1,
        actions = list(
          action("offer",c(1,50))
        )
      ),
      stage("responderStage",
        player=2,
        observe = "offer",
        actions = list(
          action("accept",c(FALSE,TRUE))
        )
      ),
      stage("PayoffStage",
        player=1:2,
        compute=list(
          payoff_1 ~ ifelse(accept, 100-offer,0),
          payoff_2 ~ ifelse(accept, offer,0)
        )
      )
    )
  )
  vg
  tg = vg.to.tg(vg)
  tg
  eq.li = gtree.solve.spe(tg, keep.weakly.dominated = FALSE)
  length(eq.li)
  eq.li.expected.outcomes(eq.li,tg)
  eq.li.tables(eq.li, tg)
}

examples.types.eq = function() {


  library(gtree)
  vg = new.vg(
    gameId = "MiniUG",
    params = list(numPlayers=2),
    stages = list(
      stage("proposerStage",
        player=1,
        actions = list(
          action("offer",c(1,50))
        )
      ),
      stage("responderStage",
        player=2,
        observe = "offer",
        actions = list(
          action("accept",c(FALSE,TRUE))
        )
      ),
      stage("PayoffStage",
        player=1:2,
        compute=list(
          payoff_1 ~ ifelse(accept, 100-offer,0),
          payoff_2 ~ ifelse(accept, offer,0)
        )
      )
    )
  )
  vg = vg.add.types(vg,typeProbs = c(1/3,1/3,1/3), typeNames=c("selfish","altru","reci"))
  vg

  tg = vg.to.tg(vg)
  tg
  utils = selfAltruReciTypesUtil(1:2, altru=2, reci=2)
  set.tg.util(tg,utils )
  oco.df = tg$oco.df

  eq.li = gtree.solve.spe(tg, keep.weakly.dominated = FALSE)
  eq.li.outcomes(eq.li, tg)
  eq.li.tables(eq.li,tg,keep.keys = list(offer="typeName_1",accept=c("typeName_2","offer")))

}

game_add_types = function(game, typeProbs =c(1/2,1/2), typeNames=NULL) {
  game$vg = vg.add.types(game$vg, typeProbs, typeNames)
  game = clear.non.vg(game)
  game
}

vg.add.types = function(vg, typeProbs=c(1/2,1/2), typeNames=NULL) {
  restore.point("vg.add.types")

  numTypes = length(typeProbs)

  vg$params$numTypes = numTypes
  n = vg$params$numPlayers

  sel.stage = stage("typeSelectionStage",
    nature = lapply(1:n, function(i) {
      natureMove(paste0("typeInd_",i),1:numTypes, typeProbs)
    })
  )



  # Type observation stages
  obs.stages = lapply(1:vg$params$numPlayers, function(i) {
    s = stage(paste0("typeObservationStage", i),
      player = i,
      observe = paste0("typeInd_",i)
    )
    if (!is.null(typeNames)) {
      var = paste0("typeName_",i)
      typeNameFormula = substitute(integer.to.label(typeInd,typeNames), list(typeNames=typeNames, typeInd = as.name(paste0("typeInd_",i))))
      s$compute = list(list(
        name=var,
        formula = typeNameFormula
      ))
      names(s$compute) = var
    }
    s
  })

  add.stages = name.by.name(c(list(sel.stage), obs.stages))

  # Add typeName as last transformation
  if (FALSE & !is.null(typeNames)) {
    ns = length(vg$stages)
    vg$stages[[ns]]$compute = c(vg$stages[[ns]]$compute,list(typeName=comp))
  }

  if (!is.null(vg$stage$typeSelectionStage)) {
    vg$stages[names(add.stages)] = add.stages
  } else {
    vg$stages = c(add.stages,vg$stages)
  }
  vg
}

integer.to.label = function(x, labels) {
  labels[x]
}

# Selfish Reciprocal Type Utility for 2 player games
selfishReciprocalTypesUtil = function(player = 1,altru = 1, reci=-1) {
  np = length(player)
  util = lapply(player, function(i) {
    j = 3-i
    subst = list(
      payoff_i = as.name(paste0("payoff_",i)),
      payoff_j = as.name(paste0("payoff_",j)),
      typeInd_i = as.name(paste0("typeInd_",i)),
      typeInd_j = as.name(paste0("typeInd_",j)),
      reci = reci,
      altru = altru
    )

    formula = substitute(payoff_i + cases(
      typeInd_i == 2 & typeInd_j==1, - (reci * payoff_j),
      typeInd_i == 2 & typeInd_j==2, altru * payoff_j,
      0
    ),subst)
    formula
  })
  lab = paste0("selfishReciprocalTypes",altru*100,"_", reci*100)
  names(util)=rep(lab,np)
  attr(util, "util.param") = list(util.type = "selfishReciprocalTypes", altru=altru, reci=reci)
  util
}

# Selfish Reciprocal Type Utility for 2 player games
selfAltruReciTypesUtil = function(player = 1,altru = 1, reci=-1) {
  np = length(player)
  util = lapply(player, function(i) {
    j = 3-i
    subst = list(
      payoff_i = as.name(paste0("payoff_",i)),
      payoff_j = as.name(paste0("payoff_",j)),
      typeInd_i = as.name(paste0("typeInd_",i)),
      typeInd_j = as.name(paste0("typeInd_",j)),
      reci = reci,
      altru = altru
    )

    formula = substitute(payoff_i + cases(
      typeInd_i == 2, altru * payoff_j,
      typeInd_i == 3 & typeInd_j != 1, altru * payoff_j,
      typeInd_i == 3 & typeInd_j==1, - (reci * payoff_j),
      0
    ),subst)
    formula
  })
  lab = paste0("selfishReciprocalTypes",altru*100,"_", reci*100)
  names(util)=rep(lab,np)
  attr(util, "util.param") = list(util.type = "selfishReciprocalTypes", altru=altru, reci=reci)
  util
}
