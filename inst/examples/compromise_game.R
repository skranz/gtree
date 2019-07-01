# Model in gtree the compromise game as described in
#
# Carrillo, Juan D.	Palfrey, Thomas R. (2009),	"The Compromise Game: Two-Sided Adverse Selection in the Laboratory", American Economic Journal: Microeconomics
#
# https://www.aeaweb.org/articles?id=10.1257/mic.1.1.151

library(gtree)
numSteps = 3
game = new_game(
  gameId = "CompromiseGame",
  params = list(numPlayers=2, H=1, M=0.5, L=0, numSteps=numSteps, eps.fight = 0.001),
  options = make_game_options(verbose=TRUE),
  stages = list(
    stage("drawPower",
      nature = list(
        # Player 1
        natureMove("power1", seq(0,1,length = numSteps)),
        # Draw from remaining power for player 2
        # avoids ties
        natureMove("power2", ~setdiff(seq(0,1,length = numSteps),power1))
      )
    ),
    stage("pl1Stage",
      player=1,
      observe = "power1",
      actions = list(
        action("a1",c("retreat","fight"))
      )
    ),
    stage("pl2Stage",
      player=2,
      observe = c("power2","a1"),
      actions = list(
        action("a2",c("retreat","fight"))
      )
    ),
    stage("PayoffStage",

      compute=list(
        compromise ~ (a1=="retreat" & a2=="retreat"),
        payoff_1 ~ eps.fight*(a1=="fight")+case_distinction(
          compromise, M,
          !compromise & power1 > power2, H,
          L
        ),
        payoff_2 ~ eps.fight*(a2=="fight")+case_distinction(
          compromise, M,
          !compromise & power2 > power1, H,
          L
        )
      )
    )
  )
)

game %>%
  game_gambit_solve(mixed=TRUE) %>%
  eq_tables()

game %>%
  game_set_preferences(pref_payoff()) %>%
  game_gambit_solve(qre.lambda=8) %>%
  eq_tables()

game %>%
  game_set_preferences(pref_ineqAv(alpha=0.5, beta=0)) %>%
  game_gambit_solve(qre.lambda=8) %>%
  eq_tables()

  game_solve() %>%
  eq_expected_outcomes()

game %>%
  eq_tables()

game %>%
  eq_expected_outcomes() -> eo

# Consider inequality averse players
game %>%
  game_set_preferences(pref_ineqAv(alpha=2.1, beta=1.5)) %>%
  game_solve() %>%
  eq_tables()

# Consider inequality averse players
game %>%
  game_set_preferences(pref_ineqAv(alpha=0.99, beta=0.49)) %>%
  game_gambit_solve(qre.lambda = 2) %>%
  eq_tables()



eo = eq_expected_outcomes(game)
eo
game
