# In RStudio you can directly press "Run App" best run it in the viewer pane

library(gtree)



# Laser beats rock, paper and scissors but is beaten by mirror
# Mirror only beats laser and is beaten by everything else
# Otherwise as in rock paper scissors
game = new_game(
  gameId = "RockPaperScissorsLaserMirror",
  options =make_game_options(verbose=FALSE),
  params = list(numPlayers=2),
  stages = list(
    stage("player1",
      player=1,
      actions = list(
        action("a1",set=c("r","p","s","l","m"))
      )
    ),
    stage("player2",
      player=2,
      actions = list(
        action("a2",set=c("r","p","s","l","m"))
      )
    ),
    stage("PayoffStage",
      compute=list(
        payoff_1 ~ -1+2*(
          (a1=="r" & a2 %in% c("s","m")) |
          (a1=="p" & a2 %in% c("r","m")) |
          (a1=="s" & a2 %in% c("p","m")) |
          (a1=="l" & a2 %in% c("r","p","s")) |
          (a1=="m" & a2=="l")
        ) + (a1==a2),
        #payoff_1 ~ pi1,
        payoff_2 ~ -payoff_1
      )
    )
  )
)

# Find one mixed equilibrium with "gambit-logit"
game %>%
  game_gambit_solve(mixed=TRUE) %>%
  eq_tables()

# Try to find more equilibria
game %>%
  game_gambit_solve("gambit-enummixed -q -d 12") %>%
  eq_tables()

# We seem to have found the unique equilibrium
