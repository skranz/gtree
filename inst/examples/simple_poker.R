library(gtree)


pl2_card_table = data_frame(
  pl1_card = c(1,1,2,2,3,3),
  pl2_card = c(2,3,1,3,1,2),
  .prob = 0.5
)

game = org.game =  new_game(
  gameId = "Simplest-Poker",
  params = list(numPlayers=2),
  options = make_game_options(verbose=TRUE),
  stages = list(
    stage("dealCards",
      nature = list(
        # Player 1 gets a random card 1, 2, or 3
        natureMove("pl1_card", 1:3, fixed=2),
        # Draw from remaining cards for player 2
        natureMove("pl2_card", table = pl2_card_table)
      )
    ),
    stage("pl1Stage",
      player=1,
      observe = "pl1_card",
      actions = list(
        action("a1",c("drop","bet"))
      )
    ),
    stage("PayoffStage",
      compute=list(
        payoff_1 ~ (a1=="bet")*ifelse(pl1_card>pl2_card,1,-1)+0+0+0+0+0+0+0+0+0+0+0+0+0+0+0+0,
        payoff_2 ~ (a1=="bet")*ifelse(pl1_card>pl2_card,-1,1)
      )
    )
  )
)

fgame = org.game %>%
  game_copy() %>%
  game_fix_actions(a1="bet") %>%
  print



game %>%
  game_solve(mixed=TRUE) %>%
  eq_tables()

eo = eq_expected_outcomes(game)
eo
game
