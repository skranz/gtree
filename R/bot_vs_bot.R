# Against the population experiment
# Only a single player plays against a bot
# The bot may randomly draw from actions of the pool

bot_vs_bot.example = function() {
  library(gtree)
  game = new_game(
    gameId = "UltimatumGame",
    options = make_game_options(verbose=TRUE),
    params = list(numPlayers=2,cake=4),
    stages = list(
      stage("proposerStage",
        player=1,
        actions = list(
          action("offer",~0:cake)
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
          payoff_1 ~ ifelse(accept, cake-offer,0),
          payoff_2 ~ ifelse(accept, offer,0)
        )
      )
    )
  )

  library(gtree)
  game = org.game = new_game(
  gameId = "Kuhn-Poker",
  params = list(numPlayers=2),
  options = make_game_options(verbose=FALSE),
  stages = list(
    stage("dealCards",
      nature = list(
        # Player 1 gets a random card 1, 2, or 3
        natureMove("card1", 1:3),
        # Draw from remaining cards for player 2
        natureMove("card2", ~setdiff(1:3, card1))
      )
    ),
    stage("pl1CheckBet",
      player=1,
      observe = "card1",
      actions = list(
        action("cb1",c("check","bet"))
      )
    ),
    stage("pl2CheckBet",
      player=2,
      condition = ~ cb1 == "check",
      observe = c("card2","cb1"),
      actions = list(
        action("cb2",c("check","bet"))
      )
    ),
    stage("pl2FoldCall",
      player=2,
      condition = ~ cb1 == "bet",
      observe = c("card2","cb1"),
      actions = list(
        action("fc2",c("fold","call"))
      )
    ),
    stage("pl1FoldCall",
      player=1,
      condition = ~ is_true(cb1 == "check" & cb2=="bet"),
      observe = "cb2",
      actions = list(
        action("fc1",c("fold","call"))
      )
    ),
    stage("PayoffStage",
      player=1:2,
      compute=list(
        # Which player folds?
        folder ~ case_distinction(
          is_true(fc1 == "fold"),1,
          is_true(fc2 == "fold"),2,
          0 # 0 means no player folds
        ),

        # Which player wins?
        winner ~ case_distinction(
          folder == 1,2,
          folder == 2,1,
          folder == 0, (card2 > card1) +1
        ),

        # How much gave each player to the pot?
        gave1 ~ 1 + 1*is_true((cb1 == "bet") | (fc1 == "call")),
        gave2 ~ 1 + 1*is_true((cb2 == "bet") | (fc2 == "call")),
        pot ~ gave1 + gave2,

        # Final payoffs
        payoff_1 ~ (winner == 1)*pot - gave1,
        payoff_2 ~ (winner == 2)*pot - gave2
      )
    )
  )
  )
  game %>%
#    game_set_preferences(pref_envy(alpha=0)) %>%
    game_solve(mixed=TRUE) %>%
    eq_tables()

  # TO DO: Correct eq_tables
  tables = eq_tables(game)

  #bots = list(bot_random(game,1), bot_eq(game,2))
  bots = make_bots(game, bot_tables, tables=tables)

  play_bot_vs_bot(game, bots)

  disable.restore.points()
  sim = bind_rows(replicate(100, play_bot_vs_bot(game, bots),simplify = FALSE))
}

#' Simulate one play of the game
#'
#' @param game the game object
#' @param bots a list containing one bot per player
#' @param return.play.object By default only the outcome of the play as a one-row data frame is returned. If you set \code{return.play.object} an internal \code{play} object will be returned with more detailed information about the simulation run
#' @family Bots
#' @family Simulate

play_bot_vs_bot = function(game, bots, return.play.object = FALSE) {
  play = new_play(game=game,bots=bots, human=0)
  while(play$auto.stage.finished < length(game$vg$stages)) {
    play = play_stage_auto(play)
  }
  if (return.play) return(play)
  as_tibble(play$hist)
}

new_play = function(game, bots, human=NULL) {
  play = list(
    hist = game$vg$params,
    auto.stage.finished = 0,
    human.stage.finished = 0,
    game = game,
    bots = bots,
    human = human,
    bot.player = setdiff(game$players,human)
  )
  play
}



play_stage_auto = function(play) {
  restore.point("play_stage_auto")
  stage.num = play$auto.stage.finished+1

  vg = play$game$vg
  stage = vg$stages[[stage.num]]
  env = play$hist

  play$stage.finished = FALSE
  # Check condition
  if (!is.empty(stage$condition)) {
    cond = eval.or.return(stage$condition,env)
    if (!cond) {
      na.vars = setdiff(c(names(stage$actions), names(stage$nature), names(stage$compute)), names(play$hist))
      play$hist[na.vars] = lapply(na.vars, function(var) NA)

      play$auto.stage.finished = play$auto.stage.finished+1
      return(play)
    }
  }

  # Draw moves of nature
  for (rv in stage$nature) {
    set = eval.or.return(rv$set,env)
    probs =  eval.or.return(rv$probs, env)
    if (is.null(probs)) {
      val = sample(set,1)
    } else {
      val = sample(set,1,prob=probs)
    }
    env[[rv$name]] = val
  }

  # Deterministic transformations
  for (comp in stage$compute) {
    env[[comp$name]] = eval.or.return(comp$formula, env)
  }

  # Determine players
  play$hist = env
  player = eval.or.return(stage$player, play$hist)
  play$is.human.stage = does.intersect(player, play$human)


  bot.player = intersect(play$bot.player, player)
  if (length(bot.player==1))  {
    bot  = play$bots[[bot.player]]
    for (action in stage$actions) {
      set = eval.or.return(action$set,env)
      val = do.call(bot$choose_action, c(list(play=play, action=action, stage=stage, set=set), bot$extra.arg))
      #val = bot$choose_action(play=play, action=action, stage=stage, set=set)
      if (!isTRUE(val %in% set)) {
        stop(paste0("The bot '", bot$name, "' picked an infeasible value (", val, ") for the action '", action$name,"'"))
      }
      play$hist[[action$name]] = val
    }
  }

  play$auto.stage.finished = play$auto.stage.finished+1
  return(play)
}

