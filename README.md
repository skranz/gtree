# gtree: Game-Theoretic Representation of Economic Experiments

Author: Sebastian Kranz, Ulm University

[gtree](skranz.github.io/gtree) is an R package that allows you to specify extensive form games using stages, similar as one specifies economic experiments with [ztree](https://www.ztree.uzh.ch/en.html) or [otree](https://otree.readthedocs.io/en/latest/). The game is internally converted to a formal game tree and one can find its equilibria using a [Gambit](http://www.gambit-project.org/) solver or an internal solver. The equilibria are presented in a format that facilitates comparison with experimental results. One can also conveniently study the effects of non-standard preferences characterized e.g. by inequality aversion or loss aversion.

Detailed documentation can be found on [skranz.github.io/gtree](skranz.github.io/gtree).

## Install gtree

To install gtree will all R dependencies simply run
```r
install.packages("gtree",repos = c("https://skranz-repo.github.io/drat/",getOption("repos")))
```
(My packages are hosted on a custom [drat](https://cran.r-project.org/web/packages/drat/index.html) powered R repositorium. While CRAN ist great, I find it too time consuming to maintain all my packages there and `devtools::install_github` does not handle custom dependencies as easily.)

## Install Gambit

To effectively use gtree, you should also install Gambit from the [Gambit project page](http://www.gambit-project.org/). The easiest is to add the installation directory to your PATH, then `gtree` can directly use it.

Otherwise adapt the following call to globably specify the gambit directory:
```
options(gtree.gambit.dir="Your/gambit/path")
```

## Example

Here is an example of a game definition:

```r
library(gtree)
game = new_game(
  gameId = "Ultimatum_Dictator",
  options = make_game_options(verbose=FALSE),
  params = list(numPlayers=2,cake=10, autoAcceptProb=0.1),
  stages = list(
    stage("proposerStage",
      player=1,
      actions = list(
        action("offer",~0:cake)
      )
    ),
    stage("autoAcceptStage",
      nature = list(
        natureMove("autoAccept", 
          set=c(TRUE,FALSE),
          probs = ~ c(autoAcceptProb, 1-autoAcceptProb)
        )
      )  
    ),
    stage("responderStage",
      player=2,
      condition =  ~ autoAccept == FALSE,
      observe = c("offer","autoAccept"),
      actions = list(
        action("accept",c(FALSE,TRUE))
      )
    ),
    stage("payoffStage",
      player=1:2,
      compute=list(
        payoff_1 ~ ifelse(accept | autoAccept, cake-offer,0),
        payoff_2 ~ ifelse(accept | autoAccept, offer,0)
      )
    )
  )
)
```
This is a modified Ultimatum Game. Player 1 (the proposer) can decide how much of the parameter `cake` she offers to player 2 (the responder). With probability `autoAcceptProb` the responder has no choice and the offer is automatically accepted. Otherwise the responder can decide to accept or reject the offer. If he rejects, both players get a payoff of 0.

The game is defined by various stages, that can specify an active player and his actions, like `offer`, random moves of nature like `autoAccept`, or deterministically computed variables like `payoff_1` or `payoff_2` (the payoff variables have reserved names and must always be specified). In addition, one can explicitly specify which variables players `observe` in a stage and a `condition` specifying whether a stage is played or not. The corresponding fields can contain fixed values or formulas starting with `~` that reference to previously specified variables or parameters.

The following code assumes that players have inequality aversion preferences à la Fehr & Schmidt with an envy parameter of `alpha=1` but no guilt (`beta=0`) and then solves for all pure strategy SPE of the game. The results in a table format:


```r
game %>%
  game_set_preferences(pref_ineqAv(alpha=1, beta=0)) %>%
  game_solve() %>%
  game.eq.tables()
```

```
## $offer
## # A tibble: 1 x 2
##   offer eq.inds
##   <int> <chr>  
## 1     4 1      
## 
## $accept
## # A tibble: 11 x 3
##    offer accept eq.inds
##    <int> <lgl>  <chr>  
##  1     0 FALSE  1      
##  2     1 FALSE  1      
##  3     2 FALSE  1      
##  4     3 FALSE  1      
##  5     4 TRUE   1      
##  6     5 TRUE   1      
##  7     6 TRUE   1      
##  8     7 TRUE   1      
##  9     8 TRUE   1      
## 10     9 TRUE   1      
## 11    10 TRUE   1
```

We see that the proposer offers 4 and every offer below 4 would be rejected. What happens for an auto accept probability of 90%?


```r
game %>%
  game_change_param(autoAcceptProb = 0.9) %>%
  game_solve() %>%
  game.eq.tables()
```

```
## $offer
## # A tibble: 1 x 2
##   offer eq.inds
##   <int> <chr>  
## 1     0 1      
## 
## $accept
## # A tibble: 11 x 3
##    offer accept eq.inds
##    <int> <lgl>  <chr>  
##  1     0 FALSE  1      
##  2     1 FALSE  1      
##  3     2 FALSE  1      
##  4     3 FALSE  1      
##  5     4 TRUE   1      
##  6     5 TRUE   1      
##  7     6 TRUE   1      
##  8     7 TRUE   1      
##  9     8 TRUE   1      
## 10     9 TRUE   1      
## 11    10 TRUE   1
```

While the responder's behavior does not change, the proposer will now only offer 0. Sure the offer will be rejected with 10% probability, but in 90% of cases the proposer gets the whole cake for herself.

Take a look at the different [tutorials](https://skranz.github.io/gtree/articles/) for more examples and features.

## Shiny apps to play games

The companion package [gtreeWebPlay](https://github.com/skranz/gtreeWebPlay) allows to create [shiny](https://shiny.rstudio.com/) apps that allow a single player to play the game. The player plays against *bots*. A bot could e.g. follow an equilibrium strategy `bot_eq` or allow the current player to play against randomly drawn strategies of earlier users of the app `bot_pop`.

Currently there is no implementation for multiple human players to play against each other. To run controlled computerized experiments in which subjects directly play with, use e.g. [ztree](https://www.ztree.uzh.ch/en.html) or [otree](https://otree.readthedocs.io/en/latest/) instead.
