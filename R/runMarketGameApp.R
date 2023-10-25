##' Run an `econGame` app.
##'
##' This is a wrapper function for the game-specific `run_Game_App()` functions in the marketGames package.
##'
##' @param game the name of the `econGame` app you want to tabulate, which can be one of:
##'   1. `equlibriumGame` to tabulate results simple supply-and-demand equilibrium game.
##'   2. `entryGame` two-sector entry-and-exit game.
##'   3. `bertrandGame` Bertrand duopoly game.
##'   4. `cournotGame` Cournot duopoly game.
##'   5. `stackelbergGame` sequential leader-follower game.
##'   6. `ultimatumGame` ultimatum game.
##'   7. `anchoringGame` the effects of anchoring on guessing.
##'   8. `lobbyGame` lottery-auction game.
##'   9. `publicgoodGame` public good game with free-rider effects.
##'   10. `pollutionGame` the effects of alternative pollution-abatement policies.
##'   11. `multipdGame` multi-player prisoner's dilemma game.
##'   12. `staghuntGame` two-person stag hunt gmae.
##'
##' @export

runMarketGameApp <- function(game = NULL) {
  if(is.null(game)) {
    option <- readline("Which game would you like to tabulate? \n
                       1. `equlibriumGame` a simple supply-and-demand equilibrium game. \n
                       2. `entryGame` two-sector entry-and-exit game. \n
                       3. `bertrandGame` Bertrand duopoly game. \n
                       4. `cournotGame` Cournot duopoly game. \n
                       5. `stackelbergGame` sequential leader-follower game. \n
                       6. `ultimatumGame` ultimatum game.  \n
                       7. `anchoringGame` the effects of anchoring on guessing.  \n
                       8. `lobbyGame` lottery-auction game. \n
                       9. `publicgoodGame` public good game with free-rider effects. \n
                       10. `pollutionGame` the effects of alternative pollution-abatement policies. \n
                       11. `multipdGame` multi-player prisoner's dilemma game. \n
                       12. `staghuntGame` two-person stag hunt gmae. \n
                       ")
  }
  appDir <- system.file("shiny-examples", game, package = "econGame")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `econGame`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
