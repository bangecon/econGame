#' @export
runUltimatumGameApp <- function() {
  appDir <- system.file("shiny-examples", "ultimatumGameApp", package = "econGame")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `econGame`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
