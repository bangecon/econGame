#' @export
runPublicgoodGameApp <- function() {
  appDir <- system.file("shiny-examples", "publicgoodGameApp", package = "econGame")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `econGame`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
