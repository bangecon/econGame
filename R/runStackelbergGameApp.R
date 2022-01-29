#' @export
runStackelbergGameApp <- function() {
  appDir <- system.file("shiny-examples", "stackelbergGameApp", package = "econGame")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `econGame`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
