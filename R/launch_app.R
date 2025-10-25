#' Launch my shiny app
#' 
#' @export
#' @importFrom shiny runApp
launch_app <- function() {
  appDir <- system.file("shiny", package = "BHAIBYE")
  
  shiny::runApp(appDir)
}
