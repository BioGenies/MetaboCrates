#' MetaboCrates Graphical User Interface
#'
#' @description Launches graphical user interface .
#'
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages
#' @importFrom shiny runApp
#'
#' @section Warning: Any ad-blocking software may cause malfunctions.
#'
#' @examples
#' if(interactive()) {
#'   MetaboCrates_gui()
#' }
#'
#' @export MetaboCrates_gui
#'

MetaboCrates_gui <- function(){
  runApp(system.file("metabocrates", package = "MetaboCrates"))
}