#' Get path to example data
#'
#' @description Allows to access example data.
#'
#' @param name Name of the file.
#'
#' @returns Path to example data.
#'
#' @examples
#' get_example_data("small_biocrates_example.xlsx")
#'
#' @export

get_example_data <- function(name){
  system.file("extdata", name, package = "MetaboCrates")
}
