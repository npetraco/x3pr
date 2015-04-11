#--------------------------------------------
#' @title Shiny app to view a 3D surface
#'
#' @description Shiny app to view a 3D surface
#' 
#' @details Should work with all supported 3D file formats.
#'
#' @references http://shiny.rstudio.com/
#'
#' @examples
#' launch_3Dview()
#--------------------------------------------
launch_3Dview <- function(){
  #Location of the ui and server scripts:
  runApp(system.file("gui/view", package = "x3pr"))
}

