#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r <- shiny::reactiveValues()

  # added tests to make sure the datasets do not have any na's
  # assigning global variables
  r$masterProd <- refineryanalysis::masterProd

  r$masterDeliveries <- refineryanalysis::masterDeliveries

  r$masterReceipts <- refineryanalysis::masterReceipts

  r$refMap <- refineryanalysis::refShape

  mod_refineryProd_server("refineryProd_1", r = r)
  mod_refineryMove_server("refineryMove_1", r = r)
  mod_refLocations_server("refLocations_1", r = r)
  mod_userGuide_server("userGuide_1")
}
