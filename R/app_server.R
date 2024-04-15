#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r <- shiny::reactiveValues()

  # assigning global variables
  r$masterProd <- refineryanalysis::masterProd

  r$masterDeliveries <- refineryanalysis::masterDeliveries

  r$masterReceipts <- refineryanalysis::masterReceipts

  mod_refineryProd_server("refineryProd_1", r = r)
  mod_refineryMove_server("refineryMove_1", r = r)
}
