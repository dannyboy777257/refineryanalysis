#' refLocations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_refLocations_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
    leaflet::leafletOutput(ns("refMap"), height = "90vh")
    )
  )
}

#' refLocations Server Functions
#'
#' @noRd
mod_refLocations_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$refMap <- leaflet::renderLeaflet({

      # creating leaflet map
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = -99.818720, lat = 30.780712, zoom = 4) %>%
        leaflet::addCircleMarkers(data = r$refMap,
                                  label = ~paste0(Site, " Owned by: ", Company),
                                  popup = ~paste(paste0("Refinery: ", Site), "<br>",
                                                 paste0("Vacuum Distillation Capacity (Mbpd): ", Vdist_Mbpd), "<br>",
                                                 paste0("Catalytic Cracking Capacity (Mbpd): ", CaDis_Mbpd), "<br>",
                                                 paste0("Hydro Cracking Capacity (Mbpd): ", HyCrk_Mbpd), "<br>",
                                                 paste0("Catalytic Reforming Capacity (Mbpd): ", CaRef_Mbpd), "<br>",
                                                 paste0("Alkylates Isomerization Capacity (Mbpd): ", Isal_Mbpd), "<br>",
                                                 paste0("Desulfurization Capacity (Mbpd): ", HDS_Mbpd), "<br>",
                                                 paste0("Fluid and Delayed Coking Capacity (Mbpd): ", Cokin_Mbpd), "<br>",
                                                 paste0("Asphalt and Road Oil Capacity (Mbpd): ", Asph_Mbpd), "<br>"),
                                  col = 'blue',
                                  radius = 0.4)


    })


  })
}

## To be copied in the UI
# mod_refLocations_ui("refLocations_1")

## To be copied in the server
# mod_refLocations_server("refLocations_1")
