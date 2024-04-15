#' userGuide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_userGuide_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
      shiny::mainPanel(
        shiny::h1("Application Information"),
        shiny::h4("- All charts are interactive, by clicking on the legend entries or various tools in the top right of the chart"),
        shiny::h4("- Most inputs are searchable by: selecting input, hitting backspace and typing"),
        shiny::h4("- Ui is built in a waterfall showing largest refineries which can be selected easily in following inputs"),
        shiny::h4("- Inputs are displayed multiple times to allow for comparison between receipts and deliveries easily"),
        shiny::h2("Refinery Production"),
        shiny::h4("- This tab provides information on specific refinery production, inventory and inputs"),
        shiny::h4("- The first refinery input will be used for all charts below that input"),
        shiny::h4("- The product input allows you to choose multiple products to view starting inventories for your chosen refinery"),
        shiny::h2("Refinery Movement"),
        shiny::h4("- This tab displays receipts and deliveries of various products to specific companies per refinery"),
        shiny::h4("- The first refinery input will be used for all receipt charts below that input"),
        shiny::h4("- The product input allows you to filter the data for the chart below"),
        shiny::h4("- The company input works with the refinery input above to view product movement by company"),
        shiny::h4("- Deliveries charts and inputs work the same way as receipts inputs described above"),
        shiny::h2("Refinery Map"),
        shiny::h4("- This tab provides a visual reference for refineries in Texas"),
        shiny::h4("- By hovering over the dots the refinery name and owner will be displayed"),
        shiny::h4("- If you click on the dot, all refinery units will be displayed with their respective capacities"),
        shiny::h2("In Progress"),
        shiny::h4("Currently in progress work that will be added in the coming weeks/months:"),
        shiny::h4("- Currently only 2 refineries have been parsed fully for company specifc receipts and deliveries, so working on adding the rest"),
        shiny::h4("- Additional tab which will display pipeline, barge movements of products to and from PADD 3"),
        shiny::h4("- Additional tab for analytics to view correlations, relationships etc."),
        shiny::h4("- Creating more ways of viewing the data in current tabs using various graphs"),
        shiny::h4("- Adding more years of data"),
        shiny::h3("Author: Daniel Vovk")
      )
    )

  )
}

#' userGuide Server Functions
#'
#' @noRd
mod_userGuide_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_userGuide_ui("userGuide_1")

## To be copied in the server
# mod_userGuide_server("userGuide_1")
