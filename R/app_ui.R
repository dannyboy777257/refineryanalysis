#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = shinythemes::shinytheme("united"),
      shiny::navbarPage(id = "myTabs",
           "Refinery Analysis",
           shiny::tabPanel("User Guide",
                           mod_userGuide_ui("userGuide_1")),
           shiny::tabPanel("Refinery Production",
                           mod_refineryProd_ui("refineryProd_1")),
           shiny::tabPanel("Refinery Movements",
                           mod_refineryMove_ui("refineryMove_1")),
           shiny::tabPanel("Refinery Map",
                           mod_refLocations_ui("refLocations_1"))

      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "refineryanalysis"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
