#' refineryMove UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_refineryMove_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::br(),
    shiny::fluidRow(
      shiny::column(4,
                    shiny::selectInput(inputId = ns("selectRefineryRec"),
                                       label = "Choose Refinery (Used for all Receipt Graphs)",
                                       choices = unique(refineryanalysis::masterReceipts$Refinery),
                                       multiple = FALSE,
                                       selected = unique(refineryanalysis::masterReceipts$Refinery)[1])),
      shiny::column(4,
                    shiny::selectInput(inputId = ns("selectProductRec"),
                                       label = "Product",
                                       choices = "",
                                       multiple = FALSE,
                                       selected = ""))
      ),
    shiny::br(),
    shiny::br(),
    shiny::fluidRow(plotly::plotlyOutput(outputId = ns("compRec"))),
    shiny::br(),
    shiny::br(),
    fluidRow(
      shiny::column(4,
                   shiny::selectInput(inputId = ns("selectCompanyRec"),
                                      label = "Company (Receipts From)",
                                      choices = "",
                                      multiple = FALSE,
                                      selected = ""))
    ),
    shiny::br(),
    shiny::br(),
    shiny::fluidRow(plotly::plotlyOutput(outputId = ns("compRecTime"))),
    shiny::br(),
    shiny::br(),
    shiny::fluidRow(
    shiny::column(4,
                  shiny::selectInput(inputId = ns("selectRefineryDel2"),
                                     label = "Choose Refinery (used for all Delivery Graphs)",
                                     choices = unique(refineryanalysis::masterDeliveries$Refinery),
                                     multiple = FALSE,
                                     selected = unique(refineryanalysis::masterDeliveries$Refinery)[1])),
    shiny::column(4,
                  shiny::selectInput(inputId = ns("selectProductDel2"),
                                     label = "Product",
                                     choices = "",
                                     multiple = FALSE,
                                     selected = ""))),
    shiny::br(),
    shiny::br(),
    shiny::fluidRow(plotly::plotlyOutput(outputId = ns("compDel"))),
    shiny::br(),
    shiny::br(),
    fluidRow(
      shiny::column(4,
                    shiny::selectInput(inputId = ns("selectCompanyDel"),
                                       label = "Company (Delivered To)",
                                       choices = "",
                                       multiple = FALSE,
                                       selected = ""))
    ),
    shiny::br(),
    shiny::br(),
    shiny::fluidRow(plotly::plotlyOutput(outputId = ns("compDelTime")))

  )
}

#' refineryMove Server Functions
#'
#' @noRd
mod_refineryMove_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # update product/company inputs based on refinery selected for receipts
    shiny::observeEvent(input$selectRefineryRec, {

      shiny::updateSelectInput(
        session,
        "selectProductRec",
        choices = unique(refineryanalysis::masterReceipts %>%
                           dplyr::filter(
                             Refinery == input$selectRefineryRec) %>%
                           dplyr::pull(material)),
        selected = unique(refineryanalysis::masterReceipts %>%
                            dplyr::filter(
                              Refinery == input$selectRefineryRec) %>%
                            dplyr::pull(material))[1],
      )


      shiny::updateSelectInput(
        session,
        "selectCompanyRec",
        choices = unique(refineryanalysis::masterReceipts %>%
                           dplyr::filter(
                             Refinery == input$selectRefineryRec) %>%
                           dplyr::pull(ReceivedFrom)),
        selected = unique(refineryanalysis::masterReceipts %>%
                            dplyr::filter(
                              Refinery == input$selectRefineryRec) %>%
                            dplyr::pull(ReceivedFrom))[2],
      )

    })

    # update product/company inputs based on refinery selected for deliveries
    shiny::observeEvent(input$selectRefineryDel2, {

      shiny::updateSelectInput(
        session,
        "selectProductDel2",
        choices = unique(refineryanalysis::masterDeliveries %>%
                           dplyr::filter(
                             Refinery == input$selectRefineryDel2) %>%
                           dplyr::pull(material)),
        selected = unique(refineryanalysis::masterDeliveries %>%
                            dplyr::filter(
                              Refinery == input$selectRefineryDel2) %>%
                            dplyr::pull(material))[1],
      )

      shiny::updateSelectInput(
        session,
        "selectCompanyDel",
        choices = unique(refineryanalysis::masterDeliveries %>%
                           dplyr::filter(
                             Refinery == input$selectRefineryDel2) %>%
                           dplyr::pull(DeliveredTo)),
        selected = unique(refineryanalysis::masterDeliveries %>%
                            dplyr::filter(
                              Refinery == input$selectRefineryDel2) %>%
                            dplyr::pull(DeliveredTo))[1],
      )

    })

    # creating bar graph of largest company sending to refinery
    output$compRec <- plotly::renderPlotly({

      shiny::req(input$selectRefineryRec)

      # I am colorblind and I can see it so colors may be funky because I needed
      # 24 of them. Pallete from google with some of my own
      colorPal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                    "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                    "#920000","#924900","#db6d00","#24ff24","#ffff6d",
                    "#1b8537","#1343d4","#092787","#7c0982","#442e45",
                    "#b8393f","#f5959a","#e0c453","#db231d")

      mats <- unique(masterReceipts$material)
      pal <- magrittr::set_names(colorPal, mats)

      r$masterReceipts %>%
        dplyr::filter(Refinery == input$selectRefineryRec,
                      material == input$selectProductRec) %>%
        dplyr::group_by(ReceivedFrom) %>%
        dplyr::summarise(Barrels = sum(Barrels)) %>%
        dplyr::arrange(dplyr::desc(Barrels)) %>%
        dplyr::mutate(ReceivedFrom = factor(ReceivedFrom, levels = ReceivedFrom)) %>%
        plotly::plot_ly(x = ~ReceivedFrom,
                        y = ~Barrels,
                        type = "bar",
                        color = ~ReceivedFrom) %>%
        plotly::layout(title = paste0("Largest Total Receipts for ", input$selectRefineryRec, " since 2023"),
                       xaxis = list(title = "",
                                    categoryorder = "total descending",
                                    tickangle = -45),
                       yaxis = list(title = "Volume (barrels)"),
                       showlegend = TRUE)

    })

    # receipts through time line chart displaying various products for one company
    output$compRecTime <- plotly::renderPlotly({

      shiny::req(input$selectRefineryRec, input$selectCompanyRec)
      # browser()
      r$masterReceipts %>%
        dplyr::filter(Refinery == input$selectRefineryRec,
                      ReceivedFrom == input$selectCompanyRec) %>%
        dplyr::arrange(dplyr::desc(Date)) %>%
        plotly::plot_ly(x = ~Date,
                        y = ~Barrels,
                        type = "scatter",
                        mode = "lines+markers",
                        color = ~material) %>%
        plotly::layout(title = paste0("Receipts from ", input$selectCompanyRec, " by Product"),
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Volume (barrels)"),
                       showlegend = TRUE)

    })


    # creating bar graph of largest company receiving from refinery
    output$compDel <- plotly::renderPlotly({

      shiny::req(input$selectRefineryDel2)

      colorPal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                    "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                    "#920000","#924900","#db6d00","#24ff24","#ffff6d",
                    "#1b8537","#1343d4","#092787","#7c0982","#442e45",
                    "#b8393f","#f5959a","#e0c453","#db231d")

      mats <- unique(masterReceipts$material)
      pal <- magrittr::set_names(colorPal, mats)

      r$masterDeliveries %>%
        dplyr::filter(Refinery == input$selectRefineryDel2,
                      material == input$selectProductDel2) %>%
        dplyr::group_by(DeliveredTo) %>%
        dplyr::summarise(Barrels = sum(Barrels)) %>%
        dplyr::mutate(DeliveredTo = toupper(DeliveredTo)) %>%
        dplyr::arrange(dplyr::desc(Barrels)) %>%
        dplyr::mutate(DeliveredTo = factor(DeliveredTo, levels = DeliveredTo)) %>%
        plotly::plot_ly(x = ~DeliveredTo,
                        y = ~Barrels,
                        type = "bar",
                        color = ~DeliveredTo) %>%
        plotly::layout(title = paste0("Largest Total Deliveries from ", input$selectRefineryDel2, " since 2023"),
                       xaxis = list(title = "",
                                    categoryorder = "total descending",
                                    tickangle = -45),
                       yaxis = list(title = "Volume (barrels)"))

    })

    # deliveries through time by product for specific company
    output$compDelTime <- plotly::renderPlotly({

      shiny::req(input$selectRefineryDel2, input$selectCompanyDel)
      # browser()
      r$masterDeliveries %>%
        dplyr::filter(Refinery == input$selectRefineryDel2,
                      DeliveredTo == input$selectCompanyDel) %>%
        dplyr::arrange(dplyr::desc(Date)) %>%
        plotly::plot_ly(x = ~Date,
                        y = ~Barrels,
                        type = "scatter",
                        mode = "lines+markers",
                        color = ~material) %>%
        plotly::layout(title = paste0("Deliveries for ", input$selectCompanyDel, " by Product"),
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Volume (barrels)"),
                       showlegend = TRUE)

    })

  })
}

## To be copied in the UI
# mod_refineryMove_ui("refineryMove_1")

## To be copied in the server
# mod_refineryMove_server("refineryMove_1")
