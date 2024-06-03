#' refineryProd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_refineryProd_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(plotly::plotlyOutput(outputId = ns("bigRef"))),
    shiny::br(),
    shiny::br(),
    shiny::column(4,
                  shiny::selectInput(inputId = ns("selectRefinery"),
                                     label = "Refinery",
                                     choices = unique(refineryanalysis::masterProd$Refinery),
                                     multiple = FALSE,
                                     selected = unique(refineryanalysis::masterProd$Refinery)[1])),
    shiny::br(),
    shiny::br(),
    plotly::plotlyOutput(outputId = ns("totalProduction")),
    shiny::br(),
    shiny::br(),
    shiny::plotOutput(outputId = ns("prodCor"), height = "900px"),
    shiny::br(),
    shiny::br(),
    plotly::plotlyOutput(outputId = ns("totalRuns")),
    shiny::br(),
    shiny::br(),
    shiny::fluidRow(
    shiny::column(4,
                  shiny::selectInput(inputId = ns("selectProduct"),
                                     label = "Product (multiple selections)",
                                     choices = "",
                                     multiple = TRUE,
                                     selectize = FALSE,
                                     selected = ""))),
    shiny::br(),
    shiny::br(),
    shiny::fluidRow(plotly::plotlyOutput(outputId = ns("startInv"))),
    shiny::br(),
    shiny::br(),

  )
}

#' refineryProd Server Functions
#'
#' @noRd
mod_refineryProd_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # largest refineries based on total product productions and displays fuel used too
    output$bigRef <- plotly::renderPlotly({

      r$masterProd %>%
        dplyr::group_by(Refinery)%>%
        dplyr::summarise(Produced = sum(Produced),
                         `Fuel Used` = sum(fuelUsed)) %>%
        dplyr::arrange(dplyr::desc(Produced)) %>%
        tidyr::pivot_longer(cols = c(Produced, `Fuel Used`), names_to = "type", values_to = "vols") %>%
        dplyr::mutate(Refinery = factor(Refinery, levels = unique(Refinery))) %>%
        plotly::plot_ly(x = ~Refinery,
                        y = ~vols,
                        type = "bar",
                        color = ~type) %>%
        plotly::layout(title = "Highest Producing Refineries with Fuel Usage since 2023",
                       xaxis = list(title = "",
                                    tickangle = -45),
                       yaxis = list(title = "Volume (barrels)"),
                       barmode = "group")

    })

    # updating product input based on refinery chosen above
    shiny::observeEvent(input$selectRefinery, {

      shiny::updateSelectInput(
        session,
        "selectProduct",
        choices = unique(refineryanalysis::masterProd %>%
                           dplyr::filter(
                             Refinery == input$selectRefinery,
                             startInv != 0) %>%
                           dplyr::pull(material)),
        selected = unique(refineryanalysis::masterProd %>%
                            dplyr::filter(
                              Refinery == input$selectRefinery,
                              startInv != 0) %>%
                            dplyr::pull(material))[1],
      )

    })

    # total production by product for chosen refinery through time
    output$totalProduction <- plotly::renderPlotly({

      shiny::req(input$selectRefinery)

      r$masterProd %>%
        dplyr::filter(Refinery == input$selectRefinery,
                      Produced != 0) %>%
        dplyr::arrange(dplyr::desc(date)) %>%
        plotly::plot_ly(x = ~date,
                        y = ~Produced,
                        type = "scatter",
                        mode = "lines+markers",
                        color = ~material) %>%
        plotly::layout(title = "Total Production by Product",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Volume (barrels)"))

    })

    output$prodCor <- shiny::renderPlot({

    col_function <- function(data, mapping, method = "kendall", use ="pairwise"){
      x <- GGally::eval_data_col(data, mapping$x)
      y <- GGally::eval_data_col(data, mapping$y)
      correlation <- stats::cor(x,y, method = method, use = use)
      col_palate <- grDevices::colorRampPalette(c("blue", "white", "green"), interpolate = "spline")
      fill_in <- col_palate(100)[findInterval(correlation, seq(-1,1, length=100))]
      GGally::ggally_cor(data = data, mapping = mapping)+ggplot2::theme_void()+ggplot2::theme(panel.background = ggplot2::element_rect(fill = fill_in))}
    # browser()
    r$masterProd %>%
      dplyr::filter(Refinery == input$selectRefinery,
                    Produced != 0) %>%
      dplyr::select(material, Produced, date) %>%
      tidyr::pivot_wider(names_from = material, values_from = Produced) %>%
      dplyr::arrange(dplyr::desc(date)) %>%
      tidyr::fill(dplyr::everything(), .direction = "down") %>%
      tidyr::fill(dplyr::everything(), .direction = "up") %>%
      dplyr::select(-date) %>%
      GGally::ggpairs(axisLabels = "none", upper = list(continuous = col_function, lower = list(continuous = GGally::wrap("points", size = 0.8))))

    })


    # total inputs used by product for chosen refinery
    output$totalRuns <- plotly::renderPlotly({

      shiny::req(input$selectRefinery)

      r$masterProd %>%
        dplyr::filter(Refinery == input$selectRefinery,
                      InputsRuns != 0) %>%
        dplyr::arrange(dplyr::desc(date)) %>%
        plotly::plot_ly(x = ~date,
                        y = ~InputsRuns,
                        type = "scatter",
                        mode = "lines+markers",
                        color = ~material) %>%
        plotly::layout(title = "Total Input Runs by Product",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Volume (barrels)"),
                       showlegend = TRUE)
    })

    # view starting inventories by product through time for refinery
    output$startInv <- plotly::renderPlotly({

      shiny::req(input$selectRefinery, input$selectProduct)

      # I am colorblind and I can see it so colors may be funky
      colorPal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
               "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
               "#920000","#924900","#db6d00","#24ff24","#ffff6d",
               "#1b8537","#1343d4","#092787","#7c0982","#442e45",
               "#b8393f","#f5959a","#e0c453","#db231d")

      mats <- unique(masterProd$material)
      pal <- magrittr::set_names(colorPal, mats)

      r$masterProd %>%
        dplyr::filter(Refinery == input$selectRefinery,
                      material %in% input$selectProduct) %>%
        dplyr::arrange(dplyr::desc(date)) %>%
        plotly::plot_ly(x = ~date,
                        y = ~startInv,
                        type = "bar",
                        color = ~material,
                        colors = pal) %>%
        plotly::layout(title = "Inventory Through Time",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Volume (barrels)"),
                       barmode = "group")

    })

  })
}

## To be copied in the UI
# mod_refineryProd_ui("refineryProd_1")

## To be copied in the server
# mod_refineryProd_server("refineryProd_1")
