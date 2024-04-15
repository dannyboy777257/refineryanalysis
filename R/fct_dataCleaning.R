#' dataCleaningProduction
#'
#' @description Additional data cleaning missed in pre processing files for refinery production
#'
#' @param x dataframe of masterProd
#'
#' @return cleaned up dataframe same format
#'
#' @export
dataCleaningProduction <- function(x) {

  x %>%
    dplyr::mutate(material = trimws(material)) %>%
    dplyr::filter(!(Refinery == "Borger Refinery" & date == "2024-02-01" & material == "PROPANE" & Produced == 2165579),
                  !(Refinery == "Mckee" & date == "2023-09-01" & pageNumber == 2),
                  !(Refinery == "Valero Refining" & date == "2023-09-01" & material == "PROPANE" & Produced == 3708239),
                  !(Refinery %in% c("Nixon Refinery", "Valero Bill Greehey Refinery West", "Beaumont"))) %>%
    dplyr::mutate(InputsRuns = dplyr::case_when(
      Refinery == "El Paso Refinery" & InputsRuns == 32044741 ~ 3204474,
      TRUE ~ InputsRuns)
    )

}

#' dataCleaningDeliveries
#'
#' @description Additional data cleaning missed in pre processing files for refinery deliveries
#'
#' @param x dataframe of master deliveries
#'
#' @return cleaned up dataframe same format
#'
#' @export
dataCleaningDeliveries <- function(x) {

  x %>%
    dplyr::filter(Refinery != "Buckeye Texas Processing") %>%
    dplyr::mutate(DeliveredTo = toupper(DeliveredTo),
                  DeliveredTo = dplyr::if_else(DeliveredTo == "WHOLESALE RELAIL", "WHOLESALE RETAIL", DeliveredTo),
                  DeliveredTo = dplyr::case_when(
                    DeliveredTo %in% c("FLINT HILLS RESOURCES, LC", "RESOURCES, LC HILLS FLINT") ~ "FLINT HILLS RESOURCES. LC",
                    TRUE ~ DeliveredTo
                  ))

}

#' dataCleaningReceipts
#'
#' @description Additional data cleaning missed in pre processing files for refinery receipts
#'
#' @param x dataframe of master receipts
#'
#' @return cleaned up dataframe same format
#'
#' @export
dataCleaningReceipts <- function(x) {

  x %>%
    dplyr::mutate(ReceivedFrom = dplyr::case_when(
      ReceivedFrom == "(85fi1180, 159) 590fc2502, markwest cstl pl" ~ "markwest pl",
      ReceivedFrom == "markwest pl (85fi1180, 590fc2502, cstl 159)" ~ "markwest pl",
      ReceivedFrom == "enterprise markham pl dome" ~ "enterprise pl markham dome",
      ReceivedFrom == "gasoline valero" ~ "valero gasoline",
      ReceivedFrom == "diesel nustar valero" ~ "valero diesel nustar",
      grepl("javalina pl \\(air", ReceivedFrom)  ~ "javalina",
      TRUE ~ ReceivedFrom),
      ReceivedFrom = toupper(ReceivedFrom))

}

#' shapeClean
#'
#' @description read in shapefile and clean
#'
#' @param path file path to shape file
#'
#' @return shapefile
#'
#' @export
shapeClean <- function(path) {

  refShape <- sf::read_sf(path)

  refShape <- sf::st_transform(refShape, crs = 4326)

  return(refShape)

}
