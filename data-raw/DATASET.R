# This is the  master table for production data on all refineries
# had to edit certain error which were not caught in data clean
# some errors are from users filling out forms etc :(
# some refinery having multiple duplicates and major issues due to OCR need to fix with more time
masterProd <- readRDS(file = "C:/Users/danie/Documents/Fin488/refineryFinal/Masterfiles/finalProduction.rds") %>%
  dplyr::mutate(material = trimws(material)) %>%
  dplyr::filter(!(Refinery == "Borger Refinery" & date == "2024-02-01" & material == "PROPANE" & Produced == 2165579),
                !(Refinery == "Mckee" & date == "2023-09-01" & pageNumber == 2),
                !(Refinery == "Valero Refining" & date == "2023-09-01" & material == "PROPANE" & Produced == 3708239),
                !(Refinery %in% c("Nixon Refinery", "Valero Bill Greehey Refinery West", "Beaumont"))) %>%
  dplyr::mutate(InputsRuns = dplyr::case_when(
    Refinery == "El Paso Refinery" & InputsRuns == 32044741 ~ 3204474,
    TRUE ~ InputsRuns
  ))

masterProd[is.na(masterProd)] <- 0
usethis::use_data(masterProd, overwrite = TRUE)

# Master data for deliveries for refineries
masterDeliveries <- readRDS(file = "C:/Users/danie/Documents/Fin488/refineryFinal/Masterfiles/finalDeliveries.rds")
usethis::use_data(masterDeliveries, overwrite = TRUE)

# Master data for receipts for refineries
masterReceipts <- readRDS(file = "C:/Users/danie/Documents/Fin488/refineryFinal/Masterfiles/finalReceipts.rds")
usethis::use_data(masterReceipts, overwrite = TRUE)
