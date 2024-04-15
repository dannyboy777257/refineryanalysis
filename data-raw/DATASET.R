# This is the  master table for production data on all refineries
# had to edit certain errors which were not caught in data clean
# some errors are from users filling out forms incorrectly etc :(
# some refinery having multiple duplicates and major issues due to OCR need to fix with more time
masterProd <- refineryanalysis::dataCleaningProduction(
  readRDS(file = "C:/Users/danie/Documents/Fin488/refineryFinal/Masterfiles/finalProduction.rds"))

masterProd[is.na(masterProd)] <- 0
usethis::use_data(masterProd, overwrite = TRUE)

# Master data for deliveries for refineries
masterDeliveries <- refineryanalysis::dataCleaningDeliveries(
  readRDS(file = "C:/Users/danie/Documents/Fin488/refineryFinal/Masterfiles/finalDeliveries.rds"))

usethis::use_data(masterDeliveries, overwrite = TRUE)

# Master data for receipts for refineries
masterReceipts <- refineryanalysis::dataCleaningReceipts(
  readRDS(file = "C:/Users/danie/Documents/Fin488/refineryFinal/Masterfiles/finalReceipts.rds"))

usethis::use_data(masterReceipts, overwrite = TRUE)

# shape file for refineries in texas
refShape <- refineryanalysis::shapeClean("C:/Users/danie/Documents/Fin488/refineryFinal/shapeRef/Petroleum_Refinery.shp")

usethis::use_data(refShape, overwrite = TRUE)
