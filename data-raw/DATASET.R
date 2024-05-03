# This is the  master table for production data on all refineries
# had to edit certain errors which were not caught in data clean
# some errors are from users filling out forms incorrectly etc :(
# some refinery having multiple duplicates and major issues due to OCR need to fix with more time
masterProd <- refineryanalysis::dataCleaningProduction(
  readRDS(file = "./data-raw/finalProduction.rds"))

masterProd[is.na(masterProd)] <- 0
usethis::use_data(masterProd, overwrite = TRUE)

# Master data for deliveries for refineries
masterDeliveries <- refineryanalysis::dataCleaningDeliveries(
  readRDS(file = "./data-raw/finalDeliveries.rds"))

usethis::use_data(masterDeliveries, overwrite = TRUE)

# Master data for receipts for refineries
masterReceipts <- refineryanalysis::dataCleaningReceipts(
  readRDS(file = "./data-raw/finalReceipts.rds"))

usethis::use_data(masterReceipts, overwrite = TRUE)

# shape file for refineries in texas
refShape <- refineryanalysis::shapeClean("./data-raw/Petroleum_Refinery.shp")

usethis::use_data(refShape, overwrite = TRUE)
