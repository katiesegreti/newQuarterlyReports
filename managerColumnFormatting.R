

########################CONVERT ENTRYDATE TO DATE FORMAT##############
managers$MandateDate <- as.POSIXct(managers$MandateDate, format="%m/%d/%Y")
###----------------------create Month column
managers$Month <- format(managers$MandateDate, "%m")
########################REMOVE UNNEEDED COLUMNS########################

managers$MandateAttribute <- NULL
managers$Service <- NULL
managers$LinkToRFP <- NULL
managers$Comments <- NULL
managers$FundAddress <- NULL
managers$FundCity <- NULL
managers$FundState <- NULL
managers$FundZipPostalCode <- NULL
managers$SearchConsultantTelephone <- NULL
managers$SearchConsultantAddress <- NULL
managers$SearchConsultantZipPostalCode <- NULL
managers$SearchConsultantParentFirm <- NULL
managers$SearchConsultantCity <- NULL
managers$SearchConsultantState <- NULL
managers$SearchConsultantCountry <- NULL
managers$Duration <- NULL
managers$BenchmarkIndex <- NULL
managers$AssetClass <- NULL

managers$AccountSizeCurrency <- NULL
managers$GeneralConsultant <- NULL
managers$GeneralConsultantParentFirm <- NULL
managers$GeneralConsultantCountry <- NULL
managers$AssetManagerContact <- NULL
managers$AssetManagerContactPosition <- NULL
managers$AssetManagerContactTelephone <- NULL
managers$AssetManagerContactFax <- NULL
managers$AssetManagerContactEmail <- NULL
managers$AssetManagerAddress <- NULL
managers$AssetManagerCity <- NULL
managers$AssetManagerState <- NULL
managers$AssetManagerZip.PostalCode <- NULL
managers$AssetManagerMainTelephone <- NULL
managers$AssetManagerMainFax <- NULL
managers$AssetManagerWebSiteURL <- NULL





########################CREATE FUND REGION COLUMN###############
######################## (FUND REGION BASED ON FUND COUNTRY)########
fundRegionz <- function(x) {
  if(x == "United States") {
    return("US")
  }else if(x == "United Kingdom") {
    return("UK")
  }else if(x %in% Europe) {
    return("EUR")
  }else if(x %in% Asia) {
    return("ASIA")
  }else if(x %in% Oceania) {
    return("AUS")
  }else {
    return("Other")
  }
}
managers$FundRegion <- map_chr(managers$FundCountry, fundRegionz)

########################DATES - ADD YEAR/ QUARTER CATEGORIES########
########################ADD YEAR COLUMN###########################
########################UPDATE EVERY YEAR!!!!!!!!!!!!!!!!!!!!!!!!!!
getYear <- function(xdate) {
  if(xdate < y2016start) {
    return("2015")
  }else if(xdate < y2017start) {
    return("2016")
  }else {
    return("2017")
  }
}
managers$Year <- map_chr(managers$MandateDate, getYear)
########################ADD QUARTER COLUMN###################
###UPDATE QUARTERTLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
getQuarter <- function(xdate) {
  if(xdate >= q4start) {
    return("Q4")
  }else if(xdate >= q3start) {
    return("Q3")
  }else if(xdate >= q2start) {
    return("Q2")
  }else if(xdate >= q1start) {
    return("Q1")
  }else {
    return("NA")
  }
}
managers$Quarter <- map_chr(managers$MandateDate, getQuarter)

#######################NEW COLUMN-CONSOLIDATE SOME ASSET CLASSES##
getAC2 <- function(x) {
  if(x %in% alternatives) {
    return("Other Alternatives")
  }else if(x %in% other) {
    return("Other")
  }else {
    return(as.character(x))
  }
}
managers$AC2 <- map_chr(managers$MainAssetClass, getAC2)

#####################CURRENCY CONVERSION###########################
#---assing exchange rate based on Mandate Size Currency
managers <- left_join(managers, xrateLookup)
#----------convert to USD
managers$OriginalAmount <- managers$AccountSizeAmount
managers$AccountSizeAmount <- managers$OriginalAmount / managers$XR




