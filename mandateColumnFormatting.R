#remove spaces, slashes, dashes from column names
colnames(mandates)
col_names <- colnames(mandates)
col_names <- gsub(" ", "", col_names)
col_names <- gsub("\\/", "", col_names)
col_names <- gsub("\\-", "", col_names)
colnames(mandates) <- col_names

class(mandates$EntryDate)

########################CONVERT ENTRYDATE TO DATE FORMAT##############
mandates$EntryDate <- as.POSIXct(mandates$EntryDate, format="%m/%d/%Y")
###----------------------create Month column
mandates$Month <- format(mandates$EntryDate, "%m")
########################REMOVE UNNEEDED COLUMNS########################

mandates$MandateAttribute <- NULL
mandates$Service <- NULL
mandates$LinkToRFP <- NULL
mandates$Comments <- NULL
mandates$FundAddress <- NULL
mandates$FundCity <- NULL
mandates$FundState <- NULL
mandates$FundZipPostalCode <- NULL
mandates$SearchConsultantTelephone <- NULL
mandates$SearchConsultantAddress <- NULL
mandates$SearchConsultantZipPostalCode <- NULL
mandates$SearchConsultantParentFirm <- NULL
mandates$SearchConsultantCity <- NULL
mandates$SearchConsultantState <- NULL
mandates$SearchConsultantCountry <- NULL
mandates$Duration <- NULL
mandates$BenchmarkIndex <- NULL
mandates$AssetClass <- NULL

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
mandates$FundRegion <- map_chr(mandates$FundCountry, fundRegionz)
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
mandates$Year <- map_chr(mandates$EntryDate, getYear)
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
mandates$Quarter <- map_chr(mandates$EntryDate, getQuarter)
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
mandates$AC2 <- map_chr(mandates$MainAssetClass, getAC2)


#levels(mandates$MandateSizeCurrency)

#####################CURRENCY CONVERSION###########################
#---add exchange rate based on Mandate Size Currency from table, left join
mandates <- left_join(mandates, xrateLookup)
#----------convert to USD
mandates$OriginalAmount <- mandates$MandateSizeAmount
mandates$MandateSizeAmount <- mandates$OriginalAmount / mandates$XR
