#--------------------------------------------
###### GLOBAL FUNCTIONS ################

### paste number at end of column names so you can join
pasteNumber <- function(x, y) {  paste(x, y)}
##total number of mandates: df, 2 variables
totalN <- function(df, rowVar, colVar) {
  x <- enquo(rowVar)
  y <- enquo(colVar)
  df1 <- df %>%
    group_by(!!x, !!y) %>%
    summarise(total = n()) %>%
    spread(!!y, total) %>%
    replace(., is.na(.), 0)
  return(cbind(df1, Total = rowSums(df1[-1], na.rm = TRUE)))
}

###check to make sure replace_na() is working right
##total value of mandates: df, 2 variables
totalSum <- function(df, rowVar, colVar) {
  x <- enquo(rowVar)
  y <- enquo(colVar)
  df1 <- df %>%
    group_by(!!x, !!y) %>%
    replace_na(list(MandateSizeAmount = 0)) %>%
    summarise(total = sum(MandateSizeAmount)) %>%
    spread(!!y, total) %>%
    replace(., is.na(.), 0)
  return(cbind(df1, Total = rowSums(df1[-1], na.rm = TRUE)))
}

#----FUNCTION: POTENTIAL NEW COMPLETED ASSET CLASS QUARTER MATRIX  USQ,USPNC1,USPNC2
##updated
PNCtable <- function(df) {
  tbl1 <- totalN(df, MainAssetClass, SearchStatus)
  tbl2 <- totalSum(df, MainAssetClass, SearchStatus)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  combined <- tbl1 %>% left_join(tbl2)
  combined <- combined[,c(1,4,8,3,7,2,6,5,9)]
  return(combined)
}

####### FTPtable the new way 
FTPtable <- function(df) {
  tbl1 <- totalN(df, FundType, MainAssetClass)
  tbl2 <- totalN(df, FundType, MandateRegion)
  combined <- tbl1 %>% left_join(tbl2)
  return(combined)
}

### updated
#---- Asset class by investment region
ACbyMRtable <- function(df) {
  tbl1 <- totalN(df, MainAssetClass, MandateRegion)
  tbl2 <- totalSum(df, MainAssetClass, MandateRegion)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}

### UPDATED
#---- SubAsset class by investment region for assete class
SACbyMRtable <- function(df1, assetclass) {
  dftemp <- df1 %>% filter(MainAssetClass == assetclass)
  tbl1 <- totalN(dftemp, SubAssetClass, MandateRegion)
  tbl2 <- totalSum(dftemp, SubAssetClass, MandateRegion)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}


### UPDATED
#---- Investment approach by investment region for equity and fixed Income

IAbyMRtable <- function(df1, assetclass) {
  dftemp <- df1 %>% filter(MainAssetClass == assetclass)
  tbl1 <- totalN(dftemp, InvestmentApproach, MandateRegion)
  tbl2 <- totalSum(dftemp, InvestmentApproach, MandateRegion)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}

### UPDATED
#---- style by investment region for equity 

EQStyletable <- function(df1) {
  dftemp <- df1 %>% filter(MainAssetClass == "Equity")
  tbl1 <- totalN(dftemp, Style, MandateRegion)
  tbl2 <- totalSum(dftemp, Style, MandateRegion)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}

### UPDATED
EQCaptable <- function(df1) {
  dftemp <- df1 %>% filter(MainAssetClass == "Equity")
  tbl1 <- totalN(dftemp, CapSize, MandateRegion)
  tbl2 <- totalSum(dftemp, CapSize, MandateRegion)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}

### UPDATED
# alts table

AltsTable <- function(df1) {
  tbl1 <- df1 %>%
    filter(MainAssetClass %in% alternatives) %>%
    group_by(MainAssetClass, SubAssetClass, MandateRegion) %>%
    summarise(total = n()) %>%
    replace_na(list(SubAssetClass = "(Unspecified)")) %>%
    mutate(MainSub = paste(MainAssetClass, SubAssetClass)) %>%
    spread(MandateRegion, total) %>%
    replace(., is.na(.), 0) 
  tbl1 <- tbl1[, -c(1, 2)]
  tbl1 <- cbind(tbl1, Total = rowSums(tbl1[-1], na.rm = TRUE))
  tbl2 <- df1 %>%
    filter(MainAssetClass %in% alternatives) %>%
    replace_na(list(MandateSizeAmount = 0)) %>%
    group_by(MainAssetClass, SubAssetClass, MandateRegion) %>%
    summarise(total = sum(MandateSizeAmount)) %>%
    replace_na(list(SubAssetClass = "(Unspecified)")) %>%
    mutate(MainSub = paste(MainAssetClass, SubAssetClass)) %>%
    spread(MandateRegion, total) %>%
    replace(., is.na(.), 0) 
  tbl2 <- tbl2[, -c(1, 2)]
  tbl2 <- cbind(tbl2, Total = rowSums(tbl2[-1], na.rm = TRUE))
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}

### UPDATED
#-----Month table function
MonthTable <- function(df1) {
  moz <- df1 %>%
    group_by(Month) %>%
    replace_na(list(MandateSizeAmount = 0)) %>%
    summarise(Total = n(), SumTotal = sum(MandateSizeAmount)) %>%
    mutate(Month = month.abb[as.integer(Month)]) %>%
    replace(., is.na(.), 0)
  moz2 <- rbind(moz, c("Total", colSums(moz[-1])))
  return(moz2)
}

### UPDATED
#-----Year Table Function
YearTable <- function(df1) {
  tbl1 <- totalN(df1, MainAssetClass, Year)
  tbl2 <- totalSum(df1, MainAssetClass, Year)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}

###UPDATED
#----FUNCTION: top consultants: # and $
consTable <- function(df1, sortz = 1) {
  conz <- df1 %>%
    replace_na(list(MandateSizeAmount = 0)) %>%
    group_by(SearchConsultant) %>%
    summarise(Total = n(), SumTotal = sum(MandateSizeAmount)) %>%
    replace(., is.na(.), 0)
  if(sortz != 1){
    conz <- arrange(conz, desc(SumTotal), desc(Total))
  } else {
    conz <- arrange(conz, desc(Total), desc(SumTotal))
  }
  return(conz)
}

###UPDATED
#---function - search consultant matrix
consMX <- function(df0) {
  df1 <- filter(df0, SearchConsultant != "Unknown")
  tbl1 <- totalN(df1, SearchConsultant, FundType)
  tbl2 <- totalN(df1, SearchConsultant, MainAssetClass)
  tbl3 <- totalN(df1, SearchConsultant, MandateRegion)
  combined <- tbl1 %>% left_join(tbl2)
  combined <- combined %>% left_join(tbl3)
  return(combined)
}
