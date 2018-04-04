#--------------------------------------------
###### GLOBAL FUNCTIONS ################
#is even and is odd functions
is.even <- function(x){x %%2 == 0}
is.odd <- function(x){x %%2 == 1}
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

##total value of mandates: df, 2 variables
totalSum <- function(df, rowVar, colVar) {
  x <- enquo(rowVar)
  y <- enquo(colVar)
  df1 <- df %>%
    group_by(!!x, !!y) %>%
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
USPNC <- PNCtable(USQ)

####### FTPtable the new way 
## still have to get totals column
FTtableNEW <- function(df) {
  tbl1 <- totalN(df, FundType, MainAssetClass)
  tbl2 <- totalN(df, FundType, MandateRegion)
  combined <- tbl1 %>% left_join(tbl2)
  return(combined)
}
USP_FT <- FTtableNEW(USQP)

### updated
#---- Asset class by investment region
ACbyMRtable <- function(df) {
  tbl1 <- totalN(df, MainAssetClass, MandateRegion)
  tbl2 <- totalSum(df, MainAssetClass, MandateRegion)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}

USQAC1 <- ACbyMRtable(USQP)


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

SACtest1 <- SACbyMRtable(mandates, "Commodities")


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
IAtest <- IAbyMRtable(US12mo, "Equity")

  
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
eqstest <- EQStyletable(mandates)

### UPDATED
EQCaptable <- function(df1) {
  dftemp <- df1 %>% filter(MainAssetClass == "Equity")
  tbl1 <- totalN(dftemp, CapSize, MandateRegion)
  tbl2 <- totalSum(dftemp, CapSize, MandateRegion)
  colznmz <- colnames(tbl2)[-1]
  colnames(tbl2)[-1] <- map_chr(colznmz, pasteNumber, 1)
  tbl1 %>% left_join(tbl2)
}
eqctest <- EQCaptable(mandates)


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

altstest <- AltsTable(USQ)

### STILL NEED TO UPDATE
#-----Month table function
MonthTable <- function(df1){
  monthz <- c("01","02","03","04","05","06","07","08", "09","10","11","12")
  monthz2 <- c("Jan","Feb","March","April","May","June","July","Aug","Sept", "Oct","Nov","Dec")
  totalz <- c("Total#", "Total$")
  z <- rep(0, length(monthz)*length(totalz))
  m1 <- matrix(z, nrow=length(monthz), ncol=length(totalz))
  rownames(m1) <- monthz2
  colnames(m1) <- totalz
  for(i in 1:length(monthz)){
    m1[i,1] <- nrow(df1[df1$Month==monthz[i],])
    m1[i,2] <- sum(df1[df1$Month==monthz[i], "MandateSizeAmount"])
  }
  return(m1)
}
### STILL NEED TO UPDATE
#-----Year Table Function
YearTable <- function(df1){
  yrs <- c("2015","2016","2017")
  AC <- levels(droplevels(df1$MainAssetClass))
  yearz <- rep(yrs, times=1, each=2)
  z <- rep(0, length(yearz)*length(AC))
  #create matrix
  m1 <- matrix(z, nrow=length(AC), ncol=length(yearz))
  rownames(m1) <- AC
  colnames(m1) <- yearz
  #fill matrix
  for(i in 1:length(AC)){
    for(j in 1:length(yearz)){
      if(is.odd(j)){
        m1[i,j] <- nrow(df1[df1$MainAssetClass==AC[i]&df1$Year==yearz[j],])
      }else{
        m1[i,j] <- sum(df1[df1$MainAssetClass==AC[i]&df1$Year==yearz[j], "MandateSizeAmount"])
      }
    }
  }
  return(m1)
}
### STILL NEED TO UPDATE
#----FUNCTION: top consultants: # and $
consTable <- function(df1, sortz=1){
  consz <- levels(droplevels(df1$SearchConsultant))
  colz <- c("Total1", "Total2")
  z <- rep(0, length(consz)*length(colz))
  #create matrix
  m1 <- matrix(z, nrow=length(consz), ncol=length(colz))
  rownames(m1) <- consz
  colnames(m1) <- colz
  #fill matrix
  for(i in 1:length(consz)){
    m1[i,1] <- nrow(df1[df1$SearchConsultant==consz[i],])
    m1[i,2] <- sum(df1[df1$SearchConsultant==consz[i],"MandateSizeAmount"])
  }
  if(sortz==1){
    m2 <- m1[order(m1[,1],m1[,2],decreasing = TRUE),]
  }else{
    m2 <- m1[order(m1[,2],m1[,1],decreasing = TRUE),]
  }
  
  return(m2)
}
### STILL NEED TO UPDATE
#---function - search consultant matrix
consMX <- function(df0){
  df1 <- df0[df0$SearchConsultant!="Unknown",]
  SC <- levels(droplevels(df1$SearchConsultant))
  FT <- levels(droplevels(df1$FundType))
  AC <- levels(droplevels(df1$MainAssetClass))
  MR <- levels(droplevels(df1$MandateRegion))
  colz <- c("Total", FT, AC, MR)
  z <- rep(0, length(SC)*length(colz))
  #create matrix
  m1 <- matrix(z, nrow=length(SC), ncol=length(colz))
  rownames(m1) <- SC
  colnames(m1) <- colz
  #fillmatrix
  for(i in 1:length(SC)){
    m1[i,1] <- nrow(df1[df1$SearchConsultant==SC[i],])
    for(j in 2:length(colz)){
      m1[i,j] <- nrow(df1[df1$SearchConsultant==SC[i]&(df1$FundType==colz[j]|df1$MainAssetClass==colz[j]|
                                                         df1$MandateRegion==colz[j]),])
    }
  }
  return(m1)
}