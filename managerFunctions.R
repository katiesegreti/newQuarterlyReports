
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
    replace_na(list(AccountSizeAmount = 0)) %>%
    summarise(total = sum(AccountSizeAmount)) %>%
    spread(!!y, total) %>%
    replace(., is.na(.), 0)
  return(cbind(df1, Total1 = rowSums(df1[-1], na.rm = TRUE)))
}


# updated
##------function #1 manager table sorted (default sorts by $, enter sortz to sort by #)
mgrTable <- function(df1, sortz = 1) {
  mgrz <- df1 %>%
    replace_na(list(AccountSizeAmount = 0)) %>%
    group_by(AssetManager) %>%
    summarise(Total = n(), SumTotal = sum(AccountSizeAmount)) %>%
    replace(., is.na(.), 0)
  if(sortz != 1) {
    mgrz <- arrange(mgrz, desc(SumTotal), desc(Total))
  } else {
    mgrz <- arrange(mgrz, desc(Total), desc(SumTotal))
  }
  return(mgrz)
}
mgrtabletestUSQ <- mgrTable(USmanagersQ)

## UPDATED 
###CAN THIS BE REPLACED BY mgrTableX?
##----function #2  select asset class for manager table (default sorts by $, enter sortz to sort by #)
mgrTableAC <- function(df0, assetclass, sortz = 1) {
  df1 <- filter(df0, MainAssetClass == assetclass)
  mgrTable(df1, sortz = sortz)
}
mgrTableAC(USmanagersY, "Real Assets")
##mgrTable for subasset classes 
###CAN THIS BE REPLACED BY mgrTableX?
mgrTableSAC <- function(df0, subclass, sortz = 1) {
  df1 <- filter(df0, SubAssetClass == subclass)
  mgrTable(df1, sortz = sortz)
}
mgrTableSAC(USFImanagersY, "Core")



## this one is good
mgrTableX <- function(df0, colname, subclass, sortz = 1) {
  colname <- enquo(colname)
  df1 <- filter(df0, (!!colname) == subclass)
  mgrTable(df1, sortz = sortz)
  
}

mgrTableX(USmanagersQ, MainAssetClass, "Fixed Income")
##------function3 Asset Class totals
## UPDATED
roundup <- function(df1) {
  rndp <- df1 %>%
    replace_na(list(AccountSizeAmount = 0)) %>%
    group_by(MainAssetClass) %>%
    summarise(Total = n(), SumTotal = sum(AccountSizeAmount))
}
USQroundup <- roundup(USmanagersQ)
USYroundup <- roundup(USmanagersY)


## topMgr function used in roundup2
topMgr <- function(df1, assetclass) {
  top <- mgrTableAC(df1, assetclass, sortz = 2)
  if(length(top) > 1 & is.na(top[1,1])) {
    return(top[2,])
  } else{
  return(top[1,])
  }
}
mgrTableAC(USmanagersY, "Equity", sortz = 2)
topMgr(USmanagersY, "Equity")




#------roundup2 calls roundup and topMgr
roundup2 <- function(df1) {
  rndp1 <- roundup(df1)
  rndp1$Manager <- rep("", nrow(rndp1))
  rndp1$MgrTotal1 <- rep(0, nrow(rndp1))
  rndp1$MgrTotal2 <- rep(0, nrow(rndp1))
  for(i in 1:nrow(rndp1)) {
    rndp1$Manager[i] <- topMgr(df1, rndp1$MainAssetClass[i])[[1]]
    rndp1$MgrTotal1[i] <- topMgr(df1, rndp1$MainAssetClass[i])[[2]]
    rndp1$MgrTotal2[i] <- topMgr(df1, rndp1$MainAssetClass[i])[[3]]
  }
  return(rndp1)
}


roundup2(USmanagersY)



##UPDATED
#--------function7 Month Tables (by # of mandates is default, enter sortz to sort by $)
monthTable <- function(df1, sortz=1) {
  tbl1 <- totalN(df1, AssetManager, Month)
  tbl2 <- totalSum(df1, AssetManager, Month)
  if(sortz == 1){
    totalz <- data.frame(AssetManager = tbl2$AssetManager, Total1 = tbl2$Total1)
    combined <- tbl1 %>% left_join(totalz)
    combined <- arrange(combined, desc(Total), desc(Total1))
  } else {
    totalz <- data.frame(AssetManager = tbl1$AssetManager, Total = tbl1$Total)
    combined <- tbl2 %>% left_join(totalz)
    combined <- arrange(combined, desc(Total1), desc(Total))
  }
  return(combined)
}
monthtest1 <- monthTable(USmanagersY)
monthtest2 <- monthTable(USmanagersY, sortz = 2)



#function8 manager matrix by # of mandates (by # of mandates is default, enter sortz to sort by $)
## UPDATED
managerMatrix <- function(df1, sortz = 1) {
  if(sortz == 1){
    tbl1 <- totalN(df1, AssetManager, FundType)
    tbl2 <- totalN(df1, AssetManager, MainAssetClass)
    tbl3 <- totalN(df1, AssetManager, MandateRegion)
  } else {
    tbl1 <- totalSum(df1, AssetManager, FundType)
    tbl2 <- totalSum(df1, AssetManager, MainAssetClass)
    tbl3 <- totalSum(df1, AssetManager, MandateRegion)
  }
  combined <- tbl1 %>% left_join(tbl2)
  combined <- combined %>% left_join(tbl3)
}
USYmx2 <- managerMatrix(USmanagersY, sortz = 2)


#function8 manager matrix by # of mandates (by # of mandates is default, enter sortz to sort by $)
## UPDATED
managerMatrixEQ <- function(df0, sortz = 1) {
  df1 <- df0 %>% filter(MainAssetClass == "Equity")
  if(sortz == 1){
    tbl1 <- totalN(df1, AssetManager, FundType)
    tbl2 <- totalN(df1, AssetManager, CapSize)
    tbl3 <- totalN(df1, AssetManager, Style)
    tbl4 <- totalN(df1, AssetManager, MandateRegion)
  } else {
    tbl1 <- totalSum(df1, AssetManager, FundType)
    tbl2 <- totalSum(df1, AssetManager, CapSize)
    tbl3 <- totalSum(df1, AssetManager, Style)
    tbl4 <- totalSum(df1, AssetManager, MandateRegion)
  }
  combined <- tbl1 %>% left_join(tbl2)
  combined <- combined %>% left_join(tbl3)
  combined <- combined %>% left_join(tbl4)
}
USYeqMX <- managerMatrixEQ(USmanagersY)

#function8 manager matrix by # of mandates (by # of mandates is default, enter sortz to sort by $)
## UPDATED
managerMatrixAC <- function(df0, assetclass, sortz = 1) {
  df1 <- df0 %>% filter(MainAssetClass == assetclass)
  if(sortz == 1){
    tbl1 <- totalN(df1, AssetManager, FundType)
    tbl2 <- totalN(df1, AssetManager, SubAssetClass)
    tbl3 <- totalN(df1, AssetManager, MandateRegion)
  } else {
    tbl1 <- totalSum(df1, AssetManager, FundType)
    tbl2 <- totalSum(df1, AssetManager, SubAssetClass)
    tbl3 <- totalSum(df1, AssetManager, MandateRegion)
  }
  combined <- tbl1 %>% left_join(tbl2)
  combined <- combined %>% left_join(tbl3)
}
USYfiMX <- managerMatrixAC(USmanagersY, "Fixed Income")



roundupAC <- function(df1) {
  rndp <- df1 %>%
    replace_na(list(AccountSizeAmount = 0)) %>%
    group_by(SubAssetClass) %>%
    summarise(Total = n(), SumTotal = sum(AccountSizeAmount))
}

roundupX <- function(df1, x) {
  x <- enquo(x)
  rndp <- df1 %>%
    replace_na(list(AccountSizeAmount = 0)) %>%
    group_by(!!x) %>%
    summarise(Total = n(), SumTotal = sum(AccountSizeAmount))
}
t <- roundupX(USFImanagersQ, SubAssetClass)

#-----function4  returns top manager (by disclosed value), used in topManagers other ac
topMgrAC <- function(df1, subclass) {
  top <- mgrTableAC(df1, subclass, sortz = 2)
  if(length(top) > 1 & is.na(top[1,1])) {
    return(top[2,])
  } else{
    return(top[1,])
  }
}



topMgrAC(USmanagersY, "Equity")

topMgrX <- function(df1, colname, subclass) {
  colname <- enquo(colname)
  top <- mgrTableX(df1, !!colname, subclass, sortz = 2)
  if(length(top) > 1 & is.na(top[1,1])) {
    return(top[2,])
  } else{
    return(top[1,])
  }
}
topMgrX(USmanagersY, SubAssetClass, "Real Estate")
##-- REDO THIS ONE TO USE ROUNDUPX AND TOPMGRX
#------function6 totals by asset class, with top managers for each asset class. calls topManagers.
roundup2AC <- function(df1) {
  rndp1 <- roundupAC(df1) 
  rndp1$Manager <- rep("", nrow(rndp1))
  rndp1$MgrTotal1 <- rep(0, nrow(rndp1))
  rndp1$MgrTotal2 <- rep(0, nrow(rndp1))
  for(i in 1:nrow(rndp1)) {
    rndp1$Manager[i] <- topMgrAC(df1, rndp1$SubAssetClass[i])[[1]]
    rndp1$MgrTotal1[i] <- topMgrAC(df1, rndp1$SubAssetClass[i])[[2]]
    rndp1$MgrTotal2[i] <- topMgrAC(df1, rndp1$SubAssetClass[i])[[3]]
  }
  return(rndp1)
}
rndup2ACtest <- roundup2AC(USFImanagersY)

#--- UPDATED
#------function6 totals by asset class, with top managers for each asset class. calls topManagers.
roundup2IR <- function(df1) {
  rndp1 <- roundupX(df1, MandateRegion)
  rndp1$Manager <- rep("", nrow(rndp1))
  rndp1$MgrTotal1 <- rep(0, nrow(rndp1))
  rndp1$MgrTotal2 <- rep(0, nrow(rndp1))
  for(i in 1:nrow(rndp1)) {
    rndp1$Manager[i] <- topMgrX(df1, MandateRegion, rndp1$MandateRegion[i])[[1]]
    rndp1$MgrTotal1[i] <- topMgrX(df1, MandateRegion, rndp1$MandateRegion[i])[[2]]
    rndp1$MgrTotal2[i] <- topMgrX(df1, MandateRegion, rndp1$MandateRegion[i])[[3]]
  }
  return(rndp1)
}
IRrndp <- roundup2IR(USmanagersY)

#--- UPDATED
#------ equity capsize roundup2
roundup2CS <- function(df1) {
  rndp1 <- roundupX(df1, CapSize)
  rndp1$Manager <- rep("", nrow(rndp1))
  rndp1$MgrTotal1 <- rep(0, nrow(rndp1))
  rndp1$MgrTotal2 <- rep(0, nrow(rndp1))
  for(i in 1:nrow(rndp1)) {
    rndp1$Manager[i] <- topMgrX(df1, CapSize, rndp1$CapSize[i])[[1]]
    rndp1$MgrTotal1[i] <- topMgrX(df1, CapSize, rndp1$CapSize[i])[[2]]
    rndp1$MgrTotal2[i] <- topMgrX(df1, CapSize, rndp1$CapSize[i])[[3]]
  }
  return(rndp1)
}
roundup2CS(USeqManagersY)

#--- UPDATED
#------ equity style roundup2
roundup2SS <- function(df1) {
  rndp1 <- roundupX(df1, Style)
  rndp1$Manager <- rep("", nrow(rndp1))
  rndp1$MgrTotal1 <- rep(0, nrow(rndp1))
  rndp1$MgrTotal2 <- rep(0, nrow(rndp1))
  for(i in 1:nrow(rndp1)) {
    rndp1$Manager[i] <- topMgrX(df1, Style, rndp1$Style[i])[[1]]
    rndp1$MgrTotal1[i] <- topMgrX(df1, Style, rndp1$Style[i])[[2]]
    rndp1$MgrTotal2[i] <- topMgrX(df1, Style, rndp1$Style[i])[[3]]
  }
  return(rndp1)
}
roundup2SS(USeqManagersY)


#function0 manager matrix by # of mandates (by # of mandates is default, enter sortz to sort by $) OA
managerMatrixAC2 <- function(df1, sortz=1){
  MGR <- levels(droplevels(df1$AssetManager))
  FT <- levels(droplevels(df1$FundType))
  AC <- levels(droplevels(df1$MainAssetClass))
  RG <- levels(droplevels(df1$MandateRegion))
  ACz <- vector() #for full list of main asset class
  SAC <- vector()
  SACz <- vector()
  ACSACz <- vector() #combined main and sub
  sNum <- vector() #number of sub asset classes in each AC
  #get sub asset class list  (SACz)
  for(i in 1:length(AC)){
    dftemp <- df1[df1$MainAssetClass==AC[i],]
    SAC <- levels(droplevels(dftemp$SubAssetClass))
    sNum <- c(sNum, length(SAC))
    SACz <- c(SACz, SAC)
  }
  columnz <- c("total", FT, SACz, RG)
  z <- rep(0, length(MGR)*length(columnz))
  #get main asset class list (ACz)
  for(i in 1:length(sNum)){
    ACz <- c(ACz, rep(AC[i], times=1,each=sNum[i]))
  }
  #get asset class/subasset class combo list ACSACz <- c(ACSACz, paste(ACz[j],SACz[j])) 
  for(i in 1:sum(sNum)){
    ACSACz <- c(ACSACz, paste(ACz[i],SACz[i])) 
  }
  ACz2 <- c("total", FT, ACz, RG)
  #create matrix
  m1 <- matrix(z, nrow=length(MGR), ncol=length(columnz))
  
  rownames(m1) <- MGR
  #fill matrix
  if(sortz==1){
    for(i in 1:length(MGR)){
      m1[i,1] <- nrow(df1[df1$AssetManager==MGR[i],])
      for(j in 2:length(columnz)){
        m1[i,j] <- nrow(df1[df1$AssetManager==MGR[i]&(df1$FundType==columnz[j]|
                                                                  (df1$SubAssetClass==columnz[j]&df1$MainAssetClass==ACz2[j])|
                                                                  
                                                                  df1$MandateRegion==columnz[j]),])
        
      }
      
    }
  }else{
    for(i in 1:length(MGR)){
      m1[i,1] <- sum(df1[df1$AssetManager==MGR[i],"AccountSizeAmount"])
      for(j in 2:length(columnz)){
        m1[i,j] <- sum(df1[df1$AssetManager==MGR[i]&(df1$FundType==columnz[j]|
                                                                 (df1$SubAssetClass==columnz[j]&df1$MainAssetClass==ACz2[j])|
                                                                 
                                                                 df1$MandateRegion==columnz[j]),"AccountSizeAmount"])
      }
      
    }
  }
  columnz2 <- c("total", FT, ACSACz, RG)
  colnames(m1) <- columnz2
  return(m1)
}








