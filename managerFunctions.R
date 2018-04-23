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
##----function #2  select asset class for manager table (default sorts by $, enter sortz to sort by #)
mgrTableAC <- function(df0, assetclass, sortz = 1) {
  df1 <- filter(df0, MainAssetClass == assetclass)
  mgrTable(df1, sortz = sortz)
}
##mgrTable for subasset classes
mgrTableSAC <- function(df0, subclass, sortz = 1) {
  df1 <- filter(df0, SubAssetClass == subclass)
  mgrTable(df1, sortz = sortz)
}


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

testAClist <- USQroundup$MainAssetClass

## topMgr function used in roundup2
topMgr <- function(df1, assetclass) {
  top <- mgrTableAC(df1, assetclass, sortz = 2)
  if(length(top) > 1 & is.na(top[1,1])) {
    return(top[2,])
  } else{
  return(top[1,])
  }
}
mgrTableAC(USmanagersY, "Equity")
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



#-----function4  returns top manager (by disclosed value), used in topManagers other ac
topMgrAC <- function(df1, subclass) {
  top <- mgrTableSAC(df1, subclass, sortz = 2)
  if(length(top) > 1 & is.na(top[1,1])) {
    return(top[2,])
  } else{
    return(top[1,])
  }
}


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
roundup2AC(USFImanagersY)
roundup2AC <- function(df1) {
  AC <- levels(droplevels(df1$SubAssetClass))
  mgrz <- topManagersAC(df1)
  dfnew <- data.frame("AssetClass" = AC, "total1" = rep(0, length(AC)), "total2" = rep(0, length(AC)), 
                      "topMgr" = rep(0, length(AC)), "mgrTotal" = rep(0, length(AC)))
  for(i in 1:length(AC)){
    dfnew[i,2] <- nrow(df1[df1$SubAssetClass==AC[i],])
    dfnew[i,3] <- sum(df1[df1$SubAssetClass==AC[i], "AccountSizeAmount"])
    dfnew[i,4] <- mgrz[i]
    dfnew[i,5] <- sum(df1[df1$SubAssetClass==AC[i] & df1$AssetManager==mgrz[i], "AccountSizeAmount"])
  }
  return(dfnew)
}






#-----function4  returns top manager (by disclosed value), used in topManagers AC
topmgrIR <- function(df0, subclass){
  df1 <- df0[df0$MandateRegion==subclass,]
  MGR <- levels(droplevels(df1$AssetManager))
  totals <- c("total1", "total2")
  z <- rep(0, length(MGR)*length(totals))
  #create matrix
  m1 <- matrix(z, nrow=length(MGR), ncol=length(totals))
  rownames(m1) <- MGR
  colnames(m1) <- totals
  #fill matrix
  if(length(MGR)==1){
    return(MGR)
  }
  for(i in 1:length(MGR)){
    m1[i,1] <- nrow(df1[df1$AssetManager==MGR[i],])
    m1[i,2] <- sum(df1[df1$AssetManager==MGR[i], "AccountSizeAmount"])
  }
  m2 <- m1[order(m1[,2],m1[,1],decreasing=TRUE),]
  cnames <- rownames(m2)
  if(cnames[1]=="Unknown" & length(MGR)>1){
    return(cnames[2])
  }
  else{
    return(cnames[1])  
  }
}

#------function5 calls topmgr to get a list of top managers by asset class, used in roundup2
topManagersIR <- function(df1){
  AC <- levels(droplevels(df1$MandateRegion))
  topmgrz <- vector()
  for(i in 1:length(AC)){
    mgr <- topmgrIR(df1, AC[i])
    if(is.null(mgr)){
      topmgrz <- c(topmgrz, "Unknown")
    }else{
      topmgrz <- c(topmgrz, mgr)
    }
  }
  return(topmgrz)
}

#------function6 totals by asset class, with top managers for each asset class. calls topManagers.
roundup2IR <- function(df1) {
  AC <- levels(droplevels(df1$MandateRegion))
  mgrz <- topManagersIR(df1)
  dfnew <- data.frame("AssetClass" = AC, "total1" = rep(0, length(AC)), "total2" = rep(0, length(AC)), 
                      "topMgr" = rep(0, length(AC)), "mgrTotal" = rep(0, length(AC)))
  for(i in 1:length(AC)){
    dfnew[i,2] <- nrow(df1[df1$MandateRegion==AC[i],])
    dfnew[i,3] <- sum(df1[df1$MandateRegion==AC[i], "AccountSizeAmount"])
    dfnew[i,4] <- mgrz[i]
    dfnew[i,5] <- sum(df1[df1$MandateRegion==AC[i] & df1$AssetManager==mgrz[i], "AccountSizeAmount"])
  }
  return(dfnew)
}

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


#-----function4  returns top manager (by disclosed value), used in topManagers EQUITY
topmgrCS <- function(df0, subclass){
  df1 <- df0[df0$CapSize==subclass,]
  MGR <- levels(droplevels(df1$AssetManager))
  totals <- c("total1", "total2")
  z <- rep(0, length(MGR)*length(totals))
  #create matrix
  m1 <- matrix(z, nrow=length(MGR), ncol=length(totals))
  rownames(m1) <- MGR
  colnames(m1) <- totals
  #fill matrix
  if(length(MGR)==1){
    return(MGR)
  }
  for(i in 1:length(MGR)){
    m1[i,1] <- nrow(df1[df1$AssetManager==MGR[i],])
    m1[i,2] <- sum(df1[df1$AssetManager==MGR[i], "AccountSizeAmount"])
  }
  m2 <- m1[order(m1[,2],m1[,1],decreasing=TRUE),]
  cnames <- rownames(m2)
  return(cnames[1])
}

#------function5 calls topmgr to get a list of top managers by asset class, used in roundup2
topManagersCS <- function(df1){
  AC <- levels(droplevels(df1$CapSize))
  topmgrz <- vector()
  for(i in 1:length(AC)){
    mgr <- topmgrCS(df1, AC[i])
    if(is.null(mgr)){
      topmgrz <- c(topmgrz, "Unknown")
    }else{
      topmgrz <- c(topmgrz, mgr)
    }
  }
  return(topmgrz)
}

#------function6 totals by asset class, with top managers for each asset class. calls topManagers.
roundup2CS <- function(df1) {
  AC <- levels(droplevels(df1$CapSize))
  mgrz <- topManagersCS(df1)
  dfnew <- data.frame("AssetClass" = AC, "total1" = rep(0, length(AC)), "total2" = rep(0, length(AC)), 
                      "topMgr" = rep(0, length(AC)), "mgrTotal" = rep(0, length(AC)))
  for(i in 1:length(AC)){
    dfnew[i,2] <- nrow(df1[df1$CapSize==AC[i],])
    dfnew[i,3] <- sum(df1[df1$CapSize==AC[i], "AccountSizeAmount"])
    dfnew[i,4] <- mgrz[i]
    dfnew[i,5] <- sum(df1[df1$CapSize==AC[i] & df1$AssetManager==mgrz[i], "AccountSizeAmount"])
  }
  return(dfnew)
}





#-----function4  returns top manager (by disclosed value), used in topManagers EQUITY
topmgrSS <- function(df0, subclass){
  df1 <- df0[df0$Style==subclass,]
  MGR <- levels(droplevels(df1$AssetManager))
  totals <- c("total1", "total2")
  z <- rep(0, length(MGR)*length(totals))
  #create matrix
  m1 <- matrix(z, nrow=length(MGR), ncol=length(totals))
  rownames(m1) <- MGR
  colnames(m1) <- totals
  #fill matrix
  if(length(MGR)==1){
    return(MGR)
  }
  for(i in 1:length(MGR)){
    m1[i,1] <- nrow(df1[df1$AssetManager==MGR[i],])
    m1[i,2] <- sum(df1[df1$AssetManager==MGR[i], "AccountSizeAmount"])
  }
  m2 <- m1[order(m1[,2],m1[,1],decreasing=TRUE),]
  cnames <- rownames(m2)
  return(cnames[1])
}

#------function5 calls topmgr to get a list of top managers by asset class, used in roundup2
topManagersSS <- function(df1){
  AC <- levels(droplevels(df1$Style))
  topmgrz <- vector()
  for(i in 1:length(AC)){
    mgr <- topmgrSS(df1, AC[i])
    if(is.null(mgr)){
      topmgrz <- c(topmgrz, "Unknown")
    }else{
      topmgrz <- c(topmgrz, mgr)
    }
  }
  return(topmgrz)
}

#------function6 totals by asset class, with top managers for each asset class. calls topManagers.
roundup2SS <- function(df1) {
  AC <- levels(droplevels(df1$Style))
  mgrz <- topManagersSS(df1)
  dfnew <- data.frame("AssetClass" = AC, "total1" = rep(0, length(AC)), "total2" = rep(0, length(AC)), 
                      "topMgr" = rep(0, length(AC)), "mgrTotal" = rep(0, length(AC)))
  for(i in 1:length(AC)){
    dfnew[i,2] <- nrow(df1[df1$Style==AC[i],])
    dfnew[i,3] <- sum(df1[df1$Style==AC[i], "AccountSizeAmount"])
    dfnew[i,4] <- mgrz[i]
    dfnew[i,5] <- sum(df1[df1$Style==AC[i] & df1$AssetManager==mgrz[i], "AccountSizeAmount"])
  }
  return(dfnew)
}


