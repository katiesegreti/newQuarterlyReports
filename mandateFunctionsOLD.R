#--------------------------------------------

#is even and is odd functions
is.even <- function(x){x %%2 == 0}
is.odd <- function(x){x %%2 == 1}

#-----function
#-----------QUARTERLY MANDATE SUMMARY - POTENTIAL, NEW, COMPLETED
#----FUNCTION: POTENTIAL NEW COMPLETED ASSET CLASS QUARTER MATRIX  USQ,USPNC1,USPNC2
PNCtable <- function(df1){
  AC <- (levels(droplevels(df1$MainAssetClass)))
  z <- rep(0, length(AC)*length(status)*2)
  #create matrix
  m1 <- matrix(z, nrow=length(AC), ncol=length(status2))
  rownames(m1) <- AC
  colnames(m1) <- status2
  #fill matrix
  for(i in 1:length(status2)){
    for(j in 1:length(AC)){
      if(is.odd(i)){
        m1[j,i] <- nrow(df1[df1$SearchStatus==status2[i]&df1$MainAssetClass==AC[j],])
      }else{
        m1[j,i] <- sum(df1[df1$SearchStatus==status2[i]&df1$MainAssetClass==AC[j],"MandateSizeAmount"])
      }
    }
  }
  return(m1)
}
#----FUNCTION: FUND TYPE BY ASSET CLASS AND MANDATE REGION
FTtable <- function(df1){
  AC <- (levels(droplevels(df1$MainAssetClass)))
  FT <- (levels(droplevels(df1$FundType)))
  MR <- (levels(droplevels(df1$MandateRegion)))
  ACMR <- c("Total", AC, MR)
  z <- rep(0, length(ACMR)*length(FT))
  #create matrix
  m1 <- matrix(z, nrow=length(FT), ncol=length(ACMR))
  rownames(m1) <- FT
  colnames(m1) <- ACMR
  #fill matrix
  for(i in 1:length(FT)){
    for(j in 2:length(ACMR)){
      m1[i,j] <- nrow(df1[df1$FundType==FT[i]&(df1$MainAssetClass==ACMR[j]|df1$MandateRegion==ACMR[j]),])
    }
  }
  #get totlas
  for(i in 1:length(FT)){
    m1[i,1] <- nrow(df1[df1$FundType==FT[i],])
  }
  return(m1)
}

#---- Asset class by investment region
ACbyMRtable <- function(df1){
  AC <- (levels(droplevels(df1$MainAssetClass)))
  MR <- (levels(droplevels(df1$MandateRegion)))
  MR2 <- rep(MR,times=1, each=2)
  MR2 <- c(MR2, "total#", "total$")
  z <- rep(0, length(AC)*length(MR2))
  #create matrix
  m1 <- matrix(z, nrow=length(AC), ncol=length(MR2))
  rownames(m1) <- AC
  colnames(m1) <- MR2
  #fill matrix
  for(i in 1:length(AC)){
    for(j in 1:length(MR2)){
      if(is.odd(j)){
        m1[i,j] <- nrow(df1[df1$MainAssetClass==AC[i]&df1$MandateRegion==MR2[j],])
      }else{
        m1[i,j] <- sum(df1[df1$MainAssetClass==AC[i]&df1$MandateRegion==MR2[j],"MandateSizeAmount"])
      }
    }
  }
  #get totals
  for(i in 1:length(AC)){
    m1[i, length(MR2)-1] <- nrow(df1[df1$MainAssetClass==AC[i],])
    m1[i,length(MR2)] <- sum(df1[df1$MainAssetClass==AC[i],"MandateSizeAmount"])
  }
  return(m1)
}
#---- SubAsset class by investment region for assete class
SACbyMRtable <- function(df1, assetclass){
  dftemp <- df1[df1$MainAssetClass == assetclass,]
  SAC <- (levels(droplevels(dftemp$SubAssetClass)))
  
  MR <- (levels(droplevels(dftemp$MandateRegion)))
  MR2 <- rep(MR,times=1, each=2)
  MR2 <- c(MR2, "total#", "total$")
  z <- rep(0, length(SAC)*length(MR2))
  #create matrix
  m1 <- matrix(z, nrow=length(SAC), ncol=length(MR2))
  rownames(m1) <- SAC
  colnames(m1) <- MR2
  #fill matrix
  for(i in 1:length(SAC)){
    for(j in 1:length(MR2)){
      if(is.odd(j)){
        m1[i,j] <- nrow(dftemp[dftemp$SubAssetClass==SAC[i]&dftemp$MandateRegion==MR2[j],])
      }else{
        m1[i,j] <- sum(dftemp[dftemp$SubAssetClass==SAC[i]&dftemp$MandateRegion==MR2[j],"MandateSizeAmount"])
      }
    }
  }
  #get totals
  for(i in 1:length(SAC)){
    m1[i, length(MR2)-1] <- nrow(dftemp[dftemp$SubAssetClass==SAC[i],])
    m1[i, length(MR2)] <- sum(dftemp[dftemp$SubAssetClass==SAC[i],"MandateSizeAmount"])
  }
  return(m1)
}


#---- Investment approach by investment region for equity and fixed Income
IAbyMRtable <- function(df1, assetclass){
  dftemp <- df1[df1$MainAssetClass == assetclass,]
  IA <- (levels(droplevels(dftemp$InvestmentApproach)))
  
  MR <- (levels(droplevels(dftemp$MandateRegion)))
  MR2 <- rep(MR,times=1, each=2)
  MR2 <- c(MR2, "Total#", "Total$")
  z <- rep(0, length(IA)*length(MR2))
  #create matrix
  m1 <- matrix(z, nrow=length(IA), ncol=length(MR2))
  rownames(m1) <- IA
  colnames(m1) <- MR2
  #fill matrix
  for(i in 1:length(IA)){
    for(j in 1:length(MR2)){
      if(is.odd(j)){
        m1[i,j] <- nrow(dftemp[dftemp$InvestmentApproach==IA[i]&dftemp$MandateRegion==MR2[j],])
      }else{
        m1[i,j] <- sum(dftemp[dftemp$InvestmentApproach==IA[i]&dftemp$MandateRegion==MR2[j],"MandateSizeAmount"])
      }
    }
  }
  #get totals
  for(i in 1:length(IA)){
    m1[i, length(MR2)-1] <- nrow(dftemp[dftemp$InvestmentApproach==IA[i],])
    m1[i, length(MR2)] <- sum(dftemp[dftemp$InvestmentApproach==IA[i],"MandateSizeAmount"])
  }
  return(m1)
}
#---- style by investment region for equity 
EQStyletable <- function(df1){
  dftemp <- df1[df1$MainAssetClass == "Equity",]
  style <- (levels(droplevels(dftemp$Style)))
  
  MR <- (levels(droplevels(dftemp$MandateRegion)))
  MR2 <- rep(MR,times=1, each=2)
  MR2 <- c(MR2, "Totla#", "Total$")
  z <- rep(0, length(style)*length(MR2))
  #create matrix
  m1 <- matrix(z, nrow=length(style), ncol=length(MR2))
  rownames(m1) <- style
  colnames(m1) <- MR2
  #fill matrix
  for(i in 1:length(style)){
    for(j in 1:length(MR2)){
      if(is.odd(j)){
        m1[i,j] <- nrow(dftemp[dftemp$Style==style[i]&dftemp$MandateRegion==MR2[j],])
      }else{
        m1[i,j] <- sum(dftemp[dftemp$Style==style[i]&dftemp$MandateRegion==MR2[j],"MandateSizeAmount"])
      }
    }
  }
  #get totals
  for(i in 1:length(style)){
    m1[i, length(MR2)-1] <- nrow(dftemp[dftemp$Style==style[i],])
    m1[i, length(MR2)] <- sum(dftemp[dftemp$Style==style[i],"MandateSizeAmount"])
  }
  return(m1)
}

EQCaptable <- function(df1){
  dftemp <- df1[df1$MainAssetClass == "Equity",]
  stylee <- levels(droplevels(dftemp$Style))
  MR <- levels(droplevels(dftemp$MandateRegion))
  MR2 <- rep(MR, times=1, each=2)
  MR2 <- c(MR2, "Total#", "Total$")
  stylez <- vector()   #for full list of main asset classes
  CAP <- vector() 
  CAPz <- vector() #for full list of sub asset classes by main asset class
  capstylez <- vector() #combined main and sub for rownames
  sNum <- vector() #number of sub asset classes in each stylee
  #get sub asset class list (CAPz)
  for(i in 1:length(stylee)){
    dftempa <- dftemp[dftemp$Style==stylee[i],]
    CAP <- levels(droplevels(dftempa$CapSize))
    sNum <- c(sNum, length(CAP))
    CAPz <- c(CAPz, CAP)
  }
  #get main asset class list (stylez)
  for(i in 1:length(sNum)){
    stylez <- c(stylez, rep(stylee[i], times=1,each=sNum[i]))
  }
  #fill matrix
  z <- rep(0, length(CAPz)*length(MR2))
  m1 <- matrix(z, nrow=length(CAPz), ncol=length(MR2))
  colnames(m1) <- MR2
  for(i in 1:length(CAPz)){
    for(j in 1:length(MR2)){
      if(is.odd(j)){
        m1[i,j] <- nrow(dftemp[dftemp$Style==stylez[i]&dftemp$CapSize==CAPz[i]&dftemp$MandateRegion==MR2[j],])
      }else{
        m1[i,j] <- sum(dftemp[dftemp$Style==stylez[i]&dftemp$CapSize==CAPz[i]&dftemp$MandateRegion==MR2[j],"MandateSizeAmount"])
      }
    }
    capstylez <- c(capstylez, paste(stylez[i],CAPz[i]))
  }
  rownames(m1) <- capstylez
  #get totals
  for(i in 1:length(capstylez)){
    m1[i,length(MR2)-1] <- nrow(dftemp[dftemp$Style==stylez[i]&dftemp$CapSize==CAPz[i],])
    m1[i, length(MR2)] <- sum(dftemp[dftemp$Style==stylez[i]&dftemp$CapSize==CAPz[i], "MandateSizeAmount"])
  }
  return(m1)
}

AltsTable <- function(df1){
  dftemp <- df1[df1$MainAssetClass%in%alternatives,]
  AC <- levels(droplevels(dftemp$MainAssetClass))
  MR <- levels(droplevels(dftemp$MandateRegion))
  MR2 <- rep(MR, times=1, each=2)
  MR2 <- c(MR2, "Total#", "Total$")
  ACz <- vector()   #for full list of main asset classes
  SAC <- vector() 
  SACz <- vector() #for full list of sub asset classes by main asset class
  ACSACz <- vector() #combined main and sub for rownames
  sNum <- vector() #number of sub asset classes in each AC
  #get sub asset class list (SACz)
  for(i in 1:length(AC)){
    dftempa <- dftemp[dftemp$MainAssetClass==AC[i],]
    SAC <- levels(droplevels(dftempa$SubAssetClass))
    sNum <- c(sNum, length(SAC))
    SACz <- c(SACz, SAC)
  }
  #get main asset class list (ACz)
  for(i in 1:length(sNum)){
    ACz <- c(ACz, rep(AC[i], times=1,each=sNum[i]))
  }
  #fill matrix
  z <- rep(0, length(SACz)*length(MR2))
  m1 <- matrix(z, nrow=length(SACz), ncol=length(MR2))
  colnames(m1) <- MR2
  for(i in 1:length(SACz)){
    for(j in 1:length(MR2)){
      if(is.odd(j)){
        m1[i,j] <- nrow(dftemp[dftemp$MainAssetClass==ACz[i]&dftemp$SubAssetClass==SACz[i]&dftemp$MandateRegion==MR2[j],])
      }else{
        m1[i,j] <- sum(dftemp[dftemp$MainAssetClass==ACz[i]&dftemp$SubAssetClass==SACz[i]&dftemp$MandateRegion==MR2[j],"MandateSizeAmount"])
      }
    }
    ACSACz <- c(ACSACz, paste(ACz[i],SACz[i]))
  }
  rownames(m1) <- ACSACz
  #get totals
  for(i in 1:length(ACSACz)){
    m1[i,length(MR2)-1] <- nrow(dftemp[dftemp$MainAssetClass==ACz[i]&dftemp$SubAssetClass==SACz[i],])
    m1[i, length(MR2)] <- sum(dftemp[dftemp$MainAssetClass==ACz[i]&dftemp$SubAssetClass==SACz[i], "MandateSizeAmount"])
  }
  return(m1)
}

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