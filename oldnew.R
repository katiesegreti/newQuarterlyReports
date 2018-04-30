is.even <- function(x){x %%2 == 0}
is.odd <- function(x){x %%2 == 1}
status <- c("Potential", "New", "Completed")
status2 <- rep(status, each = 2)

is.numeric(USQ$MandateSizeAmount)

PNCtableOLD <- function(df1){
  AC <- (levels(droplevels(df1$MainAssetClass)))
  status <- c("Potential", "New", "Completed")
  status2 <- rep(status, each = 2)
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
        m1[j,i] <- nrow(df1[df1$SearchStatus==status2[i]&df1$MainAssetClass==AC[j],])
      }
    }
  }
  return(m1)
}
PNCtableOLD(USQ)
str(USQ$MandateSizeAmount)
AC <- (levels(droplevels(USQ$MainAssetClass)))
z <- rep(0, length(AC)*length(status)*2)
m1 <- matrix(z, nrow=length(AC), ncol=length(status2))
rownames(m1) <- AC
colnames(m1) <- status2


for(i in 1:length(status2)){
  for(j in 1:length(AC)){
    if(is.odd(i)){
      m1[j,i] <- nrow(USQ[USQ$SearchStatus==status2[i]&USQ$MainAssetClass==AC[j],])
    }else{
      m1[j,i] <- max(USQ[USQ$SearchStatus==status2[i]&USQ$MainAssetClass==AC[j],
                         "MandateSizeAmount"], na.rm = TRUE)
    }
  }
}

cash <- USQ %>%
  filter(MainAssetClass == "Cash")



sum(USQ[USQ$SearchStatus==status2[3]&USQ$MainAssetClass=="Equity","MandateSizeAmount"],
    na.rm = TRUE)

ACbyMRtableOLD <- function(df1){
  AC <- (levels(droplevels(factor(df1$MainAssetClass))))
  MR <- (levels(droplevels(factor(df1$MandateRegion))))
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
        m1[i,j] <- sum(df1[df1$MainAssetClass==AC[i]&df1$MandateRegion==MR2[j],"MandateSizeAmount"],
                       na.rm = TRUE)
      }
    }
  }
  #get totals
  for(i in 1:length(AC)){
    m1[i, length(MR2)-1] <- nrow(df1[df1$MainAssetClass==AC[i],])
    m1[i,length(MR2)] <- mean(df1[df1$MainAssetClass==AC[i],"MandateSizeAmount"], na.rm = TRUE)
  }
  return(m1)
}
g <- ACbyMRtableOLD(USmandates)


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
YearTable(mandates)
?nrow
