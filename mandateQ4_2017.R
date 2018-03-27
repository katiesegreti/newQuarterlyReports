########################QUARTERLY MANDATE SUMMARY REPORT#######

#method 2: set WD and read data
getwd()
library(readr) # for read_csv()
library(purrr) # for map()
library(dplyr) # for joins
library(tidyr)
library(xlsx)
mandates1 <- read.csv("Q4_2017mandates.csv") ### old way
mandates <- read_csv("Q4_2017mandates.csv") ### new way



US_PNC1 <- USQ %>%
  group_by(SearchStatus) %>%
  summarize(total = n(), total_amount = sum(MandateSizeAmount))


USPtoPlot <- USQP %>%
  group_by(MainAssetClass) %>%
  summarize(total = n(), amount = sum(MandateSizeAmount))

ggplot(USPtoPlot, aes(x = total, y = amount, color = MainAssetClass)) + geom_point()


consultantz <- levels(mandates$SearchConsultant)
length(consultantz)



#mAKE TABLES
USPNC <- PNCtable(USQ)
#potential
USftP <- FTtable(USQP)
USacP <- ACbyMRtable(USQP)
USeq1P <- IAbyMRtable(USQP, "Equity")
USeq2P <- EQStyletable(USQP)
USeq3P <- EQCaptable(USQP)
USfi1P <- SACbyMRtable(USQP, "Fixed Income")
USfi2P <- IAbyMRtable(USQP, "Fixed Income")
UShfP <- SACbyMRtable(USQP, "Hedge Funds")
USpeP <- SACbyMRtable(USQP, "Private Equity")
USaltsP <- AltsTable(USQP)
USreP <- SACbyMRtable(USQP, "Real Estate")
#new
USftN <- FTtable(USQN)
USacN <- ACbyMRtable(USQN)
USeq1N <- IAbyMRtable(USQN, "Equity")
USeq2N <- EQStyletable(USQN)
USeq3N <- EQCaptable(USQN)
USfi1N <- SACbyMRtable(USQN, "Fixed Income")
USfi2N <- IAbyMRtable(USQN, "Fixed Income")
UShfN <- SACbyMRtable(USQN, "Hedge Funds")
USpeN <- SACbyMRtable(USQN, "Private Equity")
USaltsN <- AltsTable(USQN)
USreN <- SACbyMRtable(USQN, "Real Estate")
#completed
USftC <- FTtable(USQC)
USacC <- ACbyMRtable(USQC)
USeq1C <- IAbyMRtable(USQC, "Equity")
USeq2C <- EQStyletable(USQC)
USeq3C <- EQCaptable(USQC)
USfi1C <- SACbyMRtable(USQC, "Fixed Income")
USfi2C <- IAbyMRtable(USQC, "Fixed Income")
UShfC <- SACbyMRtable(USQC, "Hedge Funds")
USpeC <- SACbyMRtable(USQC, "Private Equity")
USaltsC <- AltsTable(USQC)
USreC <- SACbyMRtable(USQC, "Real Estate")



write.xlsx(USPNC, file = "USQ.xlsx", sheetName = "USPNC", append=TRUE)
write.xlsx(USftP, file = "USQ.xlsx", sheetName = "USftP", append=TRUE)
write.xlsx(USacP, file = "USQ.xlsx", sheetName = "USacP", append=TRUE)
write.xlsx(USeq1P, file = "USQ.xlsx", sheetName = "USeq1P", append=TRUE)
write.xlsx(USeq2P, file = "USQ.xlsx", sheetName = "USeq2P", append=TRUE)
write.xlsx(USeq3P, file = "USQ.xlsx", sheetName = "USeq3P", append=TRUE)
write.xlsx(USfi1P, file = "USQ.xlsx", sheetName = "USfi1P", append=TRUE)
write.xlsx(USfi2P, file = "USQ.xlsx", sheetName = "USfi2P", append=TRUE)
write.xlsx(UShfP, file = "USQ.xlsx", sheetName = "UShfP", append=TRUE)
write.xlsx(USpeP, file = "USQ.xlsx", sheetName = "USpeP", append=TRUE)
write.xlsx(USaltsP, file = "USQ.xlsx", sheetName = "USaltsP", append=TRUE)
write.xlsx(USreP, file = "USQ.xlsx", sheetName = "USreP", append=TRUE)

write.xlsx(USftN, file = "USQ.xlsx", sheetName = "USftN", append=TRUE)
write.xlsx(USacN, file = "USQ.xlsx", sheetName = "USacN", append=TRUE)
write.xlsx(USeq1N, file = "USQ.xlsx", sheetName = "USeq1N", append=TRUE)
write.xlsx(USeq2N, file = "USQ.xlsx", sheetName = "USeq2N", append=TRUE)
write.xlsx(USeq3N, file = "USQ.xlsx", sheetName = "USeq3N", append=TRUE)
write.xlsx(USfi1N, file = "USQ.xlsx", sheetName = "USfi1N", append=TRUE)
write.xlsx(USfi2N, file = "USQ.xlsx", sheetName = "USfi2N", append=TRUE)
write.xlsx(UShfN, file = "USQ.xlsx", sheetName = "UShfN", append=TRUE)
write.xlsx(USpeN, file = "USQ.xlsx", sheetName = "USpeN", append=TRUE)
write.xlsx(USaltsN, file = "USQ.xlsx", sheetName = "USaltsN", append=TRUE)
write.xlsx(USreN, file = "USQ.xlsx", sheetName = "USreN", append=TRUE)


write.xlsx(USftC, file = "USQ.xlsx", sheetName = "USftC", append=TRUE)
write.xlsx(USacC, file = "USQ.xlsx", sheetName = "USacC", append=TRUE)
write.xlsx(USeq1C, file = "USQ.xlsx", sheetName = "USeq1C", append=TRUE)
write.xlsx(USeq2C, file = "USQ.xlsx", sheetName = "USeq2C", append=TRUE)
write.xlsx(USeq3C, file = "USQ.xlsx", sheetName = "USeq3C", append=TRUE)
write.xlsx(USfi1C, file = "USQ.xlsx", sheetName = "USfi1C", append=TRUE)
write.xlsx(USfi2C, file = "USQ.xlsx", sheetName = "USfi2C", append=TRUE)
write.xlsx(UShfC, file = "USQ.xlsx", sheetName = "UShfC", append=TRUE)
write.xlsx(USpeC, file = "USQ.xlsx", sheetName = "USpeC", append=TRUE)
write.xlsx(USaltsC, file = "USQ.xlsx", sheetName = "USaltsC", append=TRUE)
write.xlsx(USreC, file = "USQ.xlsx", sheetName = "USreC", append=TRUE)


#year
USyrsPDB <- USmandates[USmandates$FundType=="Public D.B.",]
USacY <- ACbyMRtable(US12mo)
USftY <- FTtable(US12mo)
USeq1Y <- IAbyMRtable(US12mo, "Equity")
USeq2Y <- EQStyletable(US12mo)
USeq3Y <- EQCaptable(US12mo)
USfi1Y <- SACbyMRtable(US12mo, "Fixed Income")
USfi2Y <- IAbyMRtable(US12mo, "Fixed Income")
UShfY <- SACbyMRtable(US12mo, "Hedge Funds")
USpeY <- SACbyMRtable(US12mo, "Private Equity")
USaltsY <- AltsTable(US12mo)
USreY <- SACbyMRtable(US12mo, "Real Estate")
USmonths <- MonthTable(US12mo)
USyrs <- YearTable(US3y)
US3yrsPBD <- YearTable(USyrsPDB)

write.xlsx(USacY, file = "USY.xlsx", sheetName = "USacY", append=TRUE)
write.xlsx(USftY, file = "USY.xlsx", sheetName = "USftY", append=TRUE)
write.xlsx(USeq1Y, file = "USY.xlsx", sheetName = "USeq1Y", append=TRUE)
write.xlsx(USeq2Y, file = "USY.xlsx", sheetName = "USeq2Y", append=TRUE)
write.xlsx(USeq3Y, file = "USY.xlsx", sheetName = "USeq3Y", append=TRUE)
write.xlsx(USfi1Y, file = "USY.xlsx", sheetName = "USfi1Y", append=TRUE)
write.xlsx(USfi2Y, file = "USY.xlsx", sheetName = "USfi2Y", append=TRUE)
write.xlsx(UShfY, file = "USY.xlsx", sheetName = "UShfY", append=TRUE)
write.xlsx(USpeY, file = "USY.xlsx", sheetName = "USpeY", append=TRUE)
write.xlsx(USaltsY, file = "USY.xlsx", sheetName = "USaltsY", append=TRUE)
write.xlsx(USreY, file = "USY.xlsx", sheetName = "USreY", append=TRUE)
write.xlsx(USmonths, file = "USY.xlsx", sheetName = "USmonths", append=TRUE)
write.xlsx(USyrs, file = "USY.xlsx", sheetName = "USyrs", append=TRUE)
write.xlsx(US3yrsPBD, file = "USY.xlsx", sheetName = "US3yrsPBD", append=TRUE)

write.xlsx(USmonths, file = "USYm.xlsx", sheetName = "USmonths", append=TRUE)

#mAKE TABLES
EURPNC <- PNCtable(EURQ)
#potential
EURftP <- FTtable(EURQP)
EURacP <- ACbyMRtable(EURQP)
EUReq1P <- IAbyMRtable(EURQP, "Equity")
EUReq2P <- EQStyletable(EURQP)
EUReq3P <- EQCaptable(EURQP)
EURfi1P <- SACbyMRtable(EURQP, "Fixed Income")
EURfi2P <- IAbyMRtable(EURQP, "Fixed Income")
EURhfP <- SACbyMRtable(EURQP, "Hedge Funds")
EURpeP <- SACbyMRtable(EURQP, "Private Equity")
EURaltsP <- AltsTable(EURQP)
EURreP <- SACbyMRtable(EURQP, "Real Estate")
#new
EURftN <- FTtable(EURQN)
EURacN <- ACbyMRtable(EURQN)
EUReq1N <- IAbyMRtable(EURQN, "Equity")
EUReq2N <- EQStyletable(EURQN)
EUReq3N <- EQCaptable(EURQN)
EURfi1N <- SACbyMRtable(EURQN, "Fixed Income")
EURfi2N <- IAbyMRtable(EURQN, "Fixed Income")
EURhfN <- SACbyMRtable(EURQN, "Hedge Funds")
EURpeN <- SACbyMRtable(EURQN, "Private Equity")
EURaltsN <- AltsTable(EURQN)
EURreN <- SACbyMRtable(EURQN, "Real Estate")
#completed
EURftC <- FTtable(EURQC)
EURacC <- ACbyMRtable(EURQC)
EUReq1C <- IAbyMRtable(EURQC, "Equity")
EUReq2C <- EQStyletable(EURQC)
EUReq3C <- EQCaptable(EURQC)
EURfi1C <- SACbyMRtable(EURQC, "Fixed Income")
EURfi2C <- IAbyMRtable(EURQC, "Fixed Income")
EURhfC <- SACbyMRtable(EURQC, "Hedge Funds")
EURpeC <- SACbyMRtable(EURQC, "Private Equity")
EURaltsC <- AltsTable(EURQC)
EURreC <- SACbyMRtable(EURQC, "Real Estate")



write.xlsx(EURPNC, file = "EURQ.xlsx", sheetName = "EURPNC", append=TRUE)
write.xlsx(EURftP, file = "EURQ.xlsx", sheetName = "EURftP", append=TRUE)
write.xlsx(EURacP, file = "EURQ.xlsx", sheetName = "EURacP", append=TRUE)
write.xlsx(EUReq1P, file = "EURQ.xlsx", sheetName = "EUReq1P", append=TRUE)
write.xlsx(EUReq2P, file = "EURQ.xlsx", sheetName = "EUReq2P", append=TRUE)
write.xlsx(EUReq3P, file = "EURQ.xlsx", sheetName = "EUReq3P", append=TRUE)
write.xlsx(EURfi1P, file = "EURQ.xlsx", sheetName = "EURfi1P", append=TRUE)
write.xlsx(EURfi2P, file = "EURQ.xlsx", sheetName = "EURfi2P", append=TRUE)
write.xlsx(EURhfP, file = "EURQ.xlsx", sheetName = "EURhfP", append=TRUE)
write.xlsx(EURpeP, file = "EURQ.xlsx", sheetName = "EURpeP", append=TRUE)
write.xlsx(EURaltsP, file = "EURQ.xlsx", sheetName = "EURaltsP", append=TRUE)
write.xlsx(EURreP, file = "EURQ.xlsx", sheetName = "EURreP", append=TRUE)

write.xlsx(EURftN, file = "EURQ.xlsx", sheetName = "EURftN", append=TRUE)
write.xlsx(EURacN, file = "EURQ.xlsx", sheetName = "EURacN", append=TRUE)
write.xlsx(EUReq1N, file = "EURQ.xlsx", sheetName = "EUReq1N", append=TRUE)
write.xlsx(EUReq2N, file = "EURQ.xlsx", sheetName = "EUReq2N", append=TRUE)
write.xlsx(EUReq3N, file = "EURQ.xlsx", sheetName = "EUReq3N", append=TRUE)
write.xlsx(EURfi1N, file = "EURQ.xlsx", sheetName = "EURfi1N", append=TRUE)
write.xlsx(EURfi2N, file = "EURQ.xlsx", sheetName = "EURfi2N", append=TRUE)
write.xlsx(EURhfN, file = "EURQ.xlsx", sheetName = "EURhfN", append=TRUE)
write.xlsx(EURpeN, file = "EURQ.xlsx", sheetName = "EURpeN", append=TRUE)
write.xlsx(EURaltsN, file = "EURQ.xlsx", sheetName = "EURaltsN", append=TRUE)
write.xlsx(EURreN, file = "EURQ.xlsx", sheetName = "EURreN", append=TRUE)


write.xlsx(EURftC, file = "EURQ.xlsx", sheetName = "EURftC", append=TRUE)
write.xlsx(EURacC, file = "EURQ.xlsx", sheetName = "EURacC", append=TRUE)
write.xlsx(EUReq1C, file = "EURQ.xlsx", sheetName = "EUReq1C", append=TRUE)
write.xlsx(EUReq2C, file = "EURQ.xlsx", sheetName = "EUReq2C", append=TRUE)
write.xlsx(EUReq3C, file = "EURQ.xlsx", sheetName = "EUReq3C", append=TRUE)
write.xlsx(EURfi1C, file = "EURQ.xlsx", sheetName = "EURfi1C", append=TRUE)
write.xlsx(EURfi2C, file = "EURQ.xlsx", sheetName = "EURfi2C", append=TRUE)
write.xlsx(EURhfC, file = "EURQ.xlsx", sheetName = "EURhfC", append=TRUE)
write.xlsx(EURpeC, file = "EURQ.xlsx", sheetName = "EURpeC", append=TRUE)
write.xlsx(EURaltsC, file = "EURQ.xlsx", sheetName = "EURaltsC", append=TRUE)
write.xlsx(EURreC, file = "EURQ.xlsx", sheetName = "EURreC", append=TRUE)


#year
EURyrsPDB <- EURmandates[EURmandates$FundType=="Public D.B.",]
EURacY <- ACbyMRtable(EUR12mo)
EURftY <- FTtable(EUR12mo)
EUReq1Y <- IAbyMRtable(EUR12mo, "Equity")
EUReq2Y <- EQStyletable(EUR12mo)
EUReq3Y <- EQCaptable(EUR12mo)
EURfi1Y <- SACbyMRtable(EUR12mo, "Fixed Income")
EURfi2Y <- IAbyMRtable(EUR12mo, "Fixed Income")
EURhfY <- SACbyMRtable(EUR12mo, "Hedge Funds")
EURpeY <- SACbyMRtable(EUR12mo, "Private Equity")
EURaltsY <- AltsTable(EUR12mo)
EURreY <- SACbyMRtable(EUR12mo, "Real Estate")
EURmonths <- MonthTable(EUR12mo)
EURyrs <- YearTable(EUR3y)
EUR3yrsPBD <- YearTable(EURyrsPDB)

write.xlsx(EURacY, file = "EURY.xlsx", sheetName = "EURacY", append=TRUE)
write.xlsx(EURftY, file = "EURY.xlsx", sheetName = "EURftY", append=TRUE)
write.xlsx(EUReq1Y, file = "EURY.xlsx", sheetName = "EUReq1Y", append=TRUE)
write.xlsx(EUReq2Y, file = "EURY.xlsx", sheetName = "EUReq2Y", append=TRUE)
write.xlsx(EUReq3Y, file = "EURY.xlsx", sheetName = "EUReq3Y", append=TRUE)
write.xlsx(EURfi1Y, file = "EURY.xlsx", sheetName = "EURfi1Y", append=TRUE)
write.xlsx(EURfi2Y, file = "EURY.xlsx", sheetName = "EURfi2Y", append=TRUE)
write.xlsx(EURhfY, file = "EURY.xlsx", sheetName = "EURhfY", append=TRUE)
write.xlsx(EURpeY, file = "EURY.xlsx", sheetName = "EURpeY", append=TRUE)
write.xlsx(EURaltsY, file = "EURY.xlsx", sheetName = "EURaltsY", append=TRUE)
write.xlsx(EURreY, file = "EURY.xlsx", sheetName = "EURreY", append=TRUE)
write.xlsx(EURmonths, file = "EURY.xlsx", sheetName = "EURmonths", append=TRUE)
write.xlsx(EURyrs, file = "EURY.xlsx", sheetName = "EURyrs", append=TRUE)
write.xlsx(EUR3yrsPBD, file = "EURY.xlsx", sheetName = "EUR3yrsPBD", append=TRUE)


#mAKE TABLES - ASIA
ASIAPNC <- PNCtable(ASIAQ)
#potential
ASIAftP <- FTtable(ASIAQP)
ASIAacP <- ACbyMRtable(ASIAQP)

#new
ASIAftN <- FTtable(ASIAQN)
ASIAacN <- ACbyMRtable(ASIAQN)

#completed
ASIAftC <- FTtable(ASIAQC)
ASIAacC <- ACbyMRtable(ASIAQC)

#year
ASIAacY <- ACbyMRtable(ASIA12mo)
ASIAftY <- FTtable(ASIA12mo)
ASIAeq1Y <- IAbyMRtable(ASIA12mo, "Equity")
ASIAeq2Y <- EQStyletable(ASIA12mo)
ASIAeq3Y <- EQCaptable(ASIA12mo)
ASIAfi1Y <- SACbyMRtable(ASIA12mo, "Fixed Income")
ASIAfi2Y <- IAbyMRtable(ASIA12mo, "Fixed Income")
ASIAhfY <- SACbyMRtable(ASIA12mo, "Hedge Funds")
ASIApeY <- SACbyMRtable(ASIA12mo, "Private Equity")
ASIAaltsY <- AltsTable(ASIA12mo)
ASIAreY <- SACbyMRtable(ASIA12mo, "Real Estate")
ASIAmonths <- MonthTable(ASIA12mo)
ASIAyrs <- YearTable(ASIA3y)
ASIAPDB <- ASIA3y[ASIA3y$FundType=="Public D.B.",]
ASIAPByrs <- YearTable(ASIAPDB)


write.xlsx(ASIAPNC, file = "ASIAQ.xlsx", sheetName = "ASIAPNC", append=TRUE)
write.xlsx(ASIAftP, file = "ASIAQ.xlsx", sheetName = "ASIAftP", append=TRUE)
write.xlsx(ASIAacP, file = "ASIAQ.xlsx", sheetName = "ASIAacP", append=TRUE)

write.xlsx(ASIAftN, file = "ASIAQ.xlsx", sheetName = "ASIAftN", append=TRUE)
write.xlsx(ASIAacN, file = "ASIAQ.xlsx", sheetName = "ASIAacN", append=TRUE)

write.xlsx(ASIAftC, file = "ASIAQ.xlsx", sheetName = "ASIAftC", append=TRUE)
write.xlsx(ASIAacC, file = "ASIAQ.xlsx", sheetName = "ASIAacC", append=TRUE)

write.xlsx(ASIAacY, file = "ASIAQ.xlsx", sheetName = "ASIAacY", append=TRUE)
write.xlsx(ASIAftY, file = "ASIAQ.xlsx", sheetName = "ASIAftY", append=TRUE)
write.xlsx(ASIAeq1Y, file = "ASIAQ.xlsx", sheetName = "ASIAeq1Y", append=TRUE)
write.xlsx(ASIAeq2Y, file = "ASIAQ.xlsx", sheetName = "ASIAeq2Y", append=TRUE)
write.xlsx(ASIAeq3Y, file = "ASIAQ.xlsx", sheetName = "ASIAeq3Y", append=TRUE)
write.xlsx(ASIAfi1Y, file = "ASIAQ.xlsx", sheetName = "ASIAfi1Y", append=TRUE)
write.xlsx(ASIAfi2Y, file = "ASIAQ.xlsx", sheetName = "ASIAfi2Y", append=TRUE)
write.xlsx(ASIAhfY, file = "ASIAQ.xlsx", sheetName = "ASIAhfY", append=TRUE)
write.xlsx(ASIApeY, file = "ASIAQ.xlsx", sheetName = "ASIApeY", append=TRUE)
write.xlsx(ASIAaltsY, file = "ASIAQ.xlsx", sheetName = "ASIAaltsY", append=TRUE)
write.xlsx(ASIAreY, file = "ASIAQ.xlsx", sheetName = "ASIAreY", append=TRUE)
write.xlsx(ASIAmonths, file = "ASIAQ.xlsx", sheetName = "ASIAmonths", append=TRUE)
write.xlsx(ASIAyrs, file = "ASIAQ.xlsx", sheetName = "ASIAyrs", append=TRUE)
write.xlsx(ASIAPByrs, file = "ASIAQ.xlsx", sheetName = "ASIAPByrs", append=TRUE)


#mAKE TABLES - AUS
AUSPNC <- PNCtable(AUSQ)
#potential
AUSftP <- FTtable(AUSQP)
AUSacP <- ACbyMRtable(AUSQP)

#new
AUSftN <- FTtable(AUSQN)
AUSacN <- ACbyMRtable(AUSQN)

#completed
AUSftC <- FTtable(AUSQC)
AUSacC <- ACbyMRtable(AUSQC)

#year
AUSacY <- ACbyMRtable(AUS12mo)
AUSftY <- FTtable(AUS12mo)
AUSeq1Y <- IAbyMRtable(AUS12mo, "Equity")
AUSeq2Y <- EQStyletable(AUS12mo)
AUSeq3Y <- EQCaptable(AUS12mo)
AUSfi1Y <- SACbyMRtable(AUS12mo, "Fixed Income")
AUSfi2Y <- IAbyMRtable(AUS12mo, "Fixed Income")
AUShfY <- SACbyMRtable(AUS12mo, "Hedge Funds")
AUSpeY <- SACbyMRtable(AUS12mo, "Private Equity")
AUSaltsY <- AltsTable(AUS12mo)
AUSreY <- SACbyMRtable(AUS12mo, "Real Estate")
AUSmonths <- MonthTable(AUS12mo)
AUSyrs <- YearTable(AUS3y)
AUSPDB <- AUS3y[AUS3y$FundType=="Public D.B.",]
AUSPByrs <- YearTable(AUSPDB)

library(xlsx)
write.xlsx(AUSPNC, file = "AUSQ.xlsx", sheetName = "AUSPNC", append=TRUE)
write.xlsx(AUSftP, file = "AUSQ.xlsx", sheetName = "AUSftP", append=TRUE)
write.xlsx(AUSacP, file = "AUSQ.xlsx", sheetName = "AUSacP", append=TRUE)

write.xlsx(AUSftN, file = "AUSQ.xlsx", sheetName = "AUSftN", append=TRUE)
write.xlsx(AUSacN, file = "AUSQ.xlsx", sheetName = "AUSacN", append=TRUE)

write.xlsx(AUSftC, file = "AUSQ.xlsx", sheetName = "AUSftC", append=TRUE)
write.xlsx(AUSacC, file = "AUSQ.xlsx", sheetName = "AUSacC", append=TRUE)

write.xlsx(AUSacY, file = "AUSQ.xlsx", sheetName = "AUSacY", append=TRUE)
write.xlsx(AUSftY, file = "AUSQ.xlsx", sheetName = "AUSftY", append=TRUE)
write.xlsx(AUSeq1Y, file = "AUSQ.xlsx", sheetName = "AUSeq1Y", append=TRUE)
write.xlsx(AUSeq2Y, file = "AUSQ.xlsx", sheetName = "AUSeq2Y", append=TRUE)
write.xlsx(AUSeq3Y, file = "AUSQ.xlsx", sheetName = "AUSeq3Y", append=TRUE)
write.xlsx(AUSfi1Y, file = "AUSQ.xlsx", sheetName = "AUSfi1Y", append=TRUE)
write.xlsx(AUSfi2Y, file = "AUSQ.xlsx", sheetName = "AUSfi2Y", append=TRUE)
write.xlsx(AUShfY, file = "AUSQ.xlsx", sheetName = "AUShfY", append=TRUE)
write.xlsx(AUSpeY, file = "AUSQ.xlsx", sheetName = "AUSpeY", append=TRUE)
write.xlsx(AUSaltsY, file = "AUSQ.xlsx", sheetName = "AUSaltsY", append=TRUE)
write.xlsx(AUSreY, file = "AUSQ.xlsx", sheetName = "AUSreY", append=TRUE)
write.xlsx(AUSmonths, file = "AUSQ.xlsx", sheetName = "AUSmonths", append=TRUE)
write.xlsx(AUSyrs, file = "AUSQ.xlsx", sheetName = "AUSyrs", append=TRUE)
write.xlsx(AUSPByrs, file = "AUSQ.xlsx", sheetName = "AUSPByrs", append=TRUE)

##### IMPORTANT - REDO EUROPE SO UK IS SEPARATE BEFORE DOING CONSULTANT TABLES
## change "Europe" variable in mandateVariables,
## redo "create fund region column" in mandateColumnFormatting
## redo Create UK & Europe Quarterly and 12 month dataframes in mandateCreateDataframes
USQtable1 <- consTable(USQ)
USQtable2 <- consTable(USQ, 2)
USQmx <- consMX(USQ)

UKQtable1 <- consTable(UKQ)
UKQtable2 <- consTable(UKQ, 2)
UKQmx <- consMX(UKQ)

EURQtable1 <- consTable(EURQ)
EURQtable2 <- consTable(EURQ, 2)
EURQmx <- consMX(EURQ)

USYtable1 <- consTable(US12mo)
USYtable2 <- consTable(US12mo, 2)
USYmx <- consMX(US12mo)

UKYtable1 <- consTable(UK12mo)
UKYtable2 <- consTable(UK12mo, 2)
UKYmx <- consMX(UK12mo)

EURYtable1 <- consTable(EUR12mo)
EURYtable2 <- consTable(EUR12mo, 2)
EURYmx <- consMX(EUR12mo)

AUSYtable1 <- consTable(AUS12mo)
AUSYtable2 <- consTable(AUS12mo, 2)
AUSYmx <- consMX(AUS12mo)

ASIAYtable1 <- consTable(ASIA12mo)
ASIAYtable2 <- consTable(ASIA12mo, 2)
ASIAYmx <- consMX(ASIA12mo)


write.xlsx(USQtable1, file = "consultants.xlsx", sheetName = "USQtable1", append=TRUE)
write.xlsx(USQtable2, file = "consultants.xlsx", sheetName = "USQtable2", append=TRUE)
write.xlsx(USQmx, file = "consultants.xlsx", sheetName = "USQmx", append=TRUE)
write.xlsx(UKQtable1, file = "consultants.xlsx", sheetName = "UKQtable1", append=TRUE)
write.xlsx(UKQtable2, file = "consultants.xlsx", sheetName = "UKQtable2", append=TRUE)
write.xlsx(UKQmx, file = "consultants.xlsx", sheetName = "UKQmx", append=TRUE)
write.xlsx(EURQtable1, file = "consultants.xlsx", sheetName = "EURQtable1", append=TRUE)
write.xlsx(EURQtable2, file = "consultants.xlsx", sheetName = "EURQtable2", append=TRUE)
write.xlsx(EURQmx, file = "consultants.xlsx", sheetName = "EURQmx", append=TRUE)
write.xlsx(USYtable1, file = "consultants.xlsx", sheetName = "USYtable1", append=TRUE)
write.xlsx(USYtable2, file = "consultants.xlsx", sheetName = "USYtable2", append=TRUE)
write.xlsx(USYmx, file = "consultants.xlsx", sheetName = "USYmx", append=TRUE)
write.xlsx(UKYtable1, file = "consultants.xlsx", sheetName = "UKYtable1", append=TRUE)
write.xlsx(UKYtable2, file = "consultants.xlsx", sheetName = "UKYtable2", append=TRUE)
write.xlsx(UKYmx, file = "consultants.xlsx", sheetName = "UKYmx", append=TRUE)
write.xlsx(EURYtable1, file = "consultants.xlsx", sheetName = "EURYtable1", append=TRUE)
write.xlsx(EURYtable2, file = "consultants.xlsx", sheetName = "EURYtable2", append=TRUE)
write.xlsx(EURYmx, file = "consultants.xlsx", sheetName = "EURYmx", append=TRUE)
write.xlsx(AUSYtable1, file = "consultants.xlsx", sheetName = "AUSYtable1", append=TRUE)
write.xlsx(AUSYtable2, file = "consultants.xlsx", sheetName = "AUSYtable2", append=TRUE)
write.xlsx(AUSYmx, file = "consultants.xlsx", sheetName = "AUSYmx", append=TRUE)
write.xlsx(ASIAYtable1, file = "consultants.xlsx", sheetName = "ASIAYtable1", append=TRUE)
write.xlsx(ASIAYtable2, file = "consultants.xlsx", sheetName = "ASIAYtable2", append=TRUE)
write.xlsx(ASIAYmx, file = "consultants.xlsx", sheetName = "ASIAYmx", append=TRUE)
