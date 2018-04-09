library(readr) # for read_csv()
library(purrr) # for map()
library(dplyr) # for joins
library(tidyr)
library(xlsx)
getwd()
setwd(".\\Pageant\\Q4 2017")

managers <- read_csv("Q4_2017managerHires.csv") ### new way

#remove dots from column names
#remove spaces, slashes, dashes from column names
colnames(managers)
col_names <- colnames(managers)
col_names <- gsub(" ", "", col_names)
col_names <- gsub("\\/", "", col_names)
col_names <- gsub("\\-", "", col_names)
colnames(managers) <- col_names

#Make sure you only have completed searches
managers <- managers[managers$SearchStatus == "Completed",]



#US
USQ <- mgrTable(USmanagersQ)
USQa <- mgrTable(USmanagersQ, 2)
USQroundup <- roundup(USmanagersQ)
USmxQ1 <- managerMatrix(USmanagersQ)
USmxQ1colz <- colnames(USmxQ1)
USmxQ2 <- managerMatrix(USmanagersQ, 2)
USmxQ2colz <- colnames(USmxQ2)
USQalt <- mgrTableAC(USmanagersQ, "Alternative")
USQbal <- mgrTableAC(USmanagersQ, "Balanced")
USQcash <- mgrTableAC(USmanagersQ, "Cash")
USQcur <- mgrTableAC(USmanagersQ, "Currency")
USQcom <- mgrTableAC(USmanagersQ, "Commodities")
USQderi <- mgrTableAC(USmanagersQ, "Derivatives")
USQdis <- mgrTableAC(USmanagersQ, "Distressed Debt")
USQeq <- mgrTableAC(USmanagersQ, "Equity")
USQetf <- mgrTableAC(USmanagersQ, "ETF")
USQfi <- mgrTableAC(USmanagersQ, "Fixed Income")
USQhf <- mgrTableAC(USmanagersQ, "Hedge Funds")
USQma <- mgrTableAC(USmanagersQ, "Multi Asset")
USQpe <- mgrTableAC(USmanagersQ, "Private Equity")
USQra <- mgrTableAC(USmanagersQ, "Real Assets")
USQre <- mgrTableAC(USmanagersQ, "Real Estate")
USQtaa <- mgrTableAC(USmanagersQ, "Tactical Asset Allocation")


USQ %>%
  group_by(SearchStatus, MainAssetClass) %>%
  summarise(number = n(), total = sum(MandateSizeAmount))






USY <- mgrTable(USmanagersY)
USYa <- mgrTable(USmanagersY, 2)
USYroundup <- roundup(USmanagersY)
USmonths1 <- monthTable(USmanagersY)
USmonths2 <- monthTable(USmanagersY, 2)
USmxY1 <- managerMatrix(USmanagersY)
USmxY1colz <- colnames(USmxY1)
USmxY2 <- managerMatrix(USmanagersY, 2)
USmxY2colz <- colnames(USmxY2)
USYalt <- mgrTableAC(USmanagersY, "Alternative")
USYbal <- mgrTableAC(USmanagersY, "Balanced")
USYcash <- mgrTableAC(USmanagersY, "Cash")
USYcur <- mgrTableAC(USmanagersY, "Currency")
USYcom <- mgrTableAC(USmanagersY, "Commodities")
USYderi <- mgrTableAC(USmanagersY, "Derivatives")
USYdis <- mgrTableAC(USmanagersY, "Distressed Debt")
USYeq <- mgrTableAC(USmanagersY, "Equity")
USYetf <- mgrTableAC(USmanagersY, "ETF")
USYfi <- mgrTableAC(USmanagersY, "Fixed Income")
USYhf <- mgrTableAC(USmanagersY, "Hedge Funds")
USYma <- mgrTableAC(USmanagersY, "Multi Asset")
USYpe <- mgrTableAC(USmanagersY, "Private Equity")
USYra <- mgrTableAC(USmanagersY, "Real Assets")
USYre <- mgrTableAC(USmanagersY, "Real Estate")
USYtaa <- mgrTableAC(USmanagersY, "Tactical Asset Allocation")
USYroundup2 <- roundup2(USmanagersY)

write.xlsx(USQ, file = "USQmgrs.xlsx", sheetName = "USQ", append=TRUE)
write.xlsx(USQa, file = "USQmgrs.xlsx", sheetName = "USQa", append=TRUE)
write.xlsx(USQroundup, file = "USQmgrs.xlsx", sheetName = "USQroundup", append=TRUE)
write.xlsx(USmxQ1, file = "USQmgrs.xlsx", sheetName = "USmxQ1", append=TRUE)
write.xlsx(USmxQ1colz, file = "USQmgrs.xlsx", sheetName = "USmxQ1colz", append=TRUE)
write.xlsx(USmxQ2, file = "USQmgrs.xlsx", sheetName = "USmxQ2", append=TRUE)
write.xlsx(USmxQ2colz, file = "USQmgrs.xlsx", sheetName = "USmxQ2colz", append=TRUE)
write.xlsx(USQalt, file = "USQmgrs.xlsx", sheetName = "USQalt", append=TRUE)
write.xlsx(USQbal, file = "USQmgrs.xlsx", sheetName = "USQbal", append=TRUE)
write.xlsx(USQcash, file = "USQmgrs.xlsx", sheetName = "USQcash", append=TRUE)
write.xlsx(USQcur, file = "USQmgrs.xlsx", sheetName = "USQcur", append=TRUE)
write.xlsx(USQcom, file = "USQmgrs.xlsx", sheetName = "USQcom", append=TRUE)
write.xlsx(USQderi, file = "USQmgrs.xlsx", sheetName = "USQderi", append=TRUE)
write.xlsx(USQdis, file = "USQmgrs.xlsx", sheetName = "USQdis", append=TRUE)
write.xlsx(USQeq, file = "USQmgrs.xlsx", sheetName = "USQeq", append=TRUE)
write.xlsx(USQetf, file = "USQmgrs.xlsx", sheetName = "USQetf", append=TRUE)
write.xlsx(USQfi, file = "USQmgrs.xlsx", sheetName = "USQfi", append=TRUE)
write.xlsx(USQhf, file = "USQmgrs.xlsx", sheetName = "USQhf", append=TRUE)
write.xlsx(USQma, file = "USQmgrs.xlsx", sheetName = "USQma", append=TRUE)
write.xlsx(USQpe, file = "USQmgrs.xlsx", sheetName = "USQpe", append=TRUE)
write.xlsx(USQra, file = "USQmgrs.xlsx", sheetName = "USQra", append=TRUE)
write.xlsx(USQre, file = "USQmgrs.xlsx", sheetName = "USQre", append=TRUE)
write.xlsx(USQtaa, file = "USQmgrs.xlsx", sheetName = "USQtaa", append=TRUE)







write.xlsx(USY, file = "USYmgrsa.xlsx", sheetName = "USY", append=TRUE)
write.xlsx(USYa, file = "USYmgrsa.xlsx", sheetName = "USYa", append=TRUE)
write.xlsx(USYroundup, file = "USYmgrsa.xlsx", sheetName = "USYroundup", append=TRUE)
write.xlsx(USmonths1, file = "USYmgrsa.xlsx", sheetName = "USmonths1", append=TRUE)
write.xlsx(USmonths2, file = "USYmgrsa.xlsx", sheetName = "USmonths2", append=TRUE)
write.xlsx(USmxY1, file = "USYmgrsa.xlsx", sheetName = "USmxY1", append=TRUE)
write.xlsx(USmxY1colz, file = "USYmgrsa.xlsx", sheetName = "USmxY1colz", append=TRUE)
write.xlsx(USmxY2, file = "USYmgrsa.xlsx", sheetName = "USmxY2", append=TRUE)
write.xlsx(USmxY2colz, file = "USYmgrsa.xlsx", sheetName = "USmxY2colz", append=TRUE)
write.xlsx(USYalt, file = "USYmgrsa.xlsx", sheetName = "USYalt", append=TRUE)

write.xlsx(USYbal, file = "USYmgrs1.xlsx", sheetName = "USYbal", append=TRUE)
write.xlsx(USYcash, file = "USYmgrs1.xlsx", sheetName = "USYcash", append=TRUE)
write.xlsx(USYcur, file = "USYmgrs1.xlsx", sheetName = "USYcur", append=TRUE)
write.xlsx(USYcom, file = "USYmgrs1.xlsx", sheetName = "USYcom", append=TRUE)
write.xlsx(USYderi, file = "USYmgrs1.xlsx", sheetName = "USYderi", append=TRUE)
write.xlsx(USYdis, file = "USYmgrs1.xlsx", sheetName = "USYdis", append=TRUE)
write.xlsx(USYeq, file = "USYmgrs1.xlsx", sheetName = "USYeq", append=TRUE)
write.xlsx(USYetf, file = "USYmgrs1.xlsx", sheetName = "USYetf", append=TRUE)
write.xlsx(USYfi, file = "USYmgrs1.xlsx", sheetName = "USYfi", append=TRUE)
write.xlsx(USYhf, file = "USYmgrs1.xlsx", sheetName = "USYhf", append=TRUE)
write.xlsx(USYma, file = "USYmgrs1.xlsx", sheetName = "USYma", append=TRUE)
write.xlsx(USYpe, file = "USYmgrs1.xlsx", sheetName = "USYpe", append=TRUE)
write.xlsx(USYra, file = "USYmgrs1.xlsx", sheetName = "USYra", append=TRUE)
write.xlsx(USYre, file = "USYmgrs1.xlsx", sheetName = "USYre", append=TRUE)
write.xlsx(USYtaa, file = "USYmgrs1.xlsx", sheetName = "USYtaa", append=TRUE)

write.xlsx(USYroundup2, file = "USYmgrs2.xlsx", sheetName = "USYroundup2", append=TRUE)

#------------EUROPE
EURQ <- mgrTable(EURmanagersQ)
EURQa <- mgrTable(EURmanagersQ, 2)
EURQroundup <- roundup(EURmanagersQ)
EURmxQ1 <- managerMatrix(EURmanagersQ)
EURmxQcolz <- colnames(EURmxQ1)
EURmxQ2 <- managerMatrix(EURmanagersQ, 2)
EURQalt <- mgrTableAC(EURmanagersQ, "Alternative")
EURQbal <- mgrTableAC(EURmanagersQ, "Balanced")
EURQcash <- mgrTableAC(EURmanagersQ, "Cash")
EURQcur <- mgrTableAC(EURmanagersQ, "Currency")
EURQcom <- mgrTableAC(EURmanagersQ, "Commodities")
EURQderi <- mgrTableAC(EURmanagersQ, "Derivatives")
EURQdis <- mgrTableAC(EURmanagersQ, "Distressed Debt")
EURQeq <- mgrTableAC(EURmanagersQ, "Equity")
EURQetf <- mgrTableAC(EURmanagersQ, "ETF")
EURQfi <- mgrTableAC(EURmanagersQ, "Fixed Income")
EURQhf <- mgrTableAC(EURmanagersQ, "Hedge Funds")
EURQma <- mgrTableAC(EURmanagersQ, "Multi Asset")
EURQpe <- mgrTableAC(EURmanagersQ, "Private Equity")
EURQra <- mgrTableAC(EURmanagersQ, "Real Assets")
EURQre <- mgrTableAC(EURmanagersQ, "Real Estate")
EURQtaa <- mgrTableAC(EURmanagersQ, "Tactical Asset Allocation")
EURY <- mgrTable(EURmanagersY)
EURYa <- mgrTable(EURmanagersY, 2)
EURYroundup <- roundup(EURmanagersY)
EURYroundup2 <- roundup2(EURmanagersY)
EURmonths1 <- monthTable(EURmanagersY)
EURmonths2 <- monthTable(EURmanagersY, 2)
EURmxY1 <- managerMatrix(EURmanagersY)
EURmxYcolz <- colnames(EURmxY1)
EURmxY2 <- managerMatrix(EURmanagersY, 2)
EURYalt <- mgrTableAC(EURmanagersY, "Alternative")
EURYbal <- mgrTableAC(EURmanagersY, "Balanced")
EURYcash <- mgrTableAC(EURmanagersY, "Cash")
EURYcur <- mgrTableAC(EURmanagersY, "Currency")
EURYcom <- mgrTableAC(EURmanagersY, "Commodities")
EURYderi <- mgrTableAC(EURmanagersY, "Derivatives")
EURYdis <- mgrTableAC(EURmanagersY, "Distressed Debt")
EURYeq <- mgrTableAC(EURmanagersY, "Equity")
EURYetf <- mgrTableAC(EURmanagersY, "ETF")
EURYfi <- mgrTableAC(EURmanagersY, "Fixed Income")
EURYhf <- mgrTableAC(EURmanagersY, "Hedge Funds")
EURYma <- mgrTableAC(EURmanagersY, "Multi Asset")
EURYpe <- mgrTableAC(EURmanagersY, "Private Equity")
EURYra <- mgrTableAC(EURmanagersY, "Real Assets")
EURYre <- mgrTableAC(EURmanagersY, "Real Estate")
EURYtaa <- mgrTableAC(EURmanagersY, "Tactical Asset Allocation")

write.xlsx(EURQ, file = "EURQmgrs.xlsx", sheetName = "EURQ", append=TRUE)
write.xlsx(EURQa, file = "EURQmgrs.xlsx", sheetName = "EURQa", append=TRUE)
write.xlsx(EURQroundup, file = "EURQmgrs.xlsx", sheetName = "EURQroundup", append=TRUE)
write.xlsx(EURmxQ1, file = "EURQmgrs.xlsx", sheetName = "EURmxQ1", append=TRUE)
write.xlsx(EURmxQcolz, file = "EURQmgrs.xlsx", sheetName = "EURmxQcolz", append=TRUE)
write.xlsx(EURmxQ2, file = "EURQmgrs.xlsx", sheetName = "EURmxQ2", append=TRUE)
write.xlsx(EURQalt, file = "EURQmgrs.xlsx", sheetName = "EURQalt", append=TRUE)
write.xlsx(EURQbal, file = "EURQmgrs.xlsx", sheetName = "EURQbal", append=TRUE)
write.xlsx(EURQcash, file = "EURQmgrs.xlsx", sheetName = "EURQcash", append=TRUE)
write.xlsx(EURQcur, file = "EURQmgrs.xlsx", sheetName = "EURQcur", append=TRUE)
write.xlsx(EURQcom, file = "EURQmgrs.xlsx", sheetName = "EURQcom", append=TRUE)
write.xlsx(EURQderi, file = "EURQmgrs.xlsx", sheetName = "EURQderi", append=TRUE)
write.xlsx(EURQdis, file = "EURQmgrs.xlsx", sheetName = "EURQdis", append=TRUE)
write.xlsx(EURQeq, file = "EURQmgrs.xlsx", sheetName = "EURQeq", append=TRUE)
write.xlsx(EURQetf, file = "EURQmgrs.xlsx", sheetName = "EURQetf", append=TRUE)
write.xlsx(EURQfi, file = "EURQmgrs.xlsx", sheetName = "EURQfi", append=TRUE)
write.xlsx(EURQhf, file = "EURQmgrs.xlsx", sheetName = "EURQhf", append=TRUE)
write.xlsx(EURQma, file = "EURQmgrs.xlsx", sheetName = "EURQma", append=TRUE)
write.xlsx(EURQpe, file = "EURQmgrs.xlsx", sheetName = "EURQpe", append=TRUE)
write.xlsx(EURQra, file = "EURQmgrs.xlsx", sheetName = "EURQra", append=TRUE)
write.xlsx(EURQre, file = "EURQmgrs.xlsx", sheetName = "EURQre", append=TRUE)
write.xlsx(EURQtaa, file = "EURQmgrs.xlsx", sheetName = "EURQtaa", append=TRUE)

write.xlsx(EURY, file = "EURYmgrs.xlsx", sheetName = "EURY", append=TRUE)
write.xlsx(EURYa, file = "EURYmgrs.xlsx", sheetName = "EURYa", append=TRUE)
write.xlsx(EURYroundup, file = "EURYmgrs.xlsx", sheetName = "EURYroundup", append=TRUE)
write.xlsx(EURYroundup2, file = "EURYmgrs.xlsx", sheetName = "EURYroundup2", append=TRUE)
write.xlsx(EURmonths1, file = "EURYmgrs.xlsx", sheetName = "EURmonths1", append=TRUE)
write.xlsx(EURmonths2, file = "EURYmgrs.xlsx", sheetName = "EURmonths2", append=TRUE)
write.xlsx(EURmxY1, file = "EURYmgrs.xlsx", sheetName = "EURmxY1", append=TRUE)
write.xlsx(EURmxYcolz, file = "EURYmgrs.xlsx", sheetName = "EURmxYcolz", append=TRUE)
write.xlsx(EURmxY2, file = "EURYmgrs.xlsx", sheetName = "EURmxY2", append=TRUE)

write.xlsx(EURYalt, file = "EURmgrsY1.xlsx", sheetName = "EURYalt", append=TRUE)
write.xlsx(EURYbal, file = "EURmgrsY1.xlsx", sheetName = "EURYbal", append=TRUE)
write.xlsx(EURYcash, file = "EURmgrsY1.xlsx", sheetName = "EURYcash", append=TRUE)
write.xlsx(EURYcur, file = "EURmgrsY1.xlsx", sheetName = "EURYcur", append=TRUE)
write.xlsx(EURYcom, file = "EURmgrsY1.xlsx", sheetName = "EURYcom", append=TRUE)
write.xlsx(EURYderi, file = "EURmgrsY1.xlsx", sheetName = "EURYderi", append=TRUE)
write.xlsx(EURYdis, file = "EURmgrsY1.xlsx", sheetName = "EURYdis", append=TRUE)
write.xlsx(EURYeq, file = "EURmgrsY1.xlsx", sheetName = "EURYeq", append=TRUE)
write.xlsx(EURYetf, file = "EURmgrsY1.xlsx", sheetName = "EURYetf", append=TRUE)
write.xlsx(EURYfi, file = "EURmgrsY1.xlsx", sheetName = "EURYfi", append=TRUE)
write.xlsx(EURYhf, file = "EURmgrsY1.xlsx", sheetName = "EURYhf", append=TRUE)
write.xlsx(EURYma, file = "EURmgrsY1.xlsx", sheetName = "EURYma", append=TRUE)
write.xlsx(EURYpe, file = "EURmgrsY1.xlsx", sheetName = "EURYpe", append=TRUE)
write.xlsx(EURYra, file = "EURmgrsY1.xlsx", sheetName = "EURYra", append=TRUE)
write.xlsx(EURYre, file = "EURmgrsY1.xlsx", sheetName = "EURYre", append=TRUE)
write.xlsx(EURYtaa, file = "EURmgrsY1.xlsx", sheetName = "EURYtaa", append=TRUE)



#----------ASIA
ASIAQ <- mgrTable(ASIAmanagersQ)
ASIAQa <- mgrTable(ASIAmanagersQ, 2)
ASIAQroundup <- roundup(ASIAmanagersQ)
ASIAmxQ1 <- managerMatrix(ASIAmanagersQ)
ASIAmxQcolz <- colnames(ASIAmxQ1)
ASIAmxQ2 <- managerMatrix(ASIAmanagersQ, 2)
ASIAQalt <- mgrTableAC(ASIAmanagersQ, "Alternative")
ASIAQbal <- mgrTableAC(ASIAmanagersQ, "Balanced")
ASIAQcash <- mgrTableAC(ASIAmanagersQ, "Cash")
ASIAQcur <- mgrTableAC(ASIAmanagersQ, "Currency")
ASIAQcom <- mgrTableAC(ASIAmanagersQ, "Commodities")
ASIAQderi <- mgrTableAC(ASIAmanagersQ, "Derivatives")
ASIAQdis <- mgrTableAC(ASIAmanagersQ, "Distressed Debt")
ASIAQeq <- mgrTableAC(ASIAmanagersQ, "Equity")
ASIAQetf <- mgrTableAC(ASIAmanagersQ, "ETF")
ASIAQfi <- mgrTableAC(ASIAmanagersQ, "Fixed Income")
ASIAQhf <- mgrTableAC(ASIAmanagersQ, "Hedge Funds")
ASIAQma <- mgrTableAC(ASIAmanagersQ, "Multi Asset")
ASIAQpe <- mgrTableAC(ASIAmanagersQ, "Private Equity")
ASIAQra <- mgrTableAC(ASIAmanagersQ, "Real Assets")
ASIAQre <- mgrTableAC(ASIAmanagersQ, "Real Estate")
ASIAQtaa <- mgrTableAC(ASIAmanagersQ, "Tactical Asset Allocation")
ASIAY <- mgrTable(ASIAmanagersY)
ASIAYa <- mgrTable(ASIAmanagersY, 2)
ASIAYroundup <- roundup2(ASIAmanagersY)
ASIAmonths1 <- monthTable(ASIAmanagersY)
ASIAmonths2 <- monthTable(ASIAmanagersY, 2)
ASIAmxY1 <- managerMatrix(ASIAmanagersY)
ASIAmxYcolz <- colnames(ASIAmxY1)
ASIAmxY2 <- managerMatrix(ASIAmanagersY, 2)
ASIAYalt <- mgrTableAC(ASIAmanagersY, "Alternative")
ASIAYbal <- mgrTableAC(ASIAmanagersY, "Balanced")
ASIAYcash <- mgrTableAC(ASIAmanagersY, "Cash")
ASIAYcur <- mgrTableAC(ASIAmanagersY, "Currency")
ASIAYcom <- mgrTableAC(ASIAmanagersY, "Commodities")
ASIAYderi <- mgrTableAC(ASIAmanagersY, "Derivatives")
ASIAYdis <- mgrTableAC(ASIAmanagersY, "Distressed Debt")
ASIAYeq <- mgrTableAC(ASIAmanagersY, "Equity")
ASIAYetf <- mgrTableAC(ASIAmanagersY, "ETF")
ASIAYfi <- mgrTableAC(ASIAmanagersY, "Fixed Income")
ASIAYhf <- mgrTableAC(ASIAmanagersY, "Hedge Funds")
ASIAYma <- mgrTableAC(ASIAmanagersY, "Multi Asset")
ASIAYpe <- mgrTableAC(ASIAmanagersY, "Private Equity")
ASIAYra <- mgrTableAC(ASIAmanagersY, "Real Assets")
ASIAYre <- mgrTableAC(ASIAmanagersY, "Real Estate")
ASIAYtaa <- mgrTableAC(ASIAmanagersY, "Tactical Asset Allocation")


write.xlsx(ASIAY, file = "ASIAmgrs.xlsx", sheetName = "ASIAY", append=TRUE)
write.xlsx(ASIAYa, file = "ASIAmgrs.xlsx", sheetName = "ASIAYa", append=TRUE)
write.xlsx(ASIAYroundup, file = "ASIAmgrs.xlsx", sheetName = "ASIAYroundup", append=TRUE)
write.xlsx(ASIAmonths1, file = "ASIAmgrs.xlsx", sheetName = "ASIAmonths1", append=TRUE)
write.xlsx(ASIAmxQcolz, file = "ASIAmgrs.xlsx", sheetName = "ASIAmxQcolz", append=TRUE)
write.xlsx(ASIAmonths2, file = "ASIAmgrs.xlsx", sheetName = "ASIAmonths2", append=TRUE)
write.xlsx(ASIAmxY1, file = "ASIAmgrs.xlsx", sheetName = "ASIAmxY1", append=TRUE)
write.xlsx(ASIAmxYcolz, file = "ASIAmgrs.xlsx", sheetName = "ASIAmxYcolz", append=TRUE)
write.xlsx(ASIAmxY2, file = "ASIAmgrs.xlsx", sheetName = "ASIAmxY2", append=TRUE)
write.xlsx(ASIAYalt, file = "ASIAmgrs.xlsx", sheetName = "ASIAYalt", append=TRUE)
write.xlsx(ASIAYbal, file = "ASIAmgrs.xlsx", sheetName = "ASIAYbal", append=TRUE)
write.xlsx(ASIAYcash, file = "ASIAmgrs.xlsx", sheetName = "ASIAYcash", append=TRUE)
write.xlsx(ASIAYcur, file = "ASIAmgrs.xlsx", sheetName = "ASIAYcur", append=TRUE)
write.xlsx(ASIAYcom, file = "ASIAmgrs.xlsx", sheetName = "ASIAYcom", append=TRUE)
write.xlsx(ASIAYderi, file = "ASIAmgrs.xlsx", sheetName = "ASIAYderi", append=TRUE)
write.xlsx(ASIAYdis, file = "ASIAmgrs.xlsx", sheetName = "ASIAYdis", append=TRUE)
write.xlsx(ASIAYeq, file = "ASIAmgrs.xlsx", sheetName = "ASIAYeq", append=TRUE)
write.xlsx(ASIAYetf, file = "ASIAmgrs.xlsx", sheetName = "ASIAYetf", append=TRUE)
write.xlsx(ASIAYfi, file = "ASIAmgrs.xlsx", sheetName = "ASIAYfi", append=TRUE)
write.xlsx(ASIAYhf, file = "ASIAmgrs.xlsx", sheetName = "ASIAYhf", append=TRUE)
write.xlsx(ASIAYma, file = "ASIAmgrs.xlsx", sheetName = "ASIAYma", append=TRUE)
write.xlsx(ASIAYpe, file = "ASIAmgrs.xlsx", sheetName = "ASIAYpe", append=TRUE)
write.xlsx(ASIAYra, file = "ASIAmgrs.xlsx", sheetName = "ASIAYra", append=TRUE)
write.xlsx(ASIAYre, file = "ASIAmgrs.xlsx", sheetName = "ASIAYre", append=TRUE)
write.xlsx(ASIAYtaa, file = "ASIAmgrs.xlsx", sheetName = "ASIAYtaa", append=TRUE)



#---------OCEANIA
AUSQ <- mgrTable(AUSmanagersQ)
AUSQa <- mgrTable(AUSmanagersQ, 2)
AUSQroundup <- roundup(AUSmanagersQ)
AUSmxQ1 <- managerMatrix(AUSmanagersQ)
AUSmxQcolz <- colnames(AUSmxQ1)
AUSmxQ2 <- managerMatrix(AUSmanagersQ, 2)
AUSQalt <- mgrTableAC(AUSmanagersQ, "Alternative")
AUSQbal <- mgrTableAC(AUSmanagersQ, "Balanced")
AUSQcash <- mgrTableAC(AUSmanagersQ, "Cash")
AUSQcur <- mgrTableAC(AUSmanagersQ, "Currency")
AUSQcom <- mgrTableAC(AUSmanagersQ, "Commodities")
AUSQderi <- mgrTableAC(AUSmanagersQ, "Derivatives")
AUSQdis <- mgrTableAC(AUSmanagersQ, "Distressed Debt")
AUSQeq <- mgrTableAC(AUSmanagersQ, "Equity")
AUSQetf <- mgrTableAC(AUSmanagersQ, "ETF")
AUSQfi <- mgrTableAC(AUSmanagersQ, "Fixed Income")
AUSQhf <- mgrTableAC(AUSmanagersQ, "Hedge Funds")
AUSQma <- mgrTableAC(AUSmanagersQ, "Multi Asset")
AUSQpe <- mgrTableAC(AUSmanagersQ, "Private Equity")
AUSQra <- mgrTableAC(AUSmanagersQ, "Real Assets")
AUSQre <- mgrTableAC(AUSmanagersQ, "Real Estate")
AUSQtaa <- mgrTableAC(AUSmanagersQ, "Tactical Asset Allocation")
AUSY <- mgrTable(AUSmanagersY)
AUSYa <- mgrTable(AUSmanagersY, 2)
AUSYroundup <- roundup2(AUSmanagersY)
AUSmonths1 <- monthTable(AUSmanagersY)
AUSmonths2 <- monthTable(AUSmanagersY, 2)
AUSmxY1 <- managerMatrix(AUSmanagersY)
AUSmxYcolz <- colnames(AUSmxY1)
AUSmxY2 <- managerMatrix(AUSmanagersY, 2)
AUSYalt <- mgrTableAC(AUSmanagersY, "Alternative")
AUSYbal <- mgrTableAC(AUSmanagersY, "Balanced")
AUSYcash <- mgrTableAC(AUSmanagersY, "Cash")
AUSYcur <- mgrTableAC(AUSmanagersY, "Currency")
AUSYcom <- mgrTableAC(AUSmanagersY, "Commodities")
AUSYderi <- mgrTableAC(AUSmanagersY, "Derivatives")
AUSYdis <- mgrTableAC(AUSmanagersY, "Distressed Debt")
AUSYeq <- mgrTableAC(AUSmanagersY, "Equity")
AUSYetf <- mgrTableAC(AUSmanagersY, "ETF")
AUSYfi <- mgrTableAC(AUSmanagersY, "Fixed Income")
AUSYhf <- mgrTableAC(AUSmanagersY, "Hedge Funds")
AUSYma <- mgrTableAC(AUSmanagersY, "Multi Asset")
AUSYpe <- mgrTableAC(AUSmanagersY, "Private Equity")
AUSYra <- mgrTableAC(AUSmanagersY, "Real Assets")
AUSYre <- mgrTableAC(AUSmanagersY, "Real Estate")
AUSYtaa <- mgrTableAC(AUSmanagersY, "Tactical Asset Allocation")

write.xlsx(AUSQ, file = "AUSmgrs.xlsx", sheetName = "AUSQ", append=TRUE)
write.xlsx(AUSQa, file = "AUSmgrs.xlsx", sheetName = "AUSQa", append=TRUE)
write.xlsx(AUSQroundup, file = "AUSmgrs.xlsx", sheetName = "AUSQroundup", append=TRUE)
write.xlsx(AUSmxQ1, file = "AUSmgrs.xlsx", sheetName = "AUSmxQ1", append=TRUE)
write.xlsx(AUSmxQcolz, file = "AUSmgrs.xlsx", sheetName = "AUSmxQcolz", append=TRUE)
write.xlsx(AUSmxQ2, file = "AUSmgrs.xlsx", sheetName = "AUSmxQ2", append=TRUE)
write.xlsx(AUSQalt, file = "AUSmgrs.xlsx", sheetName = "AUSQalt", append=TRUE)
write.xlsx(AUSQbal, file = "AUSmgrs.xlsx", sheetName = "AUSQbal", append=TRUE)
write.xlsx(AUSQcash, file = "AUSmgrs.xlsx", sheetName = "AUSQcash", append=TRUE)
write.xlsx(AUSQcur, file = "AUSmgrs.xlsx", sheetName = "AUSQcur", append=TRUE)
write.xlsx(AUSQcom, file = "AUSmgrs.xlsx", sheetName = "AUSQcom", append=TRUE)
write.xlsx(AUSQderi, file = "AUSmgrs.xlsx", sheetName = "AUSQderi", append=TRUE)
write.xlsx(AUSQdis, file = "AUSmgrs.xlsx", sheetName = "AUSQdis", append=TRUE)
write.xlsx(AUSQeq, file = "AUSmgrs.xlsx", sheetName = "AUSQeq", append=TRUE)
write.xlsx(AUSQetf, file = "AUSmgrs.xlsx", sheetName = "AUSQetf", append=TRUE)
write.xlsx(AUSQfi, file = "AUSmgrs.xlsx", sheetName = "AUSQfi", append=TRUE)
write.xlsx(AUSQhf, file = "AUSmgrs.xlsx", sheetName = "AUSQhf", append=TRUE)
write.xlsx(AUSQma, file = "AUSmgrs.xlsx", sheetName = "AUSQma", append=TRUE)
write.xlsx(AUSQpe, file = "AUSmgrs.xlsx", sheetName = "AUSQpe", append=TRUE)
write.xlsx(AUSQra, file = "AUSmgrs.xlsx", sheetName = "AUSQra", append=TRUE)
write.xlsx(AUSQre, file = "AUSmgrs.xlsx", sheetName = "AUSQre", append=TRUE)
write.xlsx(AUSQtaa, file = "AUSmgrs.xlsx", sheetName = "AUSQtaa", append=TRUE)
write.xlsx(AUSY, file = "AUSmgrs.xlsx", sheetName = "AUSY", append=TRUE)
write.xlsx(AUSYa, file = "AUSmgrs.xlsx", sheetName = "AUSYa", append=TRUE)
write.xlsx(AUSYroundup, file = "AUSmgrs.xlsx", sheetName = "AUSYroundup", append=TRUE)
write.xlsx(AUSmonths1, file = "AUSmgrs.xlsx", sheetName = "AUSmonths1", append=TRUE)
write.xlsx(AUSmonths2, file = "AUSmgrs.xlsx", sheetName = "AUSmonths2", append=TRUE)
write.xlsx(AUSmxY1, file = "AUSmgrs.xlsx", sheetName = "AUSmxY1", append=TRUE)
write.xlsx(AUSmxYcolz, file = "AUSmgrs.xlsx", sheetName = "AUSmxYcolz", append=TRUE)
write.xlsx(AUSmxY2, file = "AUSmgrs.xlsx", sheetName = "AUSmxY2", append=TRUE)
write.xlsx(AUSYalt, file = "AUSmgrs.xlsx", sheetName = "AUSYalt", append=TRUE)
write.xlsx(AUSYbal, file = "AUSmgrs.xlsx", sheetName = "AUSYbal", append=TRUE)
write.xlsx(AUSYcash, file = "AUSmgrs.xlsx", sheetName = "AUSYcash", append=TRUE)
write.xlsx(AUSYcur, file = "AUSmgrs.xlsx", sheetName = "AUSYcur", append=TRUE)
write.xlsx(AUSYcom, file = "AUSmgrs.xlsx", sheetName = "AUSYcom", append=TRUE)
write.xlsx(AUSYderi, file = "AUSmgrs.xlsx", sheetName = "AUSYderi", append=TRUE)
write.xlsx(AUSYdis, file = "AUSmgrs.xlsx", sheetName = "AUSYdis", append=TRUE)
write.xlsx(AUSYeq, file = "AUSmgrs.xlsx", sheetName = "AUSYeq", append=TRUE)
write.xlsx(AUSYetf, file = "AUSmgrs.xlsx", sheetName = "AUSYetf", append=TRUE)
write.xlsx(AUSYfi, file = "AUSmgrs.xlsx", sheetName = "AUSYfi", append=TRUE)
write.xlsx(AUSYhf, file = "AUSmgrs.xlsx", sheetName = "AUSYhf", append=TRUE)
write.xlsx(AUSYma, file = "AUSmgrs.xlsx", sheetName = "AUSYma", append=TRUE)
write.xlsx(AUSYpe, file = "AUSmgrs.xlsx", sheetName = "AUSYpe", append=TRUE)
write.xlsx(AUSYra, file = "AUSmgrs.xlsx", sheetName = "AUSYra", append=TRUE)
write.xlsx(AUSYre, file = "AUSmgrs.xlsx", sheetName = "AUSYre", append=TRUE)
write.xlsx(AUSYtaa, file = "AUSmgrs.xlsx", sheetName = "AUSYtaa", append=TRUE)


#ALL
ALLQ <- mgrTable(ALLmanagersQ)

#US
USQ <- mgrTable(USmanagersQ)
USQa <- mgrTable(USmanagersQ, 2)
USY <- mgrTable(USmanagersY)
USYa <- mgrTable(USmanagersY, 2)


#spotlights for Europe report
UKmxY1 <- managerMatrix(UKmanagersY)
UKmxYcolz <- colnames(UKmxY1)
UKmxY2 <- managerMatrix(UKmanagersY, 2)
NordicmxY1 <- managerMatrix(NordicmanagersY)
NordicmxYcolz <- colnames(NordicmxY1)
NordicmxY2 <- managerMatrix(NordicmanagersY, 2)

write.xlsx(UKmxY1, file = "EURspotlight.xlsx", sheetName = "UKmxY1", append=TRUE)
write.xlsx(UKmxYcolz, file = "EURspotlight.xlsx", sheetName = "UKmxYcolz", append=TRUE)
write.xlsx(UKmxY2, file = "EURspotlight.xlsx", sheetName = "UKmxY2", append=TRUE)
write.xlsx(NordicmxY1, file = "EURspotlight.xlsx", sheetName = "NordicmxY1", append=TRUE)
write.xlsx(NordicmxYcolz, file = "EURspotlight.xlsx", sheetName = "NordicmxYcolz", append=TRUE)
write.xlsx(NordicmxY2, file = "EURspotlight.xlsx", sheetName = "NordicmxY2", append=TRUE)


#Equity manager reports

USQeqmgrTABLE1 <- mgrTable(USeqManagersQ)
USQeqmgrTABLE2 <- mgrTable(USeqManagersQ, 2)

USYeqmgrTABLE1 <- mgrTable(USeqManagersY)
USYeqmgrTABLE2 <- mgrTable(USeqManagersY, 2)


USYeqRoundupCS <- roundup2CS(USeqManagersY)
USYeqRoundupSS <- roundup2SS(USeqManagersY)
USYeqRoundupIR <- roundup2IR(USeqManagersY)

USQeqmx1 <- managerMatrixEQ(USeqManagersQ)
USQeqmx1colz <- colnames(USQeqmx1)
USQeqmx2 <- managerMatrixEQ(USeqManagersQ,2)

USYeqMOS1 <- monthTable(USeqManagersY)
USYeqMOS2 <- monthTable(USeqManagersY,2)

USYeqmx1 <- managerMatrixEQ(USeqManagersY)
USYeqmx1colz <- colnames(USYeqmx1)
USYeqmx2 <- managerMatrixEQ(USeqManagersY,2)

write.xlsx(USQeqmgrTABLE1, file = "USeq.xlsx", sheetName = "USQeqmgrTABLE1", append=TRUE)
write.xlsx(USQeqmgrTABLE2, file = "USeq.xlsx", sheetName = "USQeqmgrTABLE2", append=TRUE)

write.xlsx(USYeqmgrTABLE1, file = "USeq.xlsx", sheetName = "USYeqmgrTABLE1", append=TRUE)
write.xlsx(USYeqmgrTABLE2, file = "USeq.xlsx", sheetName = "USYeqmgrTABLE2", append=TRUE)

write.xlsx(USYeqRoundupCS, file = "USeq.xlsx", sheetName = "USYeqRoundupCS", append=TRUE)
write.xlsx(USYeqRoundupSS, file = "USeq.xlsx", sheetName = "USYeqRoundupSS", append=TRUE)
write.xlsx(USYeqRoundupIR, file = "USeq.xlsx", sheetName = "USYeqRoundupIR", append=TRUE)

write.xlsx(USQeqmx1, file = "USeq.xlsx", sheetName = "USQeqmx1", append=TRUE)
write.xlsx(USQeqmx1colz, file = "USeq.xlsx", sheetName = "USQeqmx1colz", append=TRUE)
write.xlsx(USQeqmx2, file = "USeq.xlsx", sheetName = "USQeqmx2", append=TRUE)

write.xlsx(USYeqMOS1, file = "USeq.xlsx", sheetName = "USYeqMOS1", append=TRUE)
write.xlsx(USYeqMOS2, file = "USeq.xlsx", sheetName = "USYeqMOS2", append=TRUE)

write.xlsx(USYeqmx1, file = "USeq.xlsx", sheetName = "USYeqmx1", append=TRUE)
write.xlsx(USYeqmx1colz, file = "USeq.xlsx", sheetName = "USYeqmx1colz", append=TRUE)
write.xlsx(USYeqmx2, file = "USeq.xlsx", sheetName = "USYeqmx2", append=TRUE)

EURQeqmgrTABLE1 <- mgrTable(EUReqManagersQ)
EURQeqmgrTABLE2 <- mgrTable(EUReqManagersQ, 2)

EURYeqmgrTABLE1 <- mgrTable(EUReqManagersY)
EURYeqmgrTABLE2 <- mgrTable(EUReqManagersY, 2)


EURYeqRoundupCS <- roundup2CS(EUReqManagersY)
EURYeqRoundupSS <- roundup2SS(EUReqManagersY)
EURYeqRoundupIR <- roundup2IR(EUReqManagersY)

EURQeqmx1 <- managerMatrixEQ(EUReqManagersQ)
EURQeqmxcolz <- colnames(EURQeqmx1)
EURQeqmx2 <- managerMatrixEQ(EUReqManagersQ,2)

EURYeqMOS1 <- monthTable(EUReqManagersY)
EURYeqMOS2 <- monthTable(EUReqManagersY,2)

EURYeqmx1 <- managerMatrixEQ(EUReqManagersY)
EURYeqmxcolz <- colnames(EURYeqmx1)
EURYeqmx2 <- managerMatrixEQ(EUReqManagersY,2)

write.xlsx(EURQeqmgrTABLE1, file = "EUReq.xlsx", sheetName = "EURQeqmgrTABLE1", append=TRUE)
write.xlsx(EURQeqmgrTABLE2, file = "EUReq.xlsx", sheetName = "EURQeqmgrTABLE2", append=TRUE)

write.xlsx(EURYeqmgrTABLE1, file = "EUReq.xlsx", sheetName = "EURYeqmgrTABLE1", append=TRUE)
write.xlsx(EURYeqmgrTABLE2, file = "EUReq.xlsx", sheetName = "EURYeqmgrTABLE2", append=TRUE)

write.xlsx(EURYeqRoundupCS, file = "EUReq.xlsx", sheetName = "EURYeqRoundupCS", append=TRUE)
write.xlsx(EURYeqRoundupSS, file = "EUReq.xlsx", sheetName = "EURYeqRoundupSS", append=TRUE)
write.xlsx(EURYeqRoundupIR, file = "EUReq.xlsx", sheetName = "EURYeqRoundupIR", append=TRUE)

write.xlsx(EURQeqmx1, file = "EUReq.xlsx", sheetName = "EURQeqmx1", append=TRUE)
write.xlsx(EURQeqmxcolz, file = "EUReq.xlsx", sheetName = "EURQeqmxcolz", append=TRUE)
write.xlsx(EURQeqmx2, file = "EUReq.xlsx", sheetName = "EURQeqmx2", append=TRUE)

write.xlsx(EURYeqMOS1, file = "EUReq.xlsx", sheetName = "EURYeqMOS1", append=TRUE)
write.xlsx(EURYeqMOS2, file = "EUReq.xlsx", sheetName = "EURYeqMOS2", append=TRUE)

write.xlsx(EURYeqmx1, file = "EUReq.xlsx", sheetName = "EURYeqmx1", append=TRUE)
write.xlsx(EURYeqmxcolz, file = "EUReq.xlsx", sheetName = "EURYeqmxcolz", append=TRUE)
write.xlsx(EURYeqmx2, file = "EUReq.xlsx", sheetName = "EURYeqmx2", append=TRUE)

#FIXED INCOME

USQfimgrTABLE1 <- mgrTable(USFImanagersQ)
USQfimgrTABLE2 <- mgrTable(USFImanagersQ, 2)

USYfimgrTABLE1 <- mgrTable(USFImanagersY)
USYfimgrTABLE2 <- mgrTable(USFImanagersY, 2)


USYfiRoundupAC <- roundup2AC(USFImanagersY)
USYfiRoundupIR <- roundup2IR(USFImanagersY)

USQfimx1 <- managerMatrixAC(USFImanagersQ)
USQfimxcolz <- colnames(USQfimx1)
USQfimx2 <- managerMatrixAC(USFImanagersQ,2)

USYfiMOS1 <- monthTable(USFImanagersY)
USYfiMOS2 <- monthTable(USFImanagersY,2)

USYfimx1 <- managerMatrixAC(USFImanagersY)
USYfimxcolz <- colnames(USYfimx1)
USYfimx2 <- managerMatrixAC(USFImanagersY,2)

write.xlsx(USQfimgrTABLE1, file = "USfi.xlsx", sheetName = "USQfimgrTABLE1", append=TRUE)
write.xlsx(USQfimgrTABLE2, file = "USfi.xlsx", sheetName = "USQfimgrTABLE2", append=TRUE)

write.xlsx(USYfimgrTABLE1, file = "USfi.xlsx", sheetName = "USYfimgrTABLE1", append=TRUE)
write.xlsx(USYfimgrTABLE2, file = "USfi.xlsx", sheetName = "USYfimgrTABLE2", append=TRUE)

write.xlsx(USYfiRoundupAC, file = "USfi.xlsx", sheetName = "USYfiRoundupAC", append=TRUE)
write.xlsx(USYfiRoundupIR, file = "USfi.xlsx", sheetName = "USYfiRoundupIR", append=TRUE)

write.xlsx(USQfimx1, file = "USfi.xlsx", sheetName = "USQfimx1", append=TRUE)
write.xlsx(USQfimxcolz, file = "USfi.xlsx", sheetName = "USQfimxcolz", append=TRUE)
write.xlsx(USQfimx2, file = "USfi.xlsx", sheetName = "USQfimx2", append=TRUE)

write.xlsx(USYfiMOS1, file = "USfi.xlsx", sheetName = "USYfiMOS1", append=TRUE)
write.xlsx(USYfiMOS2, file = "USfi.xlsx", sheetName = "USYfiMOS2", append=TRUE)

write.xlsx(USYfimx1, file = "USfi.xlsx", sheetName = "USYfimx1", append=TRUE)
write.xlsx(USYfimxcolz, file = "USfi.xlsx", sheetName = "USYfimxcolz", append=TRUE)
write.xlsx(USYfimx2, file = "USfi.xlsx", sheetName = "USYfimx2", append=TRUE)


EURQfimgrTABLE1 <- mgrTable(EURFImanagersQ)
EURQfimgrTABLE2 <- mgrTable(EURFImanagersQ, 2)

EURYfimgrTABLE1 <- mgrTable(EURFImanagersY)
EURYfimgrTABLE2 <- mgrTable(EURFImanagersY, 2)


EURYfiRoundupAC <- roundup2AC(EURFImanagersY)
EURYfiRoundupIR <- roundup2IR(EURFImanagersY)

EURQfimx1 <- managerMatrixAC(EURFImanagersQ)
EURQfimxcolz <- colnames(EURQfimx1)
EURQfimx2 <- managerMatrixAC(EURFImanagersQ,2)

EURYfiMOS1 <- monthTable(EURFImanagersY)
EURYfiMOS2 <- monthTable(EURFImanagersY,2)

EURYfimx1 <- managerMatrixAC(EURFImanagersY)
EURYfimxcolz <- colnames(EURYfimx1)
EURYfimx2 <- managerMatrixAC(EURFImanagersY,2)

write.xlsx(EURQfimgrTABLE1, file = "EURfi.xlsx", sheetName = "EURQfimgrTABLE1", append=TRUE)
write.xlsx(EURQfimgrTABLE2, file = "EURfi.xlsx", sheetName = "EURQfimgrTABLE2", append=TRUE)

write.xlsx(EURYfimgrTABLE1, file = "EURfi.xlsx", sheetName = "EURYfimgrTABLE1", append=TRUE)
write.xlsx(EURYfimgrTABLE2, file = "EURfi.xlsx", sheetName = "EURYfimgrTABLE2", append=TRUE)

write.xlsx(EURYfiRoundupAC, file = "EURfi.xlsx", sheetName = "EURYfiRoundupAC", append=TRUE)

write.xlsx(EURYfiRoundupIR, file = "EURfi.xlsx", sheetName = "EURYfiRoundupIR", append=TRUE)

write.xlsx(EURQfimx1, file = "EURfi.xlsx", sheetName = "EURQfimx1", append=TRUE)
write.xlsx(EURQfimxcolz, file = "EURfi.xlsx", sheetName = "EURQfimxcolz", append=TRUE)
write.xlsx(EURQfimx2, file = "EURfi.xlsx", sheetName = "EURQfimx2", append=TRUE)

write.xlsx(EURYfiMOS1, file = "EURfi.xlsx", sheetName = "EURYfiMOS1", append=TRUE)
write.xlsx(EURYfiMOS2, file = "EURfi.xlsx", sheetName = "EURYfiMOS2", append=TRUE)

write.xlsx(EURYfimx1, file = "EURfi.xlsx", sheetName = "EURYfimx1", append=TRUE)
write.xlsx(EURYfimxcolz, file = "EURfi.xlsx", sheetName = "EURYfimxcolz", append=TRUE)
write.xlsx(EURYfimx2, file = "EURfi.xlsx", sheetName = "EURYfimx2", append=TRUE)


#REAL ESTATE
USQremgrTABLE1 <- mgrTable(USREmanagersQ)
USQremgrTABLE2 <- mgrTable(USREmanagersQ, 2)

USYremgrTABLE1 <- mgrTable(USREmanagersY)
USYremgrTABLE2 <- mgrTable(USREmanagersY, 2)


USYreRoundupAC <- roundup2AC(USREmanagersY)
USYreRoundupIR <- roundup2IR(USREmanagersY)

USQremx1 <- managerMatrixAC(USREmanagersQ)
USQremxcolz <- colnames(USQremx1)
USQremx2 <- managerMatrixAC(USREmanagersQ,2)

USYreMOS1 <- monthTable(USREmanagersY)
USYreMOS2 <- monthTable(USREmanagersY,2)

USYremx1 <- managerMatrixAC(USREmanagersY)
USYremxcolz <- colnames(USYremx1)
USYremx2 <- managerMatrixAC(USREmanagersY,2)


write.xlsx(USQremgrTABLE1, file = "USre.xlsx", sheetName = "USQremgrTABLE1", append=TRUE)
write.xlsx(USQremgrTABLE2, file = "USre.xlsx", sheetName = "USQremgrTABLE2", append=TRUE)

write.xlsx(USYremgrTABLE1, file = "USre.xlsx", sheetName = "USYremgrTABLE1", append=TRUE)
write.xlsx(USYremgrTABLE2, file = "USre.xlsx", sheetName = "USYremgrTABLE2", append=TRUE)

write.xlsx(USYreRoundupAC, file = "USre.xlsx", sheetName = "USYreRoundupAC", append=TRUE)
write.xlsx(USYreRoundupIR, file = "USre.xlsx", sheetName = "USYreRoundupIR", append=TRUE)

write.xlsx(USQremx1, file = "USre.xlsx", sheetName = "USQremx1", append=TRUE)
write.xlsx(USQremxcolz, file = "USre.xlsx", sheetName = "USQremxcolz", append=TRUE)
write.xlsx(USQremx2, file = "USre.xlsx", sheetName = "USQremx2", append=TRUE)

write.xlsx(USYreMOS1, file = "USre.xlsx", sheetName = "USYreMOS1", append=TRUE)
write.xlsx(USYreMOS2, file = "USre.xlsx", sheetName = "USYreMOS2", append=TRUE)

write.xlsx(USYremx1, file = "USre.xlsx", sheetName = "USYremx1", append=TRUE)
write.xlsx(USYremxcolz, file = "USre.xlsx", sheetName = "USYremxcolz", append=TRUE)
write.xlsx(USYremx2, file = "USre.xlsx", sheetName = "USYremx2", append=TRUE)

#REAL ESTATE
EURQremgrTABLE1 <- mgrTable(EURREmanagersQ)
EURQremgrTABLE2 <- mgrTable(EURREmanagersQ, 2)

EURYremgrTABLE1 <- mgrTable(EURREmanagersY)
EURYremgrTABLE2 <- mgrTable(EURREmanagersY, 2)


EURYreRoundupAC <- roundup2AC(EURREmanagersY)
EURYreRoundupIR <- roundup2IR(EURREmanagersY)

EURQremx1 <- managerMatrixAC(EURREmanagersQ)
EURQremxcolz <- colnames(EURQremx1)
EURQremx2 <- managerMatrixAC(EURREmanagersQ,2)

EURYreMOS1 <- monthTable(EURREmanagersY)
EURYreMOS2 <- monthTable(EURREmanagersY,2)

EURYremx1 <- managerMatrixAC(EURREmanagersY)
EURYremxcolz <- colnames(EURYremx1)
EURYremx2 <- managerMatrixAC(EURREmanagersY,2)

write.xlsx(EURQremgrTABLE1, file = "EURRE.xlsx", sheetName = "EURQremgrTABLE1", append=TRUE)
write.xlsx(EURQremgrTABLE2, file = "EURRE.xlsx", sheetName = "EURQremgrTABLE2", append=TRUE)

write.xlsx(EURYremgrTABLE1, file = "EURRE.xlsx", sheetName = "EURYremgrTABLE1", append=TRUE)
write.xlsx(EURYremgrTABLE2, file = "EURRE.xlsx", sheetName = "EURYremgrTABLE2", append=TRUE)

write.xlsx(EURYreRoundupAC, file = "EURRE.xlsx", sheetName = "EURYreRoundupAC", append=TRUE)

write.xlsx(EURYreRoundupIR, file = "EURRE.xlsx", sheetName = "EURYreRoundupIR", append=TRUE)

write.xlsx(EURQremx1, file = "EURRE.xlsx", sheetName = "EURQremx1", append=TRUE)
write.xlsx(EURQremxcolz, file = "EURRE.xlsx", sheetName = "EURQremxcolz", append=TRUE)
write.xlsx(EURQremx2, file = "EURRE.xlsx", sheetName = "EURQremx2", append=TRUE)

write.xlsx(EURYreMOS1, file = "EURRE.xlsx", sheetName = "EURYreMOS1", append=TRUE)
write.xlsx(EURYreMOS2, file = "EURRE.xlsx", sheetName = "EURYreMOS2", append=TRUE)

write.xlsx(EURYremx1, file = "EURRE.xlsx", sheetName = "EURYremx1", append=TRUE)
write.xlsx(EURYremxcolz, file = "EURRE.xlsx", sheetName = "EURYremxcolz", append=TRUE)
write.xlsx(EURYremx2, file = "EURRE.xlsx", sheetName = "EURYremx2", append=TRUE)

#PRIVATE EQUITY
USQPEmgrTABLE1 <- mgrTable(USPEmanagersQ)
USQPEmgrTABLE2 <- mgrTable(USPEmanagersQ, 2)

USYPEmgrTABLE1 <- mgrTable(USPEmanagersY)
USYPEmgrTABLE2 <- mgrTable(USPEmanagersY, 2)


USYPERoundupAC <- roundup2AC(USPEmanagersY)
USYPERoundupIR <- roundup2IR(USPEmanagersY)

USQPEmx1 <- managerMatrixAC(USPEmanagersQ)
USQPEmx1colz <- colnames(USQPEmx1)
USQPEmx2 <- managerMatrixAC(USPEmanagersQ,2)

USYPEMOS1 <- monthTable(USPEmanagersY)
USYPEMOS2 <- monthTable(USPEmanagersY,2)

USYPEmx1 <- managerMatrixAC(USPEmanagersY)
USYPEmx1colz <- colnames(USYPEmx1)
USYPEmx2 <- managerMatrixAC(USPEmanagersY,2)

write.xlsx(USQPEmgrTABLE1, file = "USPE.xlsx", sheetName = "USQPEmgrTABLE1", append=TRUE)
write.xlsx(USQPEmgrTABLE2, file = "USPE.xlsx", sheetName = "USQPEmgrTABLE2", append=TRUE)

write.xlsx(USYPEmgrTABLE1, file = "USPE.xlsx", sheetName = "USYPEmgrTABLE1", append=TRUE)
write.xlsx(USYPEmgrTABLE2, file = "USPE.xlsx", sheetName = "USYPEmgrTABLE2", append=TRUE)

write.xlsx(USYPERoundupAC, file = "USPE.xlsx", sheetName = "USYPERoundupAC", append=TRUE)
write.xlsx(USYPERoundupIR, file = "USPE.xlsx", sheetName = "USYPERoundupIR", append=TRUE)

write.xlsx(USQPEmx1, file = "USPE.xlsx", sheetName = "USQPEmx1", append=TRUE)
write.xlsx(USQPEmx1colz, file = "USPE.xlsx", sheetName = "USQPEmx1colz", append=TRUE)
write.xlsx(USQPEmx2, file = "USPE.xlsx", sheetName = "USQPEmx2", append=TRUE)

write.xlsx(USYPEMOS1, file = "USPE.xlsx", sheetName = "USYPEMOS1", append=TRUE)
write.xlsx(USYPEMOS2, file = "USPE.xlsx", sheetName = "USYPEMOS2", append=TRUE)

write.xlsx(USYPEmx1, file = "USPE.xlsx", sheetName = "USYPEmx1", append=TRUE)
write.xlsx(USYPEmx1colz, file = "USPE.xlsx", sheetName = "USYPEmx1colz", append=TRUE)
write.xlsx(USYPEmx2, file = "USPE.xlsx", sheetName = "USYPEmx2", append=TRUE)


EURQPEmgrTABLE1 <- mgrTable(EURPEmanagersQ)
EURQPEmgrTABLE2 <- mgrTable(EURPEmanagersQ, 2)

EURYPEmgrTABLE1 <- mgrTable(EURPEmanagersY)
EURYPEmgrTABLE2 <- mgrTable(EURPEmanagersY, 2)


EURYPERoundupAC <- roundup2AC(EURPEmanagersY)
EURYPERoundupIR <- roundup2IR(EURPEmanagersY)

EURQPEmx1 <- managerMatrixAC(EURPEmanagersQ)
EURQPEmxcolz <- colnames(EURQPEmx1)
EURQPEmx2 <- managerMatrixAC(EURPEmanagersQ,2)

EURYPEMOS1 <- monthTable(EURPEmanagersY)
EURYPEMOS2 <- monthTable(EURPEmanagersY,2)

EURYPEmx1 <- managerMatrixAC(EURPEmanagersY)
EURYPEmxcolz <- colnames(EURYPEmx1)
EURYPEmx2 <- managerMatrixAC(EURPEmanagersY,2)

write.xlsx(EURQPEmgrTABLE1, file = "EURPE.xlsx", sheetName = "EURQPEmgrTABLE1", append=TRUE)
write.xlsx(EURQPEmgrTABLE2, file = "EURPE.xlsx", sheetName = "EURQPEmgrTABLE2", append=TRUE)

write.xlsx(EURYPEmgrTABLE1, file = "EURPE.xlsx", sheetName = "EURYPEmgrTABLE1", append=TRUE)
write.xlsx(EURYPEmgrTABLE2, file = "EURPE.xlsx", sheetName = "EURYPEmgrTABLE2", append=TRUE)

write.xlsx(EURYPERoundupAC, file = "EURPE.xlsx", sheetName = "EURYPERoundupAC", append=TRUE)

write.xlsx(EURYPERoundupIR, file = "EURPE.xlsx", sheetName = "EURYPERoundupIR", append=TRUE)

write.xlsx(EURQPEmx1, file = "EURPE.xlsx", sheetName = "EURQPEmx1", append=TRUE)
write.xlsx(EURQPEmxcolz, file = "EURPE.xlsx", sheetName = "EURQPEmxcolz", append=TRUE)
write.xlsx(EURQPEmx2, file = "EURPE.xlsx", sheetName = "EURQPEmx2", append=TRUE)

write.xlsx(EURYPEMOS1, file = "EURPE.xlsx", sheetName = "EURYPEMOS1", append=TRUE)
write.xlsx(EURYPEMOS2, file = "EURPE.xlsx", sheetName = "EURYPEMOS2", append=TRUE)

write.xlsx(EURYPEmx1, file = "EURPE.xlsx", sheetName = "EURYPEmx1", append=TRUE)
write.xlsx(EURYPEmxcolz, file = "EURPE.xlsx", sheetName = "EURYPEmxcolz", append=TRUE)
write.xlsx(EURYPEmx2, file = "EURPE.xlsx", sheetName = "EURYPEmx2", append=TRUE)

#HEDGE FUNDS
USQHFmgrTABLE1 <- mgrTable(USHFmanagersQ)
USQHFmgrTABLE2 <- mgrTable(USHFmanagersQ, 2)

USYHFmgrTABLE1 <- mgrTable(USHFmanagersY)
USYHFmgrTABLE2 <- mgrTable(USHFmanagersY, 2)


USYHFRoundupAC <- roundup2AC(USHFmanagersY)
USYHFRoundupIR <- roundup2IR(USHFmanagersY)

USQHFmx1 <- managerMatrixAC(USHFmanagersQ)
USQHFmx1colz <- colnames(USQHFmx1)
USQHFmx2 <- managerMatrixAC(USHFmanagersQ,2)

USYHFMOS1 <- monthTable(USHFmanagersY)
USYHFMOS2 <- monthTable(USHFmanagersY,2)

USYHFmx1 <- managerMatrixAC(USHFmanagersY)
USYHFmx1colz <- colnames(USYHFmx1)
USYHFmx2 <- managerMatrixAC(USHFmanagersY,2)
write.xlsx(USQHFmgrTABLE1, file = "USHF.xlsx", sheetName = "USQHFmgrTABLE1", append=TRUE)
write.xlsx(USQHFmgrTABLE2, file = "USHF.xlsx", sheetName = "USQHFmgrTABLE2", append=TRUE)

write.xlsx(USYHFmgrTABLE1, file = "USHF.xlsx", sheetName = "USYHFmgrTABLE1", append=TRUE)
write.xlsx(USYHFmgrTABLE2, file = "USHF.xlsx", sheetName = "USYHFmgrTABLE2", append=TRUE)

write.xlsx(USYHFRoundupAC, file = "USHF.xlsx", sheetName = "USYHFRoundupAC", append=TRUE)
write.xlsx(USYHFRoundupIR, file = "USHF.xlsx", sheetName = "USYHFRoundupIR", append=TRUE)

write.xlsx(USQHFmx1, file = "USHF.xlsx", sheetName = "USQHFmx1", append=TRUE)
write.xlsx(USQHFmx1colz, file = "USHF.xlsx", sheetName = "USQHFmx1colz", append=TRUE)
write.xlsx(USQHFmx2, file = "USHF.xlsx", sheetName = "USQHFmx2", append=TRUE)

write.xlsx(USYHFMOS1, file = "USHF.xlsx", sheetName = "USYHFMOS1", append=TRUE)
write.xlsx(USYHFMOS2, file = "USHF.xlsx", sheetName = "USYHFMOS2", append=TRUE)

write.xlsx(USYHFmx1, file = "USHF.xlsx", sheetName = "USYHFmx1", append=TRUE)
write.xlsx(USYHFmx1colz, file = "USHF.xlsx", sheetName = "USYHFmx1colz", append=TRUE)
write.xlsx(USYHFmx2, file = "USHF.xlsx", sheetName = "USYHFmx2", append=TRUE)


EURQHFmgrTABLE1 <- mgrTable(EURHFmanagersQ)
EURQHFmgrTABLE2 <- mgrTable(EURHFmanagersQ, 2)

EURYHFmgrTABLE1 <- mgrTable(EURHFmanagersY)
EURYHFmgrTABLE2 <- mgrTable(EURHFmanagersY, 2)


EURYHFRoundupAC <- roundup2AC(EURHFmanagersY)
EURYHFRoundupIR <- roundup2IR(EURHFmanagersY)

EURQHFmx1 <- managerMatrixAC(EURHFmanagersQ)
EURQHFmxcolz <- colnames(EURQHFmx1)
EURQHFmx2 <- managerMatrixAC(EURHFmanagersQ,2)

EURYHFMOS1 <- monthTable(EURHFmanagersY)
EURYHFMOS2 <- monthTable(EURHFmanagersY,2)

EURYHFmx1 <- managerMatrixAC(EURHFmanagersY)
EURYHFmxcolz <- colnames(EURYHFmx1)
EURYHFmx2 <- managerMatrixAC(EURHFmanagersY,2)

write.xlsx(EURQHFmgrTABLE1, file = "EURHF.xlsx", sheetName = "EURQHFmgrTABLE1", append=TRUE)
write.xlsx(EURQHFmgrTABLE2, file = "EURHF.xlsx", sheetName = "EURQHFmgrTABLE2", append=TRUE)

write.xlsx(EURYHFmgrTABLE1, file = "EURHF.xlsx", sheetName = "EURYHFmgrTABLE1", append=TRUE)
write.xlsx(EURYHFmgrTABLE2, file = "EURHF.xlsx", sheetName = "EURYHFmgrTABLE2", append=TRUE)

write.xlsx(EURYHFRoundupAC, file = "EURHF.xlsx", sheetName = "EURYHFRoundupAC", append=TRUE)

write.xlsx(EURYHFRoundupIR, file = "EURHF.xlsx", sheetName = "EURYHFRoundupIR", append=TRUE)

write.xlsx(EURQHFmx1, file = "EURHF.xlsx", sheetName = "EURQHFmx1", append=TRUE)
write.xlsx(EURQHFmxcolz, file = "EURHF.xlsx", sheetName = "EURQHFmxcolz", append=TRUE)
write.xlsx(EURQHFmx2, file = "EURHF.xlsx", sheetName = "EURQHFmx2", append=TRUE)

write.xlsx(EURYHFMOS1, file = "EURHF.xlsx", sheetName = "EURYHFMOS1", append=TRUE)
write.xlsx(EURYHFMOS2, file = "EURHF.xlsx", sheetName = "EURYHFMOS2", append=TRUE)

write.xlsx(EURYHFmx1, file = "EURHF.xlsx", sheetName = "EURYHFmx1", append=TRUE)
write.xlsx(EURYHFmxcolz, file = "EURHF.xlsx", sheetName = "EURYHFmxcolz", append=TRUE)
write.xlsx(EURYHFmx2, file = "EURHF.xlsx", sheetName = "EURYHFmx2", append=TRUE)

#OTHER ALTERNATIVES
USQOAmgrTABLE1 <- mgrTable(USOAmanagersQ)
USQOAmgrTABLE2 <- mgrTable(USOAmanagersQ, 2)

USYOAmgrTABLE1 <- mgrTable(USOAmanagersY)
USYOAmgrTABLE2 <- mgrTable(USOAmanagersY, 2)

USYOARoundupIR <- roundup2IR(USOAmanagersY)

USQOAmx1 <- managerMatrixAC2(USOAmanagersQ)
USQOAmx1colz <- colnames(USQOAmx1)
USQOAmx2 <- managerMatrixAC2(USOAmanagersQ,2)

USYOAMOS1 <- monthTable(USOAmanagersY)
USYOAMOS2 <- monthTable(USOAmanagersY,2)

USYOAmx1 <- managerMatrixAC2(USOAmanagersY)
USYOAmx1colz <- colnames(USYOAmx1)
USYOAmx2 <- managerMatrixAC2(USOAmanagersY,2)

write.xlsx(USQOAmgrTABLE1, file = "USOA.xlsx", sheetName = "USQOAmgrTABLE1", append=TRUE)
write.xlsx(USQOAmgrTABLE2, file = "USOA.xlsx", sheetName = "USQOAmgrTABLE2", append=TRUE)
write.xlsx(USYOAmgrTABLE1, file = "USOA.xlsx", sheetName = "USYOAmgrTABLE1", append=TRUE)
write.xlsx(USYOAmgrTABLE2, file = "USOA.xlsx", sheetName = "USYOAmgrTABLE2", append=TRUE)
write.xlsx(USYOARoundupIR, file = "USOA.xlsx", sheetName = "USYOARoundupIR", append=TRUE)
write.xlsx(USQOAmx1, file = "USOA.xlsx", sheetName = "USQOAmx1", append=TRUE)
write.xlsx(USQOAmx1colz, file = "USOA.xlsx", sheetName = "USQOAmx1colz", append=TRUE)

write.xlsx(USQOAmx2, file = "USOA.xlsx", sheetName = "USQOAmx2", append=TRUE)
write.xlsx(USYOAMOS1, file = "USOA.xlsx", sheetName = "USYOAMOS1", append=TRUE)
write.xlsx(USYOAMOS2, file = "USOA.xlsx", sheetName = "USYOAMOS2", append=TRUE)
write.xlsx(USYOAmx1, file = "USOA.xlsx", sheetName = "USYOAmx1", append=TRUE)
write.xlsx(USYOAmx1colz, file = "USOA.xlsx", sheetName = "USYOAmx1colz", append=TRUE)
write.xlsx(USYOAmx2, file = "USOA.xlsx", sheetName = "USYOAmx2", append=TRUE)


EURQOAmgrTABLE1 <- mgrTable(EUROAmanagersQ)
EURQOAmgrTABLE2 <- mgrTable(EUROAmanagersQ, 2)

EURYOAmgrTABLE1 <- mgrTable(EUROAmanagersY)
EURYOAmgrTABLE2 <- mgrTable(EUROAmanagersY, 2)

EURYOARoundupIR <- roundup2IR(EUROAmanagersY)

EURQOAmx1 <- managerMatrixAC2(EUROAmanagersQ)
EURQOAmxcolz <- colnames(EURQOAmx1)
EURQOAmx2 <- managerMatrixAC2(EUROAmanagersQ,2)

EURYOAMOS1 <- monthTable(EUROAmanagersY)
EURYOAMOS2 <- monthTable(EUROAmanagersY,2)

EURYOAmx1 <- managerMatrixAC2(EUROAmanagersY)
EURYOAmxcolz <- colnames(EURYOAmx1)
EURYOAmx2 <- managerMatrixAC2(EUROAmanagersY,2)

write.xlsx(EURQOAmgrTABLE1, file = "EUROA.xlsx", sheetName = "EURQOAmgrTABLE1", append=TRUE)
write.xlsx(EURQOAmgrTABLE2, file = "EUROA.xlsx", sheetName = "EURQOAmgrTABLE2", append=TRUE)
write.xlsx(EURYOAmgrTABLE1, file = "EUROA.xlsx", sheetName = "EURYOAmgrTABLE1", append=TRUE)
write.xlsx(EURYOAmgrTABLE2, file = "EUROA.xlsx", sheetName = "EURYOAmgrTABLE2", append=TRUE)
write.xlsx(EURYOARoundupIR, file = "EUROA.xlsx", sheetName = "EURYOARoundupIR", append=TRUE)
write.xlsx(EURQOAmx1, file = "EUROA.xlsx", sheetName = "EURQOAmx1", append=TRUE)
write.xlsx(EURQOAmxcolz, file = "EUROA.xlsx", sheetName = "EURQOAmxcolz", append=TRUE)

write.xlsx(EURQOAmx2, file = "EUROA.xlsx", sheetName = "EURQOAmx2", append=TRUE)
write.xlsx(EURYOAMOS1, file = "EUROA.xlsx", sheetName = "EURYOAMOS1", append=TRUE)
write.xlsx(EURYOAMOS2, file = "EUROA.xlsx", sheetName = "EURYOAMOS2", append=TRUE)
write.xlsx(EURYOAmx1, file = "EUROA.xlsx", sheetName = "EURYOAmx1", append=TRUE)
write.xlsx(EURYOAmxcolz, file = "EUROA.xlsx", sheetName = "EURYOAmxcolz", append=TRUE)
write.xlsx(EURYOAmx2, file = "EUROA.xlsx", sheetName = "EURYOAmx2", append=TRUE)
