
#######################CREATE DATA FRAMES############################
#######################Create regional data frames(12 months)########
ALLmanagersY <- filter(managers, Year == "2017")
USmanagersY <- filter(managers, FundRegion == "US" & Year == "2017")
##UKmanagersY <- filter(managers, FundRegion == "UK" & Year == "2017")
EURmanagersY <- filter(managers, FundRegion == "EUR" & Year == "2017")
AUSmanagersY <- filter(managers, FundRegion == "AUS" & Year == "2017")
ASIAmanagersY <- filter(managers, FundRegion == "ASIA" & Year == "2017")
#######################Create regional data frames(quarterly)########
ALLmanagersQ <- filter(ALLmanagersY, Quarter == "Q4")
USmanagersQ <- filter(USmanagersY, Quarter == "Q4")
##UKmanagersQ <- filter(UKmanagersY, Quarter == "Q4")
EURmanagersQ <- filter(EURmanagersY, Quarter == "Q4")
AUSmanagersQ <- filter(AUSmanagersY, Quarter == "Q4")
ASIAmanagersQ <- filter(ASIAmanagersY, Quarter == "Q4")

#spotlights for Europe report
UKmanagersY <- filter(EURmanagersY, FundCountry == "United Kingdom")

NordicmanagersY <- filter(EURmanagersY, FundCountry %in% Nordics)


#asset class specific manager reports
USeqManagersQ <- filter(USmanagersQ, MainAssetClass == "Equity")
USeqManagersY <- filter(USmanagersY, MainAssetClass == "Equity")
USFImanagersQ <- filter(USmanagersQ, MainAssetClass == "Fixed Income")
USFImanagersY <- filter(USmanagersY, MainAssetClass == "Fixed Income")
USREmanagersQ <- filter(USmanagersQ, MainAssetClass == "Real Estate")
USREmanagersY <- filter(USmanagersY, MainAssetClass == "Real Estate")
USPEmanagersQ <- filter(USmanagersQ, MainAssetClass == "Private Equity")
USPEmanagersY <- filter(USmanagersY, MainAssetClass == "Private Equity")
USHFmanagersQ <- filter(USmanagersQ, MainAssetClass == "Hedge Funds")
USHFmanagersY <- filter(USmanagersY, MainAssetClass == "Hedge Funds")
USOAmanagersQ <- filter(USmanagersQ, MainAssetClass %in% alternatives)
USOAmanagersY <- filter(USmanagersY, MainAssetClass %in% alternatives)


EUReqManagersQ <- filter(EURmanagersQ, MainAssetClass == "Equity")
EUReqManagersY <- filter(EURmanagersY, MainAssetClass == "Equity")
EURFImanagersQ <- filter(EURmanagersQ, MainAssetClass == "Fixed Income")
EURFImanagersY <- filter(EURmanagersY, MainAssetClass == "Fixed Income")
EURREmanagersQ <- filter(EURmanagersQ, MainAssetClass == "Real Estate")
EURREmanagersY <- filter(EURmanagersY, MainAssetClass == "Real Estate")

EUROAmanagersQ <- filter(EURmanagersQ, MainAssetClass %in% alternativesAll)
EUROAmanagersY <- filter(EURmanagersY, MainAssetClass %in% alternativesAll)












