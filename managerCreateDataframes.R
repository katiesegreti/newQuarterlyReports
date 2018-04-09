
#######################CREATE DATA FRAMES############################
#######################Create regional data frames(12 months)########
ALLmanagersY <- managers[managers$Year == "2017",]
USmanagersY <- managers[managers$FundRegion == "US" & managers$Year == "2017",]
##UKmanagersY <- managers[managers$FundRegion == "UK" & managers$Year == "2017",]
EURmanagersY <- managers[managers$FundRegion == "EUR" & managers$Year == "2017",]
AUSmanagersY <- managers[managers$FundRegion == "AUS" & managers$Year == "2017",]
ASIAmanagersY <- managers[managers$FundRegion == "ASIA" & managers$Year == "2017",]
#######################Create regional data frames(quarterly)########
ALLmanagersQ <- ALLmanagersY[ALLmanagersY$Quarter == "Q4",]
USmanagersQ <- USmanagersY[USmanagersY$Quarter == "Q4",]
##UKmanagersQ <- UKmanagersY[UKmanagersY$Quarter == "Q4",]
EURmanagersQ <- EURmanagersY[EURmanagersY$Quarter == "Q4",]
AUSmanagersQ <- AUSmanagersY[AUSmanagersY$Quarter == "Q4",]
ASIAmanagersQ <- ASIAmanagersY[ASIAmanagersY$Quarter == "Q4",]

#spotlights for Europe report
UKmanagersY <- EURmanagersY[EURmanagersY$FundCountry == "United Kingdom",]

NordicmanagersY <- EURmanagersY[EURmanagersY$FundCountry %in% Nordics,]


#asset class specific manager reports
USeqManagersQ <- USmanagersQ[USmanagersQ$MainAssetClass == "Equity",]
USeqManagersY <- USmanagersY[USmanagersY$MainAssetClass == "Equity",]
USFImanagersQ <- USmanagersQ[USmanagersQ$MainAssetClass == "Fixed Income",]
USFImanagersY <- USmanagersY[USmanagersY$MainAssetClass == "Fixed Income",]
USREmanagersQ <- USmanagersQ[USmanagersQ$MainAssetClass == "Real Estate",]
USREmanagersY <- USmanagersY[USmanagersY$MainAssetClass == "Real Estate",]
USPEmanagersQ <- USmanagersQ[USmanagersQ$MainAssetClass == "Private Equity",]
USPEmanagersY <- USmanagersY[USmanagersY$MainAssetClass == "Private Equity",]
USHFmanagersQ <- USmanagersQ[USmanagersQ$MainAssetClass == "Hedge Funds",]
USHFmanagersY <- USmanagersY[USmanagersY$MainAssetClass == "Hedge Funds",]
USOAmanagersQ <- USmanagersQ[USmanagersQ$MainAssetClass %in% alternatives,]
USOAmanagersY <- USmanagersY[USmanagersY$MainAssetClass %in% alternatives,]


EUReqManagersQ <- EURmanagersQ[EURmanagersQ$MainAssetClass == "Equity",]
EUReqManagersY <- EURmanagersY[EURmanagersY$MainAssetClass == "Equity",]
EURFImanagersQ <- EURmanagersQ[EURmanagersQ$MainAssetClass == "Fixed Income",]
EURFImanagersY <- EURmanagersY[EURmanagersY$MainAssetClass == "Fixed Income",]
EURREmanagersQ <- EURmanagersQ[EURmanagersQ$MainAssetClass == "Real Estate",]
EURREmanagersY <- EURmanagersY[EURmanagersY$MainAssetClass == "Real Estate",]

EUROAmanagersQ <- EURmanagersQ[EURmanagersQ$MainAssetClass %in% alternativesAll,]
EUROAmanagersY <- EURmanagersY[EURmanagersY$MainAssetClass %in% alternativesAll,]












