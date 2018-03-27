#######################CREATE DATA FRAMES############################
#######################Create regional data frames####################
USmandates <- mandates[mandates$FundRegion == "US",]
UKmandates <- mandates[mandates$FundRegion == "UK",]
EURmandates <- mandates[mandates$FundRegion == "EUR",]
AUSmandates <- mandates[mandates$FundRegion == "AUS",]
ASIAmandates <- mandates[mandates$FundRegion == "ASIA",]
#######################Create US Quarterly and 12 month dataframes####
USQ <- USmandates[USmandates$Quarter == "Q4",] #US quarterly all mandates
USQP <- USQ[USQ$SearchStatus == "Potential",] #US quarterly POTENTIAL
USQN <- USQ[USQ$SearchStatus == "New",]  #US quarterly NEW
USQC <- USQ[USQ$SearchStatus == "Completed",]  #US quarterly COMPLETED
US12mo <- USmandates[USmandates$Year == "2017" & USmandates$SearchStatus == "Completed",]
US3y <- USmandates[USmandates$SearchStatus == "Completed",]
###################Create UK Quarterly and 12 month dataframes####
UKQ <- UKmandates[UKmandates$Quarter == "Q4",] #UK quarterly all mandates
UKQP <- UKQ[UKQ$SearchStatus == "Potential",] #UK quarterly POTENTIAL
UKQN <- UKQ[UKQ$SearchStatus == "New",]  #UK quarterly NEW
UKQC <- UKQ[UKQ$SearchStatus == "Completed",]  #UK quarterly COMPLETED
UK12mo <- UKmandates[UKmandates$Year == "2017" & UKmandates$SearchStatus == "Completed",]
UK3y <- UKmandates[UKmandates$SearchStatus == "Completed",]
###################Create EUROPE EX-UK Quarterly and 12 month dataframes####
EURQ <- EURmandates[EURmandates$Quarter == "Q4",] #EUR quarterly all mandates
EURQP <- EURQ[EURQ$SearchStatus == "Potential",] #EUR quarterly POTENTIAL
EURQN <- EURQ[EURQ$SearchStatus == "New",]  #EUR quarterly NEW
EURQC <- EURQ[EURQ$SearchStatus == "Completed",]  #EUR quarterly COMPLETED
EUR3y <- EURmandates[EURmandates$SearchStatus == "Completed",]
EUR12mo <- EURmandates[EURmandates$Year == "2017" & EURmandates$SearchStatus == "Completed",]
###################Create ASIA Quarterly and 12 month dataframes####
ASIAQ <- ASIAmandates[ASIAmandates$Quarter == "Q4",] #ASIA quarterly all mandates
ASIAQP <- ASIAQ[ASIAQ$SearchStatus == "Potential",] #ASIA quarterly POTENTIAL
ASIAQN <- ASIAQ[ASIAQ$SearchStatus == "New",]  #ASIA quarterly NEW
ASIAQC <- ASIAQ[ASIAQ$SearchStatus == "Completed",]  #ASIA quarterly COMPLETED
ASIA12mo <- ASIAmandates[ASIAmandates$Year == "2017" & ASIAmandates$SearchStatus == "Completed",]
ASIA3y <- ASIAmandates[ASIAmandates$SearchStatus == "Completed",]
###################Create OCEANIA Quarterly and 12 month dataframes####
AUSQ <- AUSmandates[AUSmandates$Quarter == "Q4",] #AUS quarterly all mandates
AUSQP <- AUSQ[AUSQ$SearchStatus == "Potential",] #AUS quarterly POTENTIAL
AUSQN <- AUSQ[AUSQ$SearchStatus == "New",]  #AUS quarterly NEW
AUSQC <- AUSQ[AUSQ$SearchStatus == "Completed",]  #AUS quarterly COMPLETED
AUS12mo <- AUSmandates[AUSmandates$Year == "2017" & AUSmandates$SearchStatus == "Completed",]
AUS3y <- AUSmandates[AUSmandates$SearchStatus == "Completed",]
