#######################CREATE DATA FRAMES############################
#######################Create regional data frames####################
USmandates <- filter(mandates, FundRegion == "US")
UKmandates <- filter(mandates, FundRegion == "UK")
EURmandates <- filter(mandates, FundRegion == "EUR")
AUSmandates <- filter(mandates, FundRegion == "AUS")
ASIAmandates <- filter(mandates, FundRegion == "ASIA")
#######################Create US Quarterly and 12 month dataframes####
USQ <- filter(USmandates, Quarter == "Q4") #US quarterly all mandates
USQP <- filter(USQ, SearchStatus == "Potential") #US quarterly POTENTIAL
USQN <- filter(USQ, SearchStatus == "New")  #US quarterly NEW
USQC <- filter(USQ, SearchStatus == "Completed")  #US quarterly COMPLETED
US12mo <- filter(USmandates, Year == "2017" & SearchStatus == "Completed")
US3y <- filter(USmandates, SearchStatus == "Completed")
###################Create UK Quarterly and 12 month dataframes####
UKQ <- filter(UKmandates, Quarter == "Q4") #UK quarterly all mandates
UKQP <- filter(UKQ, SearchStatus == "Potential") #UK quarterly POTENTIAL
UKQN <- filter(UKQ, SearchStatus == "New")  #UK quarterly NEW
UKQC <- filter(UKQ, SearchStatus == "Completed")  #UK quarterly COMPLETED
UK12mo <- filter(UKmandates, Year == "2017" & SearchStatus == "Completed")
UK3y <- filter(UKmandates, SearchStatus == "Completed")
###################Create EUROPE EX-UK Quarterly and 12 month dataframes####
EURQ <- filter(EURmandates, Quarter == "Q4") #EUR quarterly all mandates
EURQP <- filter(EURQ, SearchStatus == "Potential") #EUR quarterly POTENTIAL
EURQN <- filter(EURQ, SearchStatus == "New")  #EUR quarterly NEW
EURQC <- filter(EURQ, SearchStatus == "Completed")  #EUR quarterly COMPLETED
EUR12mo <- filter(EURmandates, Year == "2017" & SearchStatus == "Completed")
EUR3y <- filter(EURmandates,SearchStatus == "Completed")
###################Create ASIA Quarterly and 12 month dataframes####
ASIAQ <- filter(ASIAmandates, Quarter == "Q4") #ASIA quarterly all mandates
ASIAQP <- filter(ASIAQ, SearchStatus == "Potential") #ASIA quarterly POTENTIAL
ASIAQN <- filter(ASIAQ, SearchStatus == "New")  #ASIA quarterly NEW
ASIAQC <- filter(ASIAQ, SearchStatus == "Completed")  #ASIA quarterly COMPLETED
ASIA12mo <- filter(ASIAmandates, Year == "2017" & SearchStatus == "Completed")
ASIA3y <- filter(ASIAmandates, SearchStatus == "Completed")
###################Create OCEANIA Quarterly and 12 month dataframes####
AUSQ <- filter(AUSmandates, Quarter == "Q4") #AUS quarterly all mandates
AUSQP <- filter(AUSQ, SearchStatus == "Potential") #AUS quarterly POTENTIAL
AUSQN <- filter(AUSQ, SearchStatus == "New")  #AUS quarterly NEW
AUSQC <- filter(AUSQ, SearchStatus == "Completed")  #AUS quarterly COMPLETED
AUS12mo <- filter(AUSmandates, Year == "2017" & SearchStatus == "Completed")
AUS3y <- filter(AUSmandates, SearchStatus == "Completed")
