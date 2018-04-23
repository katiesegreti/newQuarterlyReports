Europe <- c("Austria","Belgium","Bulgaria","Bosnia-Herzegovina","Channel Islands", 
            "Croatia", "Cyprus","Czech Republic","Denmark","Estonia","Finland","France",
            "Germany","Greece", "Guernsey","Hungary","Iceland","Ireland","Italy",
            "Kosovo","Latvia","Liechtenstein","Lithuania","Luxembourg","Macedonia",
            "Monaco","Montenegro","Netherlands","Norway","Poland","Portugal",
            "Romania","Russian Federation","Serbia","Slovenia","Spain","Sweden",
            "Switzerland","Eastern Europe","Euro-Zone","Europe","Europe (ex U.K.)",
            "Nordics","Northern Ireland","West Europe","Baltic States" , "United Kingdom")
Asia <- c("Azerbaijan","Bangladesh","Bhutan","Brunei","Camboida","China","Hong Kong",
          "India","Indonesia","Iran","Japan","Kazakhstan","Lebanon","Macau",
          "Malaysia","Myanmar","Nepal","North Korea","Pakistan","Philippines",
          "Singapore","Sri Lanka","Taiwan","Thailand","Timor-Leste","Turkey",
          "Vietnam","Korea","South Korea","Asia","Asia (ex Japan)","Asia Pacific" ,
          "Asia Pacific (ex Japan)")
Oceania <- c("Australia","New Zealand","Papau New Guinea","Tanzania","Australasia")
#MEA for mandate region (we don't do separate reports on funds in these regions)
MEA <- c("Africa","Botswana","Egypt","Ghana","Israel","Kenya","Kuwait","Namibia","Nigeria","Oman","Saudi Arabia","South Africa","Tanzania","Zimbabwe")
#Emerging Markets for mandate region only
Emerging <- c("Emerging Markets","Emerging Markets Asia","Emerging Markets Latin America","Frontier Markets")
#Other Americas (non-US) for mandate region only
OAmericas <- c("Brazil","Canada","Latin America","North America")
#Nordics for "spotlight on"
Nordics <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")

y2017start <- as.POSIXct("2017-01-01")
y2016start <- as.POSIXct("2016-01-01")
y2015start <- as.POSIXct("2015-01-01")

q1start <- as.POSIXct("2017-01-01")
q2start <- as.POSIXct("2017-04-01")
q3start <- as.POSIXct("2017-07-01")
q4start <- as.POSIXct("2017-10-01")



alternatives <- c("Alternative", "Commodities", "Derivatives", "Distressed Debt",
                  "Real Assets", "Tactical Asset Allocation")
alternativesAll <- c(alternatives, "Private Equity", "Hedge Funds")
other <- c("Balanced", "Cash", "Currency", "ETF", "Unspecified")


##to-do for this section: find a dataframe that has currency/rate info
##join rate to factor(mandatesizecurrency) list
managers$AccountSizeCurrency <- factor(managers$AccountSizeCurrency)
levels(managers$AccountSizeCurrency)
AccountSizeCurrency <- as.vector(levels(managers$AccountSizeCurrency))
XR <- as.vector(c(3.67250, 1.24927, 3.19701, 1.24676, 0.96179, 6.40173,
                      6.07874, 0.81668, 0.71849, 7.81822, 63.7820,
                      110.84, 1068.49, 3.93512, 7.85896, 1.36955,
                      50.8817, 8.03068, 1.31988, 31.8191, 29.2522,
                      3613.99, 1.0, 12.0783))

xrateLookup <- data.frame(AccountSizeCurrency, XR)


