Europe <- c("Austria", "Azerbaijan", "Belgium", "Cyprus", "Denmark", "Finland", "France",
            "Germany", "Ireland", "Italy", "Liechtenstein", "Luxembourg", "Netherlands", 
            "Norway", "Sweden", "Switzerland", "United Kingdom")
Asia <- c("China", "Hong Kong", "India", "Japan", "Malaysia", "Philippines", "Singapore",
          "South Korea", "Taiwan", "Thailand", "Macau")
Oceania <- c("Australia", "New Zealand", "Fiji")


y2017start <- as.POSIXct("2017-01-01")
y2016start <- as.POSIXct("2016-01-01")
y2015start <- as.POSIXct("2015-01-01")

q1start <- as.POSIXct("2017-01-01")
q2start <- as.POSIXct("2017-04-01")
q3start <- as.POSIXct("2017-07-01")
q4start <- as.POSIXct("2017-10-01")

mandates$MainAssetClass <- factor(mandates$MainAssetClass)
levels(mandates$MainAssetClass)

alternatives <- c("Alternative", "Commodities", "Derivatives", "Distressed Debt",
                  "Real Assets", "Tactical Asset Allocation")
other <- c("Balanced", "Cash", "Currency", "ETF", "Unspecified")

mandates$MandateSizeCurrency <- factor(mandates$MandateSizeCurrency)
levels(mandates$MandateSizeCurrency)
MandateSizeCurrency <- as.vector(levels(mandates$MandateSizeCurrency))
XR <- as.vector(c(1.24927, 3.19701, 1.24676, 0.96179, 6.40173,
                      6.07874, 0.81668, 0.71849, 7.81822, 63.7820,
                      110.84, 1068.49, 3.93512, 7.85896, 1.36955,
                      50.8817, 8.03068, 1.31988, 31.8191, 29.2522,
                      3613.99, 1.0, 12.0783))

xrateLookup <- data.frame(MandateSizeCurrency, XR)


status <- c("Potential", "New", "Completed")
status2 <- c("Potential1", "Potential2", "New1", "New2", "Completed1", "Completed2")
