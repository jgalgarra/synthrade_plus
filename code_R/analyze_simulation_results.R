# Analysis of simulation log
#
# symlog fields:
#         Instant: FT/TT    (Formation Time, Total Time)
#         Year
#         Experiment Number
#         Simulation step
#         Token Count
#         Link Count
#         Total mumber of Links

# Read simulation log
symlog <- read.csv("../results/symlog.txt", header=FALSE, sep=";")
datalog <- symlog
names(datalog) <- c("Instant", "Year", "NumExper", "SimStep", "TokenCount", "LinkCount", "TotalLinks")
# Read network data from NUMLINKS file
ndata <- read.csv("../results/NUMLINKS.txt", sep="")

# Add the number of importers and exporters
datalog$NExp <- 0
datalog$NImp <- 0
datalog$Connectance <- 0
for (i in 1:nrow(datalog)){
  datalog$NExp[i] <- ndata[ndata$year==datalog$Year[i],]$nexp
  datalog$NImp[i] <- ndata[ndata$year==datalog$Year[i],]$nimp
  datalog$Connectance[i] <- ndata[ndata$year==datalog$Year[i],]$connectance
}

# Ratios 
datalog$RatioTTFT <- 0
datalog$RatioLinks <- 0
datalog$RatioTokens <- 0
datalog$RatioTTLinks <- 0



for (i in 1:nrow(datalog))
  if (datalog$Instant[i]=="TT"){
    datalog$RatioTTFT[i] <- datalog$SimStep[i] / datalog[(datalog$Year == datalog$Year[i]) &
                                                 (datalog$NumExper == datalog$NumExper[i]) &
                                                 (datalog$Instant=="FT"),]$SimStep

    datalog$RatioLinks[i] <- datalog$LinkCount[i] / datalog[(datalog$Year == datalog$Year[i]) &
                                                      (datalog$NumExper == datalog$NumExper[i]) &
                                                      (datalog$Instant=="FT"),]$LinkCount
    datalog$RatioTokens[i] <- datalog$TokenCount[i] / datalog[(datalog$Year == datalog$Year[i]) &
                                                             (datalog$NumExper == datalog$NumExper[i]) &
                                                             (datalog$Instant=="FT"),]$TokenCount
    datalog$RatioTTLinks[i] <- datalog$SimStep[i] / datalog[(datalog$Year == datalog$Year[i]) &
                                                               (datalog$NumExper == datalog$NumExper[i]) &
                                                               (datalog$Instant=="FT"),]$TotalLinks

    
  }
boxplot(datalog[(datalog$Instant=="TT"),]$RatioTTFT~ datalog[(datalog$Instant=="TT"),]$Year, main="Times ratio")
boxplot(datalog[(datalog$Instant=="TT"),]$RatioLinks ~ datalog[(datalog$Instant=="TT"),]$Year, main="Links ratio")
boxplot(datalog[(datalog$Instant=="TT"),]$RatioTokens ~ datalog[(datalog$Instant=="TT"),]$Year, main="Tokens ratio")