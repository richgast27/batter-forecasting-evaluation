##Clear working directory
rm(list=ls())



##Load libraries 
library(ggplot2)
library(readxl)
library(cluster)
library(forecast)


##Load data on 2022 free agent batters 
FreeAgentBatters <- read.csv("2022 Free Agent Batters Careers.csv")


##Load data on 2022 batting stats at the team level
TeamBatting <- read.csv("2022 Team Batting.csv")




############### Data Management on Batting Data Sets ###############
############### Data Management on Batting Data Sets ###############
############### Data Management on Batting Data Sets ###############



##Removing % symbol and converting observations from character to numeric
TeamBatting <- as.data.frame(lapply(TeamBatting, 
                                    function(y) gsub("%", "", y)))

TeamBatting[,-1] <- lapply(TeamBatting[,-1], 
                           function(x) as.numeric(as.character(x)))


##Creating subset of the bottom teams in league when it comes to 
##three stats that indicate power; Home Runs hit, Slugging percentage, and
##HR-Flyball percentage. 
HRSort <- TeamBatting[order(TeamBatting$HR),]
LowHR <- head(HRSort,10)

SLGSort <- TeamBatting[order(TeamBatting$SLG),]
LowSLG <- head(SLGSort,10)

HRFBSort <- TeamBatting[order(TeamBatting$HR.FB),]
LowHRFB <- head(SLGSort,10)



##Creating subset of teams that exist in all three subsets 
LowPowerTeams <- LowHR[LowHR$Team %in% c(LowSLG$Team), ]
LowPowerTeams <- LowPowerTeams[LowPowerTeams$Team %in% c(LowHRFB$Team), ]


##Creating subset of the bottom teams in league when it comes to 
##small ball; walk rate, on base percentage, and total steals
BBSort <- TeamBatting[order(TeamBatting$BB.),]
LowBB <- head(BBSort,10)

OBPSort <- TeamBatting[order(TeamBatting$OBP),]
LowOBP <- head(OBPSort,10)

SBSort <- TeamBatting[order(TeamBatting$SB),]
LowSB <- head(SBSort,10)

LowSmallBall <- LowBB[LowBB$Team %in% c(LowOBP$Team), ]
LowSmallBall <- LowSmallBall[LowSmallBall$Team %in% c(LowSB$Team), ]




############### Batter Hierarchical Clustering ###############
############### Batter Hierarchical Clustering ###############
############### Batter Hierarchical Clustering ###############


##Removing % symbol from free agent batters dataset
##and converting observations from character to numeric
FreeAgentBatters <- as.data.frame(lapply(FreeAgentBatters, 
                                         function(y) gsub("%", "", y)))

FreeAgentBatters[,-c(2:3)] <- lapply(FreeAgentBatters[,-c(2:3)], 
                                     function(x) as.numeric(as.character(x)))


##Create subset of data to include variables that affect the team level
##deficiencies in power
PowerBatterStats <- FreeAgentBatters[ , c(1:2,10:11,20)] 
BatterStats2022 <- subset(PowerBatterStats, PowerBatterStats$Season=="2022")


##z-score scale the data (only columns 2 through 4)
##this also excludes the City variable from analysis
ScaledBatterStats <- scale(BatterStats2022[ , 3:5])


##similarity measures
##can alternatively use "manhattan" or "binary" (for Jaccard's)
BatterEuclidean <- dist(ScaledBatterStats, method = "euclidean")


##utilize the agnes function to perform agglomerative clustering
BatterAgnes <- agnes(BatterEuclidean, diss = TRUE, method = "ward")

##print the result
BatterAgnes

##produce the banner plot and dendrogram
plot(BatterAgnes)

##now we can cut the tree
##where 'k' is set to the number of desired clusters
BatterTree <- cutree(BatterAgnes, k = 8)

##append the result to the original data frame
BatterClusters <- data.frame(BatterStats2022, BatterTree)
View(BatterClusters)

##Obtain summary statistics for each cluster
summary(subset(BatterClusters, BatterTree == 1))
summary(subset(BatterClusters, BatterTree == 2))
summary(subset(BatterClusters, BatterTree == 3))
summary(subset(BatterClusters, BatterTree == 4))
summary(subset(BatterClusters, BatterTree == 5))
summary(subset(BatterClusters, BatterTree == 6))
summary(subset(BatterClusters, BatterTree == 7))
summary(subset(BatterClusters, BatterTree == 8))


##Identify the number of observations in each cluster type
summary(as.factor(BatterTree)) 

##Creating subset of players in cluster 1 who are best power options for 
##teams based on mean values
BestPowerOptions <- subset(BatterClusters,BatterClusters$BatterTree == "3")


##Create subset of data that includes variables that highlight getting on
##base at a high level and producing once there
SmallBallBatterStats <- FreeAgentBatters[ , c(1:2,12,14:16,18:19)]
SmallBallStats2022 <- subset(SmallBallBatterStats, 
                             SmallBallBatterStats$Season=="2022")


##z-score scale the data (only columns 2 through 4)
##this also excludes the City variable from analysis
ScaledSBStats <- scale(SmallBallStats2022[, 3:8])


##similarity measures
##can alternatively use "manhattan" or "binary" (for Jaccard's)
SBEuclidean <- dist(ScaledSBStats, method = "euclidean")


##utilize the agnes function to perform agglomerative clustering
##can alternatively use "single", "complete", "average"
##'diss' (e.g. dissimilarity) set to TRUE facilitates distance matrix
##rather than the original variables
SBAgnes <- agnes(SBEuclidean, diss = TRUE, method = "ward")
##print the result
SBAgnes

##produce the banner plot and dendrogram
plot(SBAgnes)


##Cut the tree where 'k' is set to the number of desired clusters
SBTree <- cutree(SBAgnes, k = 6)

##append the result to the original data frame
SBClusters <- data.frame(SmallBallStats2022, SBTree)
View(SBClusters)

##Obtain summary statistics for each cluster
summary(subset(SBClusters, SBTree == 1))
summary(subset(SBClusters, SBTree == 2))
summary(subset(SBClusters, SBTree == 3))
summary(subset(SBClusters, SBTree == 4))
summary(subset(SBClusters, SBTree == 5))
summary(subset(SBClusters, SBTree == 6))

##Identify the number of observations in each cluster type
summary(as.factor(SBTree)) 

##Creating subset of players in cluster 1 who are best power options for 
##teams based on mean values
BestSBOptions <- subset(SBClusters,SBClusters$SBTree == "1")



############### Batter Forecasting ###############
############### Batter Forecasting ###############
############### Batter Forecasting ###############


BattersLast5 <- subset(PowerBatterStats, PowerBatterStats$Season %in% 
                         c("2018","2019","2020","2021","2022"))

PowerOptionLast5 <- BattersLast5[BattersLast5$Name %in% 
                                   c(BestPowerOptions$Name), ]


##Creating time series plots for last 5 years for players who were 
##determined to be best power options based on 2023 numbers

Pederson <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Joc Pederson")

PedersonSLG.ts <- ts(Pederson$SLG,start = 2018)

plot(PedersonSLG.ts, xlab = "Year", ylab = "SLG",
     main = "Joc Pederson Slugging by Year")


##change the scale on the y-axis to 
##log-scale for exploratory purposes
plot(PedersonSLG.ts, xlab = "Year", ylab = "SLG", log = 'xy',
     main = "Time plot of log of Yearly SLG for Joc Pederson")

nValid <- 4
nTrain <- length(PedersonSLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)



Correa <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Carlos Correa")

CorreaSLG.ts <- ts(Correa$SLG,start = 2018)

plot(CorreaSLG.ts, xlab = "Year", ylab = "SLG",
     main = "Carlos Correa Slugging by Year")

nValid <- 4
nTrain <- length(CorreaSLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)


Contreras <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Wilson Contreras")

ContrerasSLG.ts <- ts(Contreras$SLG,start = 2018)

plot(ContrerasSLG.ts, xlab = "Year", ylab = "SLG",
     main = "Wilson Contreras Slugging by Year")

nValid <- 4
nTrain <- length(ContrerasSLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)



Rizzo <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Anthony Rizzo")

RizzoSLG.ts <- ts(Rizzo$SLG,start = 2018)

plot(RizzoSLG.ts, xlab = "Year", ylab = "SLG",
     main = "Anthony Rizzo Slugging by Year")

nValid <- 4
nTrain <- length(RizzaSLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)


Drury <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Brandon Drury")

DrurySLG.ts <- ts(Drury$SLG,start = 2018)

plot(DrurySLG.ts, xlab = "Year", ylab = "SLG",
     main = "Brandon Drury Slugging by Year")

nValid <- 4
nTrain <- length(DrurySLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)



Turner <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Trea Turner")

TurnerSLG.ts <- ts(Turner$SLG,start = 2018)

plot(TurnerSLG.ts, xlab = "Year", ylab = "SLG",
     main = "Trea Turner Slugging by Year")

nValid <- 4
nTrain <- length(TurnerSLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)


Swanson <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Dansby Swanson")

SwansonSLG.ts <- ts(Swanson$SLG,start = 2018)

plot(SwansonSLG.ts, xlab = "Year", ylab = "SLG",
     main = "Dansby Swanson Slugging by Year")


nValid <- 4
nTrain <- length(SwansonSLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)


Voit <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Luke Voit")

VoitSLG.ts <- ts(Voit$SLG,start = 2018)

plot(VoitSLG.ts, xlab = "Year", ylab = "SLG",
     main = "Luke Voit Slugging by Year")
nValid <- 4
nTrain <- length(VoitSLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)


Gallo <- subset(PowerOptionLast5, PowerOptionLast5$Name == "Joey Gallo")

GalloSLG.ts <- ts(Gallo$SLG,start = 2018)

plot(GalloSLG.ts, xlab = "Year", ylab = "SLG",
     main = "Joey Gallo Slugging by Year")

nValid <- 4
nTrain <- length(GalloSLG.ts) - nValid

train.ts <- window(PedersonSLG.ts, start = 2018, end = 2021)
valid.ts <- window(PedersonSLG.ts, start = 2021)

naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)
summary(naive.pred)
summary(snaive.pred)



############### Data Visualizations ###############
############### Data Visualizations ###############
############### Data Visualizations ###############


FreeAgentsPerTeam <- read_excel("Free Agents By Team.xlsx")

ggplot(data = FreeAgentsPerTeam, aes(x = reorder(Team, Free_Agents),
                                     y = Free_Agents)) + 
  labs(title = "Number of Free Agents Signed per Team", x = "Team",
       y = "Free Agents") +
  geom_bar(stat = "identity", width=.9, fill="lightblue")






