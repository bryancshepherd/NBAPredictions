#install.packages("rvest")
#install.packages("BradleyTerry2")
#install.packages("doBy")

# TODO: Set up to facilitate predicting for new data
# TODO: Get historical moneyline data
# TODO: Calculate winnings/losing using historical moneyline data

library("BradleyTerry2")
library("doBy")
library("rvest")
library("ggplot2")

### Define functions
# Estimate a B-T model
estimateModel <- function(resultsData) {
  homeWonSummary <- summaryBy(homeTeamWon~homeTeamName + awayTeamName, data = resultsData, FUN = sum)
  awayWonSummary <- summaryBy(awayTeamWon~homeTeamName + awayTeamName, data = resultsData, FUN = sum)
  
  resultsSummaryTable <- merge(homeWonSummary,awayWonSummary)
  
  # BTm expects factors for the team names
  resultsSummaryTable$homeTeamName <- as.factor(resultsSummaryTable$homeTeamName)
  resultsSummaryTable$awayTeamName <- as.factor(resultsSummaryTable$awayTeamName)
  
  levels(resultsSummaryTable$homeTeamName)
  levels(resultsSummaryTable$awayTeamName)
  
  bbModel <- BTm(cbind(homeTeamWon.sum, awayTeamWon.sum), homeTeamName, awayTeamName, ~ team, 
                 id = "team", data = resultsSummaryTable)
  
  return(list(bbModel, resultsSummaryTable))
}

# Predict future games based on an estimated B-T model
predictBT <- function(model, upcomingGames, modelData) {
  
  resultsSummaryTable = modelData
  
  awayTeams = upcomingGames$awayTeamName
  
  homeTeams = upcomingGames$homeTeamName

  for (i in 1:nrow(upcomingGames)) {
    
    model = update(model, refcat = homeTeams[i])
    modSum = summary(model)
    modSumDF = as.data.frame(modSum$coefficients)
    modSumDFSubset = modSumDF[row.names(modSumDF) %in% paste0("team", awayTeams[i]),]
    
    if (exists("prediction")) {
      tempPrediction = cbind(modSumDFSubset, upcomingGames$newDate[i])
      tempPrediction$predWinner = ifelse(modSumDFSubset$Estimate > 0, awayTeams[i], homeTeams[i])
      tempPrediction$predWinnerHome = ifelse(modSumDFSubset$Estimate > 0, 0, 1)
      
      prediction = rbind(prediction, tempPrediction)
    } else {
      prediction = cbind(modSumDFSubset, upcomingGames$newDate[i])
      prediction$predWinner = ifelse(modSumDFSubset$Estimate > 0, awayTeams[i], homeTeams[i])
      prediction$predWinnerHome = ifelse(modSumDFSubset$Estimate > 0, 0, 1)
    }
    
    # print(paste0("Prediction: ", 
    #             ifelse(prediction$Estimate > 0, awayTeams[i], homeTeams[i]), " win. ", "P-value = ", round(prediction[4], 3)))
  }
  
  upcomingGamesWithPredictions = cbind(upcomingGames, prediction)
  
  return(upcomingGamesWithPredictions)
  
}

# Function to calculate summary statistics
createSummaryStats <- function (predictions, pvalue = .05) {
  
  tempStats = {}
  
  strongPredictions <-  predictions[predictions$`Pr(>|z|)` <= pvalue, ]
  
  tempStats[["totalPlays"]] = nrow(strongPredictions)
  tempStats[["totalCorrect"]] = sum(strongPredictions$homeTeamWon == strongPredictions$predWinnerHome)
  tempStats[["totalIncorrect"]] = sum(strongPredictions$homeTeamWon != strongPredictions$predWinnerHome)
  tempStats[["percentCorrect"]] = ifelse(tempStats[["totalPlays"]] > 0, tempStats[["totalCorrect"]]/tempStats[["totalPlays"]], 0)
  
  return(tempStats)
  
}

# Function to calculate returns
calculateReturns <- function (predictions, pvalue = .05, startingBalance = 1000, betSize = 100) {
  
  tempStats = {}
  
  strongPredictions <-  predictions[predictions$`Pr(>|z|)` <= pvalue, ]
  
  strongPredictions$gameReturn <- ifelse(strongPredictions$predWinnerHome, 
                                         ifelse(strongPredictions$homeTeamWon, 
                                                ifelse(strongPredictions$homeOdds > 0, strongPredictions$homeOdds, betSize),
                                                ifelse(strongPredictions$homeOdds > 0, betSize, strongPredictions$homeOdds)),
                                         ifelse(strongPredictions$awayTeamWon, 
                                                ifelse(strongPredictions$awayOdds > 0, strongPredictions$awayOdds, betSize),
                                                ifelse(strongPredictions$awayOdds > 0, betSize, strongPredictions$awayOdds)))
  
  strongPredictions$cumReturns <- cumsum(strongPredictions$gameReturn)
  
  return(strongPredictions)
  
}


## Eventually automate the data pull, but the site layout is not conducive to webscraping
## Since a .csv file is easy to get, let's see if there are worthwhile 
## results first
# scheduleAndResultsHTML = read_html("http://www.basketball-reference.com/leagues/NBA_2016_games.html?lid=standings_sked") 
# scheduleAndResults = scheduleAndResultsHTML %>% html_nodes("tr")

bballData <- read.csv("../Data/scheduleAndResults02062016.csv", stringsAsFactors = FALSE, header = TRUE)

## Clean up data
# Better colnames 
names(bballData) <-  c("date", "time", "entryType", 
                       "awayTeamName", "awayTeamPoints", 
                       "homeTeamName", "homeTeamPoints", 
                       "overtime", "notes")

bballData$newDate <- as.Date(substr(bballData$date, 5, 15), format="%b %d %Y")

bballData$homeTeamWon <- ifelse(bballData$homeTeamPoints > bballData$awayTeamPoints, 1, 0)
bballData$awayTeamWon <- ifelse(bballData$homeTeamPoints < bballData$awayTeamPoints, 1, 0)

## Bring in odds data
oddsData <- read.csv("../Data/HistoricalOdds_cleaned.csv", stringsAsFactors = FALSE, header = TRUE)
oddsData$newDate <- as.Date(oddsData$Date, format="%m/%d/%Y")

tempMerged <- merge(bballData, oddsData, by = c("newDate", "awayTeamName", "homeTeamName"), all.x = TRUE)

bballData <- tempMerged

## Split data for estimation and prediction
# Get number of previous games so that we know when to stop 
numberOfPreviousGames <- sum(!is.na(bballData$homeTeamPoints))
lastGameday <- bballData[numberOfPreviousGames, "newDate"]

# All of the game day dates (past and future)
gameDays <- unique(bballData$newDate)

# Only previous game days
previousGameDays <- gameDays[gameDays<=lastGameday]

percentDailyCorrect = percentCorrect = vector("numeric", length(previousGameDays))
totalDailyIncorrect = totalIncorrect = vector("numeric", length(previousGameDays))
totalDailyCorrect = totalCorrect = vector("numeric", length(previousGameDays))
totalDailyPlays = totalPlays = vector("numeric", length(previousGameDays))


for (i in 1:length(previousGameDays)) {
  if (i>15 & gameDays[i]<=lastGameday) {
    prevResults <- bballData[bballData$newDate < gameDays[i], ]
    
    toPredict <- bballData[bballData$newDate == gameDays[i], ]
    
    tempModelRes <- estimateModel(prevResults)
    
    if (exists("allPredictions")) {
      tempPredictions = predictBT(tempModelRes[[1]], toPredict, tempModelRes[[2]])
      
      summaryStats = createSummaryStats(tempPredictions)
      totalDailyPlays[i] = summaryStats[["totalPlays"]]
      totalDailyCorrect[i] = summaryStats[["totalCorrect"]]
      totalDailyIncorrect[i] = summaryStats[["totalIncorrect"]]
      percentDailyCorrect[i] = summaryStats[["percentCorrect"]]
      
      allPredictions = rbind(allPredictions, tempPredictions)
      
    } else {
      allPredictions = predictBT(tempModelRes[[1]], toPredict, tempModelRes[[2]])
      
      summaryStats = createSummaryStats(allPredictions)
      totalDailyPlays[i] = summaryStats[["totalPlays"]]
      totalDailyCorrect[i] = summaryStats[["totalCorrect"]]
      totalDailyIncorrect[i] = summaryStats[["totalIncorrect"]]
      percentDailyCorrect[i] = summaryStats[["percentCorrect"]]
    }
    
    summaryStats = createSummaryStats(allPredictions)
    totalPlays[i] = summaryStats[["totalPlays"]]
    totalCorrect[i] = summaryStats[["totalCorrect"]]
    totalIncorrect[i] = summaryStats[["totalIncorrect"]]
    percentCorrect[i] = summaryStats[["percentCorrect"]]
    
  }
}

returns <- calculateReturns(allPredictions)

# Check calculations
head(returns[returns$predWinnerHome == 1 & returns$homeTeamWon == 1 & returns$homeOdds > 0, ], 5)
head(returns[returns$predWinnerHome == 1 & returns$homeTeamWon == 1 & returns$homeOdds < 0, ], 5)
head(returns[returns$predWinnerHome == 1 & returns$homeTeamWon == 0 & returns$homeOdds > 0, ], 5)
head(returns[returns$predWinnerHome == 1 & returns$homeTeamWon == 0 & returns$homeOdds < 0, ], 5)

head(returns[returns$predWinnerHome == 0 & returns$homeTeamWon == 0 & returns$homeOdds > 0, ], 5)
head(returns[returns$predWinnerHome == 0 & returns$homeTeamWon == 0 & returns$homeOdds < 0, ], 5)
head(returns[returns$predWinnerHome == 0 & returns$homeTeamWon == 1 & returns$homeOdds > 0, ], 5)
head(returns[returns$predWinnerHome == 0 & returns$homeTeamWon == 1 & returns$homeOdds < 0, ], 5)

head(returns[returns$predWinnerHome == 1 & returns$awayOdds > 0, ], 5)
head(returns[returns$predWinnerHome == 0 & returns$homeOdds > 0, ], 5)

# What is the distribution of variables for strong predictions
summary(returns[returns$predWinnerHome == 1, ])
summary(returns[returns$predWinnerHome == 0, ])

# What if we only bet on underdogs that are predicted to win?
# Given that there aren't any home teams that are underdogs and predicted to win, it
# seems like the odds makers are giving assuming a home team advantage when creating odds
returns[returns$predWinnerHome == 1 & returns$homeOdds > 0, ]
returns[returns$predWinnerHome == 0 & returns$awayOdds > 0, ]

# Combine data for easier ggplotting
cumulativeResultsByDay = as.data.frame(cbind(previousGameDays, percentCorrect))

ggplot(data = cumulativeResultsByDay, 
       aes(x=previousGameDays, y = percentCorrect)) +
  geom_line()

plot(previousGameDays, totalPlays)
plot(previousGameDays, totalIncorrect)
plot(previousGameDays, totalCorrect)
plot(previousGameDays, percentCorrect)

lm(totalCorrect ~ 0 + previousGameDays)
lm(totalIncorrect ~ 0 + previousGameDays)

cbind(totalCorrect, totalIncorrect, totalPlays, percentCorrect)

# Combine data for easier ggplotting
cumulativeDailySummaryResultsByDay = as.data.frame(cbind(previousGameDays, percentDailyCorrect))

ggplot(data = cumulativeDailySummaryResultsByDay, 
       aes(x=previousGameDays, y = percentDailyCorrect)) +
  geom_line()

plot(previousGameDays, totalDailyPlays)
plot(previousGameDays, totalDailyIncorrect)
plot(previousGameDays, totalDailyCorrect)
plot(previousGameDays, percentDailyCorrect)

summary(lm(totalDailyCorrect ~ 0 + previousGameDays))
summary(lm(totalDailyIncorrect ~ 0 + previousGameDays))
summary(lm(totalDailyPlays ~ 0 + previousGameDays))
summary(lm(percentDailyCorrect ~ 0 + previousGameDays))
