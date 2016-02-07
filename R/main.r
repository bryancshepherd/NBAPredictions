#install.packages("rvest")
#install.packages("BradleyTerry2")
#install.packages("doBy")

# TODO: Need to add percent correct, etc. by day (not just cumulative)
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
  if (i>15 & gameDays[i+1]<=lastGameday) {
    prevResults <- bballData[bballData$newDate <= gameDays[i], ]
    
    toPredict <- bballData[bballData$newDate > gameDays[i] & bballData$newDate <= gameDays[i+1], ]
    
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
