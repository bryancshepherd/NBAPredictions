
### Define functions for NBA predictions
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
