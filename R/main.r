install.packages("rvest")
install.packages("BradleyTerry2")
install.packages("doBy")

library("BradleyTerry2")
library("doBy")
library("rvest")

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
  
  bbModel <- BTm(cbind(homeTeamWon.sum, awayTeamWon.sum), homeTeamName, awayTeamName, ~ team, id = "team", data = resultsSummaryTable)
}

# Predict future games based on an estimated B-T model
predictBT <- function(model, upcomingGames) {
  update(bbModel, refcat = "Golden State Warriors")
  
  awayTeams = upcomingGames$awayTeamName
  
  homeTeams = upcomingGames$homeTeamName

  for (i in 1:nrow(upcomingGames)) {
    
    print(paste0("Displaying game: ", homeTeams[i], " vs. ", awayTeams[i],
                 "  *******************************",
                 "*******************************"))
    bbModel = update(bbModel, refcat = homeTeams[i])
    modSum = summary(bbModel)
    modSumDF = as.data.frame(modSum$coefficients)
    prediction = modSumDF[row.names(modSumDF) %in% paste0("team", awayTeams[i]),]
    print("")
    print(paste0("Prediction: ", 
                 ifelse(prediction$Estimate > 0, awayTeams[i], homeTeams[i]), " win. ", "P-value = ", round(prediction[4], 3)))
    print("")
    print(prediction)
    print("")
    print("")
    print("")
  }
}




## Eventually automate the data pull, but the site layout is not conducive to webscraping
## Since a .csv file is easy to get, let's see if there are worthwhile 
## results first
# scheduleAndResultsHTML = read_html("http://www.basketball-reference.com/leagues/NBA_2016_games.html?lid=standings_sked") 
# scheduleAndResults = scheduleAndResultsHTML %>% html_nodes("tr")

bballData <- read.csv("../Data/scheduleAndResults01312016.csv", stringsAsFactors = FALSE, header = TRUE)

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

for (i in 1:length(previousGameDays)) {
  if (gameDays[i+1]<=lastGameday) {
    prevResults <- bballData[bballData$newDate <= gameDays[i], ]
    
    toPredict <- bballData[bballData$newDate > gameDays[i] & bballData$newDate <= gameDays[i+1], ]
    
    tempModel <- estimateModel(prevResults)
    
    predictBT(tempModel, toPredict)
  }
}
















attributes(modSum)

bbModel$model

as.data.frame(bbModel)
# 
# data("citations", package = "BradleyTerry2")
# citations
# citations.sf <- countsToBinomial(citations)
# 
# citeModel <- BTm(cbind(win1, win2), player1, player2, ~ journal, id = "journal", data = citations.sf)
# citeModel
# 
# head(cbind(bballData$homeTeam, as.factor(bballData$homeTeam)))
# 


memory.profile()
memory.size()


?BTm

correct = 0
for (i in 1:10000) {
  choice = sample(1:2, 1)
  flip = sample(1:2, 1)
  
  if (choice == flip) {
    correct = correct + 1
  } 
    
}

correct
