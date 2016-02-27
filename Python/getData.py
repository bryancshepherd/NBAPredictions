import os
import sqlite3
os.chdir('C:/GitHub/NBAPredictions/Python/')

os.getcwd()

from bs4 import BeautifulSoup
import databaseHelperFunctions as dbh
import requests

db_loc = 'C:/GitHub/NBAPredictions/Data/NBAData.db'

dbh.dbSetup(db_loc)

r = requests.get("http://www.basketball-reference.com/leagues/NBA_2016_games.html?lid=standings_sked")

# Check results
```{python}
r.status_code
r.headers['content-type']
r.encoding
```

soup = BeautifulSoup(r.text, "lxml")

# Create lists from table columns that will eventually be combined into a data set

results_values = []

for row in soup.find_all('tr'):
    row_data = row.find_all('td')
    if (len(row_data) > 0):
        results_values.append((row_data[0].string, row_data[1].string, row_data[2].string, row_data[3].string, \
                               row_data[4].string, row_data[5].string, row_data[6].string, row_data[7].string, \
                               row_data[8].string))

# Make the connection
conn = sqlite3.connect('../Data/NBAData.db')
c = conn.cursor()
    
# Insert the values
c.executemany('INSERT INTO schedule VALUES (?,?,?,?,?,?,?,?,?)', results_values)
conn.commit()
conn.close()


#### Get odds data
headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36'
}

## Will need to use the schedule to iterate through this by day
url = "http://www.scoresandodds.com/grid_20151027.html"

r = requests.get(url, headers=headers)

# Check results
r.status_code
r.headers['content-type']
r.encoding

soup = BeautifulSoup(r.text, "lxml")

# Still need to pull numbers from the table the code below creates
nba_div = soup.find("div", id = "nba").next_sibling.table

# Get team name and moneyline data from table columns
# Away team
away_team_list = nba_div.find_all("tr", "team odd")
away_team_name_odds = []
away_team_moneyline_odds = []

for team in away_team_list:
    column_data = team.find_all("td")
    away_team_name_odds.append(column_data[0].string)
    away_team_moneyline_odds.append(column_data[4].string)


# Get team name and moneyline data from table columns
# Home team
home_team_list = nba_div.find_all("tr", "team odd")
home_team_name_odds = []
home_team_moneyline_odds = []

for team in home_team_list:
    column_data = team.find_all("td")
    home_team_name_odds.append(column_data[0].string)
    home_team_moneyline_odds.append(column_data[4].string)


print(nba_div.prettify())

