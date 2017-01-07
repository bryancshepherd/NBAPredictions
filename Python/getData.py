import os
import sqlite3
import datetime
os.chdir('C:/Users/bryan/Documents/Projects/NBAPredictions/Python/')

os.getcwd()

from bs4 import BeautifulSoup
import databaseHelperFunctions as dbh
import requests

db_loc = 'C:/Users/bryan/Documents/Projects/NBAPredictions/Data/NBAData.db'

dbh.dbSetup(db_loc)

r = requests.get("http://www.basketball-reference.com/leagues/NBA_2016_games-october.html")

## Check results
#```{python}
#r.status_code
#r.headers['content-type']
#r.encoding
#```

soup = BeautifulSoup(r.text, "lxml")

# Create lists from table columns that will eventually be combined into a data set
results_values = []
for row in soup.find_all('tr'):
    row_data = row.find_all('td')
    if (len(row_data) > 0):
        game_date = row.find_all('th')[0].string
        results_values.append((game_date, row_data[0].string, row_data[1].string, \
                               row_data[2].string, row_data[3].string, \
                               row_data[4].string, row_data[5].string, \
                               row_data[6].string, row_data[7].string))

# Make the connection
conn = sqlite3.connect('../Data/NBAData.db')
c = conn.cursor()
    
# Insert the values
c.executemany('INSERT INTO schedule VALUES (?,?,?,?,?,?,?,?,?)', results_values)
conn.commit()
conn.close()

# Check that values were added correctly
dbh.allTablesAreEmpty(db_loc)

conn = sqlite3.connect(db_loc)
c = conn.cursor()
    
# Get the list of tables
c.execute('SELECT name FROM sqlite_master WHERE type = "table"')

all_tables = c.fetchall()
conn.close()

############# Get odds data
headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36'
}


#### Not sure what's going on with these functions...
#### Get list of days to iterate through
def getMostRecentOddsDate(db_loc):
    conn = sqlite3.connect(db_loc)
    c = conn.cursor()
    
    # Get the list of tables
    c.execute('SELECT max(game_date) FROM odds')

    most_recent_odds = c.fetchall()
    conn.close()
    return most_recent_odds 
    
most_recent_odds_date = daysToUpdate(db_loc) 

def getListOfDaysToUpdate(db_loc, last_odds_date):
    
    conn = sqlite3.connect(db_loc)
    c = conn.cursor()
    
    # Get the list of tables
    c.execute('SELECT game_date FROM schedule')

    all_game_days = c.fetchall()
    conn.close()
    return most_recent_odds
    
conn = sqlite3.connect(db_loc)
c = conn.cursor()
    
# Get the list of tables
c.execute('SELECT game_date FROM schedule')

all_game_days = c.fetchall()
conn.close()

### Convert schedule dates to proper format

### Need to keep only unique game days for the code below
game_date = all_game_days[0][0]
game_day_dateformat = datetime.datetime.strptime(game_date[5: 17], "%b %d, %Y")
string_date = game_day_dateformat.strftime("%Y%m%d")
## Will need to use the schedule to iterate through this by day
url = "http://www.scoresandodds.com/grid_"+string_date+".html"

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
    away_team_name_odds.append(column_data[0].string[4:])
    away_team_moneyline_odds.append(column_data[4].string)


# Get team name and moneyline data from table columns
# Home team
home_team_list = nba_div.find_all("tr", "team even")
home_team_name_odds = []
home_team_moneyline_odds = []

for team in home_team_list:
    column_data = team.find_all("td")
    home_team_name_odds.append(column_data[0].string[4:])
    home_team_moneyline_odds.append(column_data[4].string)


results_values = []
if len(away_team_name_odds) > 0:
    for i in range(0, len(away_team_name_odds)):
        results_values.append((game_date, away_team_name_odds[i], home_team_name_odds[i],  \
                                away_team_moneyline_odds[i], home_team_moneyline_odds[i]))
                               

# Make the connection
conn = sqlite3.connect('../Data/NBAData.db')
c = conn.cursor()
    
# Insert the values
c.executemany('INSERT INTO odds VALUES (?,?,?,?,?)', results_values)
conn.commit()
conn.close()


# Make the connection
conn = sqlite3.connect('../Data/NBAData.db')
c = conn.cursor()
    
# Insert the values
c.execute('SELECT * FROM odds')
all_rows = c.fetchall()
conn.commit()
conn.close()

