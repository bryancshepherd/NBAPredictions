from bs4 import BeautifulSoup
import requests

r = requests.get("http://www.basketball-reference.com/leagues/NBA_2016_games.html?lid=standings_sked")

# Check results
r.status_code
r.headers['content-type']
r.encoding

soup = BeautifulSoup(r.text, "lxml")

# Create lists from table columns that will eventually be combined into a data set
game_date = []
game_time = []
entry_type = []
away_team = []
away_score = []
home_team = []
home_score = []
ot_indicator = []
notes = []

for row in soup.find_all('tr'):
    row_data = row.find_all('td')
    if (len(row_data) > 0):
        game_date.append(row_data[0].string)
        game_time.append(row_data[1].string)
        entry_type.append(row_data[2].string)
        away_team.append(row_data[3].string)
        away_score.append(row_data[4].string)
        home_team.append(row_data[5].string)
        home_score.append(row_data[6].string)
        ot_indicator.append(row_data[7].string)
        notes.append(row_data[8].string)




