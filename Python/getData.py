from bs4 import BeautifulSoup
import requests

r = requests.get("http://www.basketball-reference.com/leagues/NBA_2016_games.html?lid=standings_sked")
r.status_code

r.headers['content-type']

r.encoding

r.text

soup = BeautifulSoup(r.text, "lxml")

# print(soup.prettify())


total_iters = 0
for row in soup.find_all('tr'):
    num_of_tds = 0
    for td in row.find_all('td'):
        print(td)
        
        total_iters = total_iters+1
        num_of_tds = num_of_tds + 1


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




