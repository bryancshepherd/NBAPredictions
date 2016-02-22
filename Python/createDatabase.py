import sqlite3
from datetime import date, datetime, timedelta
import time
import random
import sys
import os.path

db_loc = 'C:/GitHub/NBAPredictions/Data/NBAData.db'

def allTablesAreEmpty(sqldb_file, close_when_done = True):
    
    # Make the connection
    conn = sqlite3.connect(sqldb_file)
    c = conn.cursor()
    
    # Get the list of tables
    c.execute('''SELECT name FROM sqlite_master WHERE type = "table"''')

    all_tables = c.fetchall()
    
    # See if all tables in the db have 0 rows
    count = []
    for table in all_tables:
        c.execute('SELECT COUNT(*) FROM {tn}'.format(tn=table[0]))
        n_rows = c.fetchall()
        count.append(n_rows[0][0])
    
    if close_when_done == True: conn.close()
    
    is_empty = True if sum(count) == 0 else False
    return is_empty


def dbHasNoTables(sqldb_file, close_when_done = True):
    
    # Make the connection
    conn = sqlite3.connect(sqldb_file)
    c = conn.cursor()
    
    # Get the list of tables
    c.execute('''SELECT name FROM sqlite_master WHERE type = "table"''')

    all_tables = c.fetchall()

    if close_when_done == True: conn.close()

    no_tables = True if len(all_tables) == 0 else False
    return no_tables



def addTables(db_file):
    conn = sqlite3.connect(db_file)
    c = conn.cursor()
                
    # Now add tables to database
    c.execute('''CREATE TABLE schedule (game_id text, home_team text, away_team text, game_date text, game_time text)''')
    c.execute('''CREATE TABLE results (game_id text, entry_type text, home_score int, away_score int, overtime_indicator int, notes text)''')
    c.execute('''CREATE TABLE odds (game_id text, home_team text, away_team text, home_team_odds real, away_team_odds real)''')
                
    conn.commit()
    conn.close()
    
    print('Tables created successfully')

def dbSetup(file_loc):
    
    # Does the database already exist?
    if os.path.exists(file_loc):
        # Is there already a connection?
        try:
            con
            print("A connection already exists, please close it before attemping to rebuild the database.")
            
        # If there isn't already an open connection, go ahead with setting up the table
        except:
            
            if dbHasNoTables(file_loc):
                
                # If the db exists but has no tables, create them
                addTables(file_loc)
                
            elif allTablesAreEmpty(file_loc):
                print('Database already exists and all tables are empty')

    else:
        # If the file doesn't exist, make the connection and implicitly create the database and add tables
        addTables(file_loc)

dbSetup(db_loc)
