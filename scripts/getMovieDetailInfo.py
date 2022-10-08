# -*- coding: utf-8 -*-
"""
Created on Sat Sep 17 18:26:31 2022

@author: Patrick
"""
import http.client
import json
import psycopg2
import time
import os

#%% Intialize
apiKey = open("secrets.txt", "r").readlines()[0].replace("\n", "")
#targetFolder = input("Choose target folder: \n")


def saveMovieInfo(id, apiKey):
    """ 
    Downloads movie info via apiKey, converts it to json and saves to
    targetFolder
    """
    
    conn = http.client.HTTPSConnection("movie-details1.p.rapidapi.com")

    headers = {
        'X-RapidAPI-Key': apiKey,
        'X-RapidAPI-Host': "movie-details1.p.rapidapi.com"
        }

    conn.request("GET", "/imdb_api/movie?id=" + id, headers=headers)

    res = conn.getresponse()
    data = res.read()

    with open("data\\json\\" + id + ".json", "w", encoding="utf-8") as f:
        f.write(data.decode("utf-8"))




    

#%% Connect to database
con = psycopg2.connect(
    host="lichthaus.ddns.net",
    port = "54320",
    database="lichthaus",
    user="donkey",
    password="axt")

#%% Get all ids in film
cur = con.cursor()
cur.execute("SELECT id FROM film")
rows = cur.fetchall()
rows = [item[0] for item in rows]


#%% Keep only the ids where there is no .json file saved for
existingIDs = os.listdir("data\\json")
existingIDs = [f.replace(".json", "") for f in existingIDs]
newIDs = list(set(rows) - set(existingIDs))


#%% Update the .json files with new ids
for id in newIDs:
    print(id)
    time.sleep(0.25)
    try:
        saveMovieInfo(id, apiKey)
    except Exception as e:
        print("Problem with: " + id)
        print(e)
    

