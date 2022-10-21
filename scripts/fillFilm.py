# -*- coding: utf-8 -*-
"""
Created on Wed Oct 12 12:23:37 2022

@author: Patrick
"""

import os
import pandas as pd
import json
from sqlalchemy import create_engine
from datetime import datetime
from getMovieDetailInfo import saveMovieInfo

#%% Initialize
targetFolder = "../data/json/"
pwd = open("../secrets.txt", "r").readlines()[1]
apiKey = open("../secrets.txt", "r").readlines()[0].replace("\n", "")

def extractRuntime(x):
    t = x.split("h")
    if "h" in x:   # Duration is longer than 1 hour     
        h = int(t[0])
        if "m" in t[1]:
            m = int(t[1].replace("m", "").replace(" ", ""))
        else: # Duration is exact multiple of 1 hour    
            m = 0
    else: # Duration is shorter than 1 hour
        h = 0
        m = int(t[0].replace("m", "").replace(" ", ""))        
    return(h * 60 + m)


#%% Connect to database
engine = create_engine('postgresql+psycopg2://patrick:' + pwd + '@lichthaus.ddns.net:54320/lichthaus')    

#%% Get all existing .json files, read genre info from them and combine them with the ids
existingJson = os.listdir(targetFolder)
existingJson = [f.replace(".json", "") for f in existingJson]


#%% Gather missing json if necessary
moviesWithoutJson = set(pd.read_sql_table("film", engine)["id"].tolist()) - set(existingJson)
saveMovieInfo(moviesWithoutJson, apiKey, targetFolder)

#%% Select the rows from film with missing data
q = """SELECT *
       FROM film
       WHERE titel IS NULL
       OR erscheinungsjahr IS NULL
       OR imdb_rating IS NULL
       OR laufzeit IS NULL
       OR image_link IS NULL;
 """
 
incompleteMovies = pd.read_sql_query(q, engine)["id"].tolist()

#%% Collect movie information for each movie with missing data
imdb_rating = []
laufzeit = []
image_link = []
titel = []
erscheinungsjahr = []
ID = []
for i in incompleteMovies:
    #print("#######################")
    #print(i)
   try: 
        with open(targetFolder + "\\" + i + ".json", "r", encoding = "charmap") as f:
            dct = json.load(f)   
            if all(k in dct for k in ("rating", "runtime", "image", "title", "release_year")):
                ID.append(i)
                imdb_rating.append(dct["rating"])
                laufzeit.append(extractRuntime(dct["runtime"]))            
                image_link.append(dct["image"])
                titel.append(dct["title"])
                erscheinungsjahr.append(dct["release_year"])
            else:
                print("Missing information for " + i)
   except:
        print("Error for: " + i)
        continue   


#%% Update the film table
df = pd.DataFrame(list(zip(ID, titel, erscheinungsjahr, imdb_rating, laufzeit, image_link)),
                  columns = ["id", "titel", "erscheinungsjahr", "imdb_rating", "laufzeit", "image_link"])

df["upload_time"] = datetime.now()

df.to_sql('tmp', engine, if_exists='replace')

q = """
    UPDATE film
    SET titel = tmp.titel,
        erscheinungsjahr = tmp.erscheinungsjahr,
        imdb_rating = tmp.imdb_rating,
        laufzeit = tmp.laufzeit,
        image_link = tmp.image_link,
        upload_time = tmp.upload_time
    FROM tmp
    WHERE film.id = tmp.id
"""
with engine.begin() as conn:     # TRANSACTION
    conn.execute(q)

