# -*- coding: utf-8 -*-
"""
Created on Sun Sep 18 16:32:29 2022

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
def repeat(s, n):
    """
    Takes a string and returns a list with the string repeated n times.

    Parameters
    ----------
    s : str
        A string to repeat.
    n : int
        number of times the integer shall be repeated.

    Returns
    -------
    list.

    """
    l = []
    for i in range(n):
        l.append(s)        
    return(l)

#%% Connect to database
engine = create_engine('postgresql+psycopg2://patrick:' + pwd + '@lichthaus.ddns.net:54320/lichthaus')    

#%% Get all existing .json files
existingJson = os.listdir(targetFolder)
existingJson = [f.replace(".json", "") for f in existingJson]

#%% Gather missing json if necessary
moviesWithoutJson = set(pd.read_sql_table("film", engine)["id"].tolist()) - set(existingJson)
saveMovieInfo(moviesWithoutJson, apiKey, targetFolder)

#%% Select the movie ids where the is no genre information in database yet
q = """SELECT DISTINCT film.id
       FROM film
       WHERE NOT EXISTS(
           SELECT
	       FROM gehoert_zu
	       WHERE gehoert_zu.id = film.id
       );
 """
 
moviesWithoutGenre = pd.read_sql_query(q, engine)["id"].tolist()

#%% Collect genre information for each movie without genre from json
ID = []
bezeichnung = []
for i in moviesWithoutGenre:
    #print("#######################")
    #print(i)
    try: 
        f = open(targetFolder + "\\" + i + ".json", "r", encoding = "charmap")
        g = json.load(f)["genres"]
        ID.extend(repeat(i, len(g)))
        bezeichnung.extend(g)
    except:
        print("Error for: " + i)
        continue     
 

#%% Extract the invidual genres and upload the new ones to database
newGenres = set(bezeichnung) - set(pd.read_sql_table("genre", engine)["bezeichnung"].tolist())
genre = pd.DataFrame(set(newGenres), columns = ["bezeichnung"])
genre['upload_time'] = str(datetime.now())
genre.to_sql("genre", engine, if_exists="append", index = False)

#%% Combine each new id with all its genres and upload the result to database
gehoert_zu = pd.DataFrame(list(zip(ID, bezeichnung)),
                  columns = ["id", "bezeichnung"])

gehoert_zu['upload_time'] = str(datetime.now())
gehoert_zu.to_sql("gehoert_zu", engine, if_exists="append", index = False)
