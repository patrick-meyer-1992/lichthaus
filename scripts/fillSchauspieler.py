# -*- coding: utf-8 -*-
"""
Created on Wed Sep 21 17:18:09 2022

@author: Patrick
"""

#%% Import packages
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

#%% Get all existing .json files, read genre info from them and combine them with the ids
existingJson = os.listdir(targetFolder)
existingJson = [f.replace(".json", "") for f in existingJson]


#%% Gather missing json if necessary
moviesWithoutJson = set(pd.read_sql_table("film", engine)["id"].tolist()) - set(existingJson)
saveMovieInfo(moviesWithoutJson, apiKey, targetFolder)

#%% Select the movie ids where the is no actor information in database yet
q = """SELECT DISTINCT film.id
       FROM film
       WHERE NOT EXISTS(
           SELECT
	       FROM spielt_mit
	       WHERE spielt_mit.film_id = film.id
       );
 """
 
moviesWithoutActor = pd.read_sql_query(q, engine)["id"].tolist()

#%% Collect actor information for each movie without actor from json
actorId = []
actorName = []
movieId = []
for i in moviesWithoutActor:
    #print("#######################")
    #print(i)
    try: 
        f = open(targetFolder + "\\" + i + ".json", "r", encoding = "charmap")
        l = json.load(f)["actors"]
        newActorIds = [d["id"] for d in l]
        newActorNames =  [d["name"] for d in l]
        actorId.extend(newActorIds)
        actorName.extend(newActorNames)
        movieId.extend(repeat(i, len(newActorIds)))
    except Exception as e:
        print("Error for: " + i)
        print(e)
        #break     
 

#%% Extract the invidual actors and upload them to database
schauspieler = pd.DataFrame(list(zip(actorId, actorName)),
                     columns = ["id", "name"])
# Some names appear more than once with the same id because of en-/decoding problems
# All the duplicate names are removed and only the first one is kept.
# Note that the first one could very well be one with faulty letters
schauspieler = schauspieler.drop_duplicates()
schauspieler["dupl"] = schauspieler["id"].duplicated()
schauspieler = schauspieler.sort_values(by = "dupl", axis = 0, ascending = False)
schauspieler = schauspieler.drop(schauspieler[schauspieler["dupl"]].index, axis = 0)
schauspieler = schauspieler.drop("dupl", axis = 1)

# Filter the ids that are already in DB
actorsInDB = pd.read_sql_table("schauspieler", engine)["id"].tolist()
schauspieler = schauspieler[~schauspieler["id"].isin(actorsInDB)]

schauspieler['upload_time'] = str(datetime.now())
schauspieler.to_sql("schauspieler", engine, if_exists="append", index = False)

#%% Combine each movie id with all its actors and upload the result to database
spielt_mit = pd.DataFrame(list(zip(movieId, actorId)),
                  columns = ["film_id", "schauspieler_id"])

spielt_mit['upload_time'] = str(datetime.now())
spielt_mit.to_sql("spielt_mit", engine, if_exists="append", index = False)
# %%
