# -*- coding: utf-8 -*-
"""
Created on Wed Sep 21 17:18:09 2022

@author: Patrick
"""


import os
import pandas as pd
import json
import psycopg2
from sqlalchemy import create_engine
from datetime import datetime

# Initialize
targetFolder = "data/json/"
pwd = open("secrets.txt", "r").readlines()[1]

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

# Connect to database
con = psycopg2.connect(
    host="lichthaus.ddns.net",
    port = "54320",
    database="lichthaus",
    user="patrick",
    password=pwd)

engine = create_engine('postgresql+psycopg2://patrick:' + pwd + '@lichthaus.ddns.net:54320/lichthaus')    

# Get all existing .json files, read genre info from them and combine them with the ids
existingIDs = os.listdir(targetFolder)
existingIDs = [f.replace(".json", "") for f in existingIDs]
actorId = []
actorName = []
movieId = []

for i in existingIDs:
    print("#######################")
    print(i)
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
        break     
 

# Extract the invidual actors and upload them to database
schauspieler = pd.DataFrame(list(zip(actorId, actorName)),
                     columns = ["id", "name"])
# Some names appear more than once with the same id because of en-/decoding problems
# All the duplicate names are removed and only the first one is kept.
# Note that the first one could very well be one with faulty letters
schauspieler = schauspieler.drop_duplicates()
schauspieler["dupl"] = schauspieler["id"].duplicated()
schauspieler = schauspieler.sort_values(by = "dupl", axis = 0, ascending = False)
schauspieler = schauspieler.drop(schauspieler[schauspieler["dupl"]].index)
schauspieler = schauspieler.drop("dupl", axis = 1)


schauspieler['upload_time'] = str(datetime.now())
schauspieler.to_sql("schauspieler", engine, if_exists="append", index = False)

# Combine each id with all its genres and upload the result to database
spielt_mit = pd.DataFrame(list(zip(movieId, actorId)),
                  columns = ["film_id", "schauspieler_id"])

spielt_mit['upload_time'] = str(datetime.now())
spielt_mit.to_sql("spielt_mit", engine, if_exists="append", index = False)