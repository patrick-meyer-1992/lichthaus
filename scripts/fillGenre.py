# -*- coding: utf-8 -*-
"""
Created on Sun Sep 18 16:32:29 2022

@author: Patrick
"""

import os
import pandas as pd
import json
import psycopg2
from sqlalchemy import create_engine
from datetime import datetime

# Initialize
targetFolder = input("Choose target folder: \n")
pwd = input("Enter password for db: \n")

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
bezeichnung = []
id = []

for i in existingIDs:
    print("#######################")
    print(i)
    try: 
        f = open(targetFolder + "\\" + i + ".json", "r", encoding = "charmap")
        g = json.load(f)["genres"]
        id.extend(repeat(i, len(g)))
        bezeichnung.extend(g)
    except:
        print("Error for: " + i)
        continue     
 

# Extract the invidual genres and upload them to database
genre = pd.DataFrame(set(bezeichnung),
                     columns = ["bezeichnung"])
genre['upload_time'] = str(datetime.now())
genre.to_sql("genre", engine, if_exists="append", index = False)

# Combine each id with all its genres and upload the result to database
gehoert_zu = pd.DataFrame(list(zip(id, bezeichnung)),
                  columns = ["id", "bezeichnung"])

gehoert_zu['upload_time'] = str(datetime.now())
gehoert_zu.to_sql("gehoert_zu", engine, if_exists="append", index = False)
