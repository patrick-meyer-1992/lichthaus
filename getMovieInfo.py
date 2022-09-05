# -*- coding: utf-8 -*-
"""
Created on Fri Sep  2 17:57:29 2022

@author: Patrick
"""

import http.client
import pandas as pd
import numpy as np
import json


def getMovieBaseInfo(q: str, apiKey: str):

    """ Takes the title of a movie as a string and requests
        id, title and release year as given by imdb via api.
        It returns those 3 values. """   
        
    # Define api connection
    conn = http.client.HTTPSConnection("online-movie-database.p.rapidapi.com")
    
    headers = {
        'X-RapidAPI-Key': apiKey,
        'X-RapidAPI-Host': "online-movie-database.p.rapidapi.com"
        }
    
    #Format the given movie title q
    q = q.replace(" ", "%20").replace("ü", "ue").replace("ä", "ae").replace("ö", "oe").replace("ß", "ss").replace("Ö", "Oe").replace("Ä", "Ae").replace("Ü", "Ue")
    
    # Make a request
    conn.request("GET", "/auto-complete?q=" + q, headers=headers)
    
    # Get the response and save it to data
    res = conn.getresponse()
    data = res.read().decode("utf-8")
    
    #Convert to json
    data = json.loads(data)
    id = data["d"][0]["id"]
    title = data["d"][0]["l"]
    releaseYear = data["d"][0]["y"]
    
    return(id, title, releaseYear)

pth = input("Enter the path of your file: ") + "\\"
df = pd.read_csv(pth + "Lichthaus_DB - film.csv")
secret = open(pth + "secrets.txt", "r").readline().replace("\n", "")


for t in df["titel"]:  
    
    idx = np.where(df["titel"] == t)[0][0]
    
    if not df.loc[df["titel"] == t, "id"].isna().bool():
        #print("Skipping: " + t)
        #print("########################################")
        continue
    
    try:
        tmp = getMovieBaseInfo(t, secret)
        oldYear = df.loc[df["titel"] == t, "erscheinungsjahr"][idx].astype(int)
        
        df.loc[df["titel"] == t, "id"] = tmp[0]
        df.loc[df["titel"] == t, "originalTitle"] = tmp[1]
        df.loc[df["titel"] == t, "erscheinungsjahr"] = tmp[2]
        
        print("Updated values for: " + t)
        print("New title is: " + tmp[1])
        
        
        print("The year was: " + str(oldYear))
        print("and is now  : " + str(tmp[2]))
        
        print("########################################")
        
    except Exception:
        print("There was a problem with: " + t)
        print("########################################")
        continue
    
    


# Convert erscheinungsjahr to int
#idx = np.where(df["erscheinungsjahr"].isnull())[0]
idx = df.loc[pd.isna(df["erscheinungsjahr"]), :].index



df.loc[idx, "erscheinungsjahr"] = 9999
df["erscheinungsjahr"] = df["erscheinungsjahr"].astype(int)


#Export to csv
df.to_csv("Lichthaus_DB - film_updated.csv", index = False)
