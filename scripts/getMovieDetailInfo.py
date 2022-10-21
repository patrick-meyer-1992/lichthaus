# -*- coding: utf-8 -*-
"""
Created on Sat Sep 17 18:26:31 2022

@author: Patrick
"""
import http.client

def saveMovieInfo(lst, apiKey, folder):
    """ 
    Takes a list of movie ids.
    Downloads movie info via apiKey, converts it to json and saves to
    folder
    """
    if len(lst) == 0:
        print("No new IDs")
        return
    
    conn = http.client.HTTPSConnection("movie-details1.p.rapidapi.com")

    headers = {
        'X-RapidAPI-Key': apiKey,
        'X-RapidAPI-Host': "movie-details1.p.rapidapi.com"
        }

    for ID in lst:
        try:            
             conn.request("GET", "/imdb_api/movie?id=" + ID, headers=headers)
             res = conn.getresponse()
             data = res.read()
             with open(folder + "\\" + ID + ".json", "w", encoding="utf-8") as f:
                 f.write(data.decode("utf-8"))
        except Exception as e:
            print("Problem with: " + ID)
            print(e)