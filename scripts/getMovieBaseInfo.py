# -*- coding: utf-8 -*-
"""
Created on Fri Sep  2 17:57:29 2022

@author: Patrick
"""

import http.client
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
