# -*- coding: utf-8 -*-
"""
Created on Sat Oct  8 15:49:58 2022

@author: Patrick
"""

import json
import pandas as pd


basics = pd.read_csv("../data/imdb/title.basics/data.tsv", sep = "\t", na_values = "\\N")
ratings = pd.read_csv("../data/imdb/title.ratings/data.tsv", sep = "\t", na_values = "\\N")
crew = pd.read_csv("../data/imdb/title.crew/data.tsv", sep = "\t", na_values = "\\N")
principals = pd.read_csv("../data/imdb/title.principals/data.tsv", sep = "\t", na_values = "\\N")
names = pd.read_csv("../data/imdb/name.basics/data.tsv", sep = "\t", na_values = "\\N")
#akas = pd.read_csv("data/imdb/title.akas/data.tsv", sep = "\t", na_values = "\\N")

ID = "tt2764598"
basicsSlice = basics[basics["tconst"] == ID]
ratingsSlice = ratings[ratings["tconst"] == ID]
crewSlice = crew[crew["tconst"] == ID]
principalsSlice = principals[principals["tconst"] == ID]
principalsSlice = pd.merge(principalsSlice, names, how="inner", on="nconst")
principalsSlice = principalsSlice[["nconst", "category", "primaryName"]]

actors = principalsSlice[(principalsSlice['category'] == "actor") | (principalsSlice['category'] == "actress")].drop("category", axis = 1)
actorList = []
for ind in actors.index:
    actorList.append({
        "id": actors["nconst"][ind],
        "name": actors["primaryName"][ind]
        })
    
directors = principalsSlice[(principalsSlice['category'] == "director")].drop("category", axis = 1)
directorList = []
for ind in directors.index:
    directorList.append({
        "id": directors["nconst"][ind],
        "name": directors["primaryName"][ind]
        })
    
writers = principalsSlice[(principalsSlice['category'] == "writer")].drop("category", axis = 1)
writerList = []
for ind in writers.index:
    writerList.append({
        "id": writers["nconst"][ind],
        "name": writers["primaryName"][ind]
        })
    

dct = {"id": ID,
       "title": basicsSlice.iloc[0]["primaryTitle"],
       "rating": ratingsSlice.iloc[0]["averageRating"],
       "rating_count": ratingsSlice.iloc[0]["numVotes"].item(),
       "release_year": int(basicsSlice.iloc[0]["startYear"]),
       "imdb_type": basicsSlice.iloc[0]["titleType"],
       "runtime": int(basicsSlice.iloc[0]["runtimeMinutes"]),
       "genres": basicsSlice.iloc[0]["genres"].split(","),
       "directors": directorList,
       "writers": writerList,
       "actors": actorList
       }


with open(ID + ".json", "w", encoding="charmap") as outfile:
    json.dump(dct, outfile)
