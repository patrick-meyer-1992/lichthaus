# -*- coding: utf-8 -*-
"""
Created on Sat Sep 24 19:24:51 2022

@author: Patrick
"""

import pandas as pd
import psycopg2
from sqlalchemy import create_engine
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
pwd = open("secrets.txt", "r").readlines()[1]

# Connect to database
con = psycopg2.connect(
    host="lichthaus.ddns.net",
    port = "54320",
    database="lichthaus",
    user="patrick",
    password=pwd)

engine = create_engine('postgresql+psycopg2://patrick:' + pwd + '@lichthaus.ddns.net:54320/lichthaus')  


# Get data for model
genres = pd.read_sql_table("genre", engine)
genres = list(genres["bezeichnung"])

q = """SELECT film.id, titel, bezeichnung, vorname, wertung
        FROM film
        JOIN gehoert_zu ON film.id = gehoert_zu.id
        JOIN bewertet ON film.id = bewertet.id"""
        
        
dfBase= pd.read_sql_query(q, engine)
dfBase[genres] = 0

# Fill the genres columns with 1 if the movie belongs that genre
for t in dfBase["titel"]:
    thisGenres = dfBase[dfBase["titel"] == t]["bezeichnung"] # Select all genres from the current title
    thisGenres = list(thisGenres.unique()) # Make sure the genres are unique and transform to list
    for g in thisGenres: # Set the relevant columns in the current row to 1
       dfBase.loc[dfBase["titel"] == t, g] = 1
       
dfBase = dfBase.drop(["id", "bezeichnung"], axis = 1)    
dfBase = dfBase.drop_duplicates(subset = ["titel", "vorname"])

# Select the participants
vorname = pd.read_sql_table("teilnehmer", engine)
vorname = list(vorname["vorname"])

for teilnehmer in vorname:
    df = dfBase[dfBase["vorname"] == teilnehmer] 
    if df.shape[0] == 0:
        continue

    X = df[genres]
    y = df['wertung']
    
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)    
    
    model = LinearRegression()
    model.fit(X_train,y_train)
    coeff_parameter = pd.DataFrame(model.coef_,X.columns,columns=['Coefficient'])
    coeff_parameter["Coefficient"] = coeff_parameter["Coefficient"].round(3)
    
    
    predictions = model.predict(X_test)
    diff = (predictions - y_test).abs()
    print("#################### " + teilnehmer + " ####################")
    print(coeff_parameter)
    print("Mean difference: " + str(round(diff.mean(), 3)))
    print("Standard deviation: " + str(round(diff.std(), 3)))
