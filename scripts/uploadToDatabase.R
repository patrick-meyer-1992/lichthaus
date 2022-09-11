library(DBI)
library(dbplyr)
library(dplyr)
library(RPostgres)
library(googlesheets4)

pth = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pth)

# Connect to database
db = 'lichthaus'
host_db = 'lichthaus.ddns.net' 
db_port = '54320' 
db_user = 'patrick'
db_password = readLines(con = "secrets.txt", warn = F)[2]
con = dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  

# Load sheets from google
googleLink = "https://docs.google.com/spreadsheets/d/1jQeJynFHIOE-krlfsOPEa8ap0w9G6anrMwqmTYp7LK0/"
film = read_sheet(ss = googleLink, sheet = "film")
teilnehmer = read_sheet(ss = googleLink, sheet = "teilnehmer")
schlaegt_vor = read_sheet(ss = googleLink, sheet = "schlaegt_vor")
stimmt_fuer = read_sheet(ss = googleLink, sheet = "stimmt_fuer")
bewertet = read_sheet(ss = googleLink, sheet = "bewertet")

# Write local copies
write.csv(film, "file.csv")
write.csv(teilnehmer, "teilnehmer.csv")
write.csv(schlaegt_vor, "schlaegt_vor.csv")
write.csv(stimmt_fuer, "stimmt_fuer.csv")
write.csv(bewertet, "bewertet.csv")

# Upload to database
DBI::dbWriteTable(conn = con, name = "film", value = film, overwrite = T)
DBI::dbWriteTable(conn = con, name = "teilnehmer", value = teilnehmer, overwrite = T)
DBI::dbWriteTable(conn = con, name = "schlaegt_vor", value = schlaegt_vor, overwrite = T)
DBI::dbWriteTable(conn = con, name = "stimmt_fuer", value = stimmt_fuer, overwrite = T)
DBI::dbWriteTable(conn = con, name = "bewertet", value = bewertet, overwrite = T)



  
