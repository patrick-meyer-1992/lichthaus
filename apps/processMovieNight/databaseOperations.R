library(dplyr)
library(reshape2)
library(lubridate)
library(DBI)
library(dbplyr)
#### Database Connection ####
connectToLichthausDB = function(pwd){
  db = 'lichthaus'
  host_db = 'lichthaus.ddns.net' 
  db_port = '54320' 
  db_user = 'patrick'
  db_password = pwd
  con = dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
  return(con)
}
update_film = function(con, df_movie){
  
  movieIDs = df_movie %>% 
    filter(rows != "Bewertung") %>% 
    select(!rows)
  
  idVector = c()
  for(i in 1:nrow(movieIDs)){
    idVector = c(idVector, movieIDs[i,] %>% slice(1) %>% unlist(use.names = F))
  }
  idVector = idVector[idVector != ""]
  
  film = tbl(con, "film") %>% select(!upload_time) %>% collect()
  knownIds = film %>% select(id) %>% pull()
  
  df = tibble(id = idVector) %>% 
    filter(!(id %in% knownIds)) %>% 
    distinct(id)
  
  missingNames = names(film)[!(names(film) %in% names(df))]
  df[missingNames] = NA
  
  df = df %>% mutate(upload_time = now())
  DBI::dbWriteTable(conn = con, name = "film", value = df, append = T)
  return(tibble(df))
  #print("The following titles were added to 'film':")
  #for(i in 1:nrow(df)) print(paste0(df$titel[i], " (", df$id[i], ")"))
}
update_schlaegt_vor = function(con, df_movie, df_murmeled, d){
  
  df = df_movie %>% 
    filter(rows == "Vorschlag") %>% 
    select(!rows) %>% 
    mutate(vorschlagsdatum = d) %>% 
    melt(id.vars = "vorschlagsdatum",
         variable.name = "vorname",
         value.name = "id") %>% 
    filter(id != "") %>% 
    mutate(murmeled = id %in% df_murmeled$murmeled)
  
  df = df %>% mutate(upload_time = now())
  DBI::dbWriteTable(conn = con, name = "schlaegt_vor", value = df, append = T)
  return(tibble(df))
  #print("The following titles were added to 'schlaegt_vor':")
  #for(i in 1:nrow(df)) print(paste0(df$vorname[i], ": ", df$titel[i]," (", df$id[i], ")"))
}
update_stimmt_fuer = function(con, df_movie, winner, d){
  n = sum(df_movie$rows == "Abstimmung")
  df = df_movie %>% 
    filter(rows == "Abstimmung") %>% 
    select(!rows) %>% 
    mutate(stimmdatum = d, wahldurchgang = n) %>% 
    melt(id.vars = c("stimmdatum", "wahldurchgang"),
         variable.name = "vorname",
         value.name = "id") %>% 
    filter(id != "") %>% 
    mutate(sieger = id == winner)

  df = df %>% mutate(upload_time = now())
  DBI::dbWriteTable(conn = con, name = "stimmt_fuer", value = df, append = T)
  return(tibble(df))
  #print("The following votes were added to 'stimmt_fuer':")
  #for(i in 1:nrow(df)) print(paste0(df$vorname[i], ": ", df$titel[i], " (", df$id[i], "), Wahlgang: ", df$wahldurchgang[i]))
}
update_bewertet = function(con, df_movie, movieToday){
  
  df = df_movie %>% 
    filter(rows == "Bewertung") %>% 
    select(!rows) %>% 
    mutate(proxy = 1) %>% 
    melt(id.vars = "proxy",
         variable.name = "vorname",
         value.name = "wertung") %>% 
    filter(wertung != "") %>% 
    mutate(wertung = as.numeric(wertung),
           id = movieToday) %>%
    select(!proxy)
  
  df = df %>% mutate(upload_time = now())
  DBI::dbWriteTable(conn = con, name = "bewertet", value = df, append = T)
  return(tibble(df))
  #print(paste0("The following ratings were added to 'bewertet' for the movie: ", titel, " (", ratedId, ")" ))
  #for(i in 1:nrow(df)) print(paste0(df$vorname[i], ": ", df$wertung[i]))
}
update_filmabend = function(con, movieToday, host, d){
  df = tibble(
    datum = d,
    gehostet_von = host,
    id = movieToday
  )
  
  df = df %>% mutate(upload_time = now())
  DBI::dbWriteTable(conn = con, name = "filmabend", value = df, append = T)
  return(tibble(df))
}
