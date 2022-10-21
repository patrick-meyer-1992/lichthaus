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
  
  if(nrow(df) > 0){
    df = df %>% mutate(upload_time = now())
    DBI::dbWriteTable(conn = con, name = "film", value = df, append = T)
    return(tibble(df))
  }else{
    return("--Nothing--")
  }
  

}
update_schlaegt_vor = function(con, df_movie, df_murmeled, d, targetFolder){
  
  df = df_movie %>% 
    filter(rows == "Vorschlag") %>% 
    select(!rows) %>% 
    mutate(vorschlagsdatum = d) %>% 
    melt(id.vars = "vorschlagsdatum",
         variable.name = "vorname",
         value.name = "id") %>% 
    filter(id != "") %>% 
    mutate(murmeled = id %in% df_murmeled$murmeled)
  
  existing = tbl(con, "schlaegt_vor") %>% 
    filter(vorschlagsdatum == d) %>% 
    collect()
  
  df = anti_join(df, existing)
  
  write.csv(tbl(con, "schlaegt_vor"), paste0(targetFolder, as.character(d), "_schlaegt_vor.csv"), row.names = F)
  
  if(nrow(df) > 0){
    df = df %>% mutate(upload_time = now())
    DBI::dbWriteTable(conn = con, name = "schlaegt_vor", value = df, append = T)
    return(tibble(df))
  }else{
    return("--Nothing--")
  }
}
update_stimmt_fuer = function(con, df_movie, winner, d, targetFolder){
  n = 1:sum(df_movie$rows == "Abstimmung")
  df = df_movie %>% 
    filter(rows == "Abstimmung") %>% 
    select(!rows) %>% 
    mutate(stimmdatum = d, wahldurchgang = n) %>% 
    melt(id.vars = c("stimmdatum", "wahldurchgang"),
         variable.name = "vorname",
         value.name = "id") %>% 
    filter(id != "") %>% 
    mutate(sieger = id == winner)
  
  existing = tbl(con, "stimmt_fuer") %>% 
    filter(stimmdatum == d) %>% 
    collect()

  write.csv(tbl(con, "stimmt_fuer"), paste0(targetFolder, as.character(d), "_stimmt_fuer.csv"), row.names = F)
  
  if(nrow(df) > 0){
    df = df %>% mutate(upload_time = now())
    DBI::dbWriteTable(conn = con, name = "stimmt_fuer", value = df, append = T)
    return(tibble(df))
  }else{
    return("--Nothing--")
  }
}
update_bewertet = function(con, df_movie, movieToday, d, targetFolder){
  
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
  
  existing = tbl(con, "bewertet") %>% 
    filter(id == movieToday) %>% 
    collect()
  
  df = anti_join(df, existing)
  
  write.csv(tbl(con, "bewertet"), paste0(targetFolder, as.character(d), "_bewertet.csv"), row.names = F)
  
  if(nrow(df) > 0){
    df = df %>% mutate(upload_time = now())
    DBI::dbWriteTable(conn = con, name = "bewertet", value = df, append = T)
    return(tibble(df))
  }else{
    return("--Nothing--")
  }
}
update_filmabend = function(con, movieToday, host, d, targetFolder){
  df = tibble(
    datum = d,
    gehostet_von = host,
    id = movieToday
  )
  
  existing = tbl(con, "filmabend") %>% 
    filter(datum == d) %>% 
    collect()
  
  df = anti_join(df, existing)
  
  write.csv(tbl(con, "filmabend"), paste0(targetFolder, as.character(d), "_filmabend.csv"), row.names = F)
  
  if(nrow(df) > 0){
    df = df %>% mutate(upload_time = now())
    DBI::dbWriteTable(conn = con, name = "filmabend", value = df, append = T)
    return(tibble(df))
  }else{
    return("--Nothing--")
  }

}
