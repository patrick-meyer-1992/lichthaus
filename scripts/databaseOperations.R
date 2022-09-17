update_film = function(con, df){
  # Reads the ids that already exist in film from database
  # Uploads only the ids from the given df which were not already existent
  knownIds = tbl(con, "film") %>% select(id) %>% collect() %>% pull()
  
  df = df %>% 
    select(id, titel, erscheinungsjahr) %>% 
    filter(!is.na(id)) %>% 
    filter(!(id %in% knownIds))
  
  DBI::dbWriteTable(conn = con, name = "film", value = mutate(df, upload_time = now()), append = T)
  print("The following titles were added to 'film':")
  for(i in 1:nrow(df)) print(paste0(df$titel[i], " (", df$id[i], ")"))
}
update_schlaegt_vor = function(con, df){
  df = df %>% 
    select(vorname, id, titel, murmeled) %>% 
    filter(!is.na(id)) %>% 
    mutate(vorschlagsdatum = today())

  df_upload = df %>% select(!titel)
  DBI::dbWriteTable(conn = con, name = "schlaegt_vor", value = mutate(df_upload, upload_time = now()), append = T)
  print("The following titles were added to 'schlaegt_vor':")
  for(i in 1:nrow(df)) print(paste0(df$vorname[i], ": ", df$titel[i]," (", df$id[i], ")"))
}
update_stimmt_fuer = function(con, df){
  
  df = tbl(con, "film") %>% 
    select(id, titel) %>% 
    right_join(y = df, by = "id", copy = T) %>% 
    collect()
  
  df_upload = df %>% select(!titel)
  DBI::dbWriteTable(conn = con, name = "stimmt_fuer", value = mutate(df_upload, upload_time = now()), append = T)
  print("The following votes were added to 'stimmt_fuer':")
  for(i in 1:nrow(df)) print(paste0(df$vorname[i], ": ", df$titel[i], " (", df$id[i], "), Wahlgang: ", df$wahldurchgang[i]))
}
update_bewertet = function(con, df){
  ratedId = df %>% distinct(id) %>% pull()
  titel = tbl(con, "film") %>% filter(id == ratedId) %>% select(titel) %>% pull()
  DBI::dbWriteTable(conn = con, name = "bewertet", value = mutate(df, upload_time = now()), append = T)
  print(paste0("The following ratings were added to 'bewertet' for the movie: ", titel, " (", ratedId, ")" ))
  for(i in 1:nrow(df)) print(paste0(df$vorname[i], ": ", df$wertung[i]))
}
