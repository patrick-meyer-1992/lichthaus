library(DBI)
library(dbplyr)
library(dplyr)
library(RPostgres)
library(lubridate)
library(reticulate)

#### Initialize working directory ####
pth = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pth)
apiKey = readLines(con = "../secrets.txt", warn = F)[1]
useAPI = T
usedDate = lubridate::as_date("2022-09-17")

#### Load python script and functions ####
py = reticulate::py_run_file("getMovieBaseInfo.py")
source("databaseOperations.R")

#### Database Connection ####
db = 'lichthaus'
host_db = 'lichthaus.ddns.net' 
db_port = '54320' 
db_user = 'patrick'
db_password = readLines(con = "../secrets.txt", warn = F)[2]
con = dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 

# Create a frame with the suggestion for that evening
teilnehmer = tbl(con, "teilnehmer") %>% 
  select(vorname) %>% 
  filter(!(vorname %in% c("Nikita", "Marco", "unknown"))) %>% 
  pull()

#### Suggestions ####
suggestions = tibble(
  vorname = rep(teilnehmer, each = 2), 
  vorschlag = NA,
  titel = NA,
  id = NA,
  erscheinungsjahr = NA,
  datum = usedDate,
  murmeled = F
  )

# Enter each suggestions. If none were made by this participant on this evening leave as NA
suggestions$vorschlag[which(suggestions$vorname == "Cem")] = c("Crime Busters", "Jumanji 1995")
suggestions$vorschlag[which(suggestions$vorname == "Daniel")] = c(NA, NA)
suggestions$vorschlag[which(suggestions$vorname == "Jan")] = c("The Voices", "Mom and Dad")
suggestions$vorschlag[which(suggestions$vorname == "Patrick")] = c("Tron", "Babylon")
suggestions$vorschlag[which(suggestions$vorname == "Stefan")] = c(NA, NA)
suggestions$vorschlag[which(suggestions$vorname == "Timo")] = c("Knives Out 2019", "Idiocracy")
suggestions$vorschlag[which(suggestions$vorname == "Toni")] = c(NA, NA)

# Enter the winners of the murmelbahn
# Use the names which were entered as the original suggestions 'vorschlag'
# (Not the original titles from imdb)
survivors = c("Tron", "The Voices")
suggestions$murmeled[which(suggestions$vorschlag %in% survivors)] = T

#### Find id via API ####
if(useAPI){
  for (i in 1:nrow(suggestions)){
    if(!is.na(suggestions$vorschlag[i]) && is.na(suggestions$id[i])){
      print(paste0("Trying for: ", suggestions$vorschlag[i]))
      tryCatch({
        result = py$getMovieBaseInfo(suggestions$vorschlag[i], apiKey)
        suggestions$id[i] = result[[1]]
        suggestions$originalTitel[i] = result[[2]]
        suggestions$erscheinungsjahr[i] = result[[3]]
      },
      error = function(e){
        print(paste0("There was a problem with : ", suggestions$vorschlag[i]))
        print(paste0("ErrorMsg: ", e))
      })
    }
    if(i == nrow(suggestions)) print("Finished")
  }
  
  # If there were problems with filling the ids automatically enter the data manually
  errIndex = which(!is.na(suggestions$vorschlag) & is.na(suggestions$id))
  suggestions$id[errIndex] = c("")
  suggestions$titel[errIndex] = c("")
  suggestions$erscheinungsjahr[errIndex] = c()
}else{
  # Manual fill if api doesn't work
  suggestions$id[which(suggestions$vorname == "Cem")] = c("tt0074442", "tt0113497")
  suggestions$id[which(suggestions$vorname == "Daniel")] = c(NA, NA)
  suggestions$id[which(suggestions$vorname == "Jan")] = c("tt1567437", "tt5462326")
  suggestions$id[which(suggestions$vorname == "Patrick")] = c("tt1104001", "tt10640346")
  suggestions$id[which(suggestions$vorname == "Stefan")] = c(NA, NA)
  suggestions$id[which(suggestions$vorname == "Timo")] = c("tt8946378", "tt0387808")
  suggestions$id[which(suggestions$vorname == "Toni")] = c(NA, NA)
  
  suggestions$erscheinungsjahr[which(suggestions$vorname == "Cem")] = c(1977, 1995)
  suggestions$erscheinungsjahr[which(suggestions$vorname == "Daniel")] = c(NA, NA)
  suggestions$erscheinungsjahr[which(suggestions$vorname == "Jan")] = c(2014, 2017)
  suggestions$erscheinungsjahr[which(suggestions$vorname == "Patrick")] = c(2010, 2022)
  suggestions$erscheinungsjahr[which(suggestions$vorname == "Stefan")] = c(NA, NA)
  suggestions$erscheinungsjahr[which(suggestions$vorname == "Timo")] = c(2019, 2006)
  suggestions$erscheinungsjahr[which(suggestions$vorname == "Toni")] = c(NA, NA)
  
  suggestions$titel[which(suggestions$vorname == "Cem")] = c("Zwei außer Rand und Band", "Jumanji")
  suggestions$titel[which(suggestions$vorname == "Daniel")] = c(NA, NA)
  suggestions$titel[which(suggestions$vorname == "Jan")] = c("The Voices", "Mom and Dad")
  suggestions$titel[which(suggestions$vorname == "Patrick")] = c("Tron", "Babylon")
  suggestions$titel[which(suggestions$vorname == "Stefan")] = c(NA, NA)
  suggestions$titel[which(suggestions$vorname == "Timo")] = c("Knives Out", "Idiocracy")
  suggestions$titel[which(suggestions$vorname == "Toni")] = c(NA, NA)
}

#### Voting ####  
numVotes = 2
gesamtSieger = ""
# Create a frame to reflect the voting of the evening
voting = tibble(
  vorname = rep(teilnehmer, each = numVotes), 
  vote = NA,
  wahldurchgang = NA,
  sieger = F
)
# Enter votes from each participant
voting[which(voting$vorname == "Cem"), c("vote", "wahldurchgang")] = list(c("The Voices", "Tron"), #Cem
                                                                          c(1:numVotes))

voting[which(voting$vorname == "Daniel"), c("vote", "wahldurchgang")] = list(c(NA), #Daniel
                                                                             c(NA))

voting[which(voting$vorname == "Jan"), c("vote", "wahldurchgang")] = list(c("Tron", "Tron"), #Jan
                                                                          c(1:numVotes))

voting[which(voting$vorname == "Patrick"), c("vote", "wahldurchgang")] = list(c("Tron", "Tron"), #Patrick
                                                                              c(1:numVotes))

voting[which(voting$vorname == "Stefan"), c("vote", "wahldurchgang")] = list(c(NA), #Stefan
                                                                             c(NA))

voting[which(voting$vorname == "Timo"), c("vote", "wahldurchgang")] = list(c("The Voices", "The Voices"), #Timo
                                                                           c(1:numVotes))

voting[which(voting$vorname == "Toni"), c("vote", "wahldurchgang")] = list(c(NA), #Toni
                                                                           c(NA))

voting = inner_join(x = voting, 
                    y = suggestions[c("vorschlag", "id")], 
                    by = c("vote" = "vorschlag")) %>% 
  filter(!is.na(id))
voting$sieger[which(voting$vote == gesamtSieger)] = T
voting = voting %>% 
  mutate(stimmdatum = usedDate) %>% 
  select(!vote) %>% 
  arrange(wahldurchgang, vorname)

#### Rating ####
rating = tibble(
  vorname = teilnehmer,
  id = NA,
  wertung = NA,
)

ratedMovie = "tt1104001"
# Enter each rating If none were made by this participant for this movie leave as NA
rating[which(rating$vorname == "Cem"), c("id", "wertung")] = list(ratedMovie, 9)
rating[which(rating$vorname == "Daniel"), c("id", "wertung")] = list(ratedMovie, NA)
rating[which(rating$vorname == "Jan"), c("id", "wertung")] = list(ratedMovie, 9)
rating[which(rating$vorname == "Patrick"), c("id", "wertung")] = list(ratedMovie, 9)
rating[which(rating$vorname == "Stefan"), c("id", "wertung")] = list(ratedMovie, NA)
rating[which(rating$vorname == "Timo"), c("id", "wertung")] = list(ratedMovie, 9)
rating[which(rating$vorname == "Toni"), c("id", "wertung")] = list(ratedMovie, NA)
rating = rating %>% filter(!is.na(wertung))

#### Upload to database ####
update_film(con, suggestions)
update_schlaegt_vor(con, suggestions)
update_stimmt_fuer(con, voting)
update_bewertet(con, rating)
  
 




