library(DBI)
library(dbplyr)
library(dplyr)
library(plotly)
library(RPostgres)
library(reshape2)
library(stringr)

pth = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pth)

# Connect to database
db = 'lichthaus'
host_db = 'lichthaus.ddns.net' 
db_port = '54320' 
db_user = 'patrick'
db_password = readLines(con = "../secrets.txt", warn = F)[2]

con = dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 

##### Wer hat den längsten Vornamen? ####
df = tbl(con, "teilnehmer")
df = df %>% select(vorname) %>% filter(vorname != "unknown") %>% collect()
df$length = nchar(df$vorname)

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(vorname, length), 
                         y = length
                         ),
           stat = "identity") +
  geom_text(aes(x = vorname,
                y = 0,
                label = paste0(" ", vorname)),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Längster Vorname",
       x = "vorname",
       y = "Länge") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )

plt

ggsave(filename = "Vorname.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

##### Wessen Vorschläge wurden im Schnitt am besten bewertet? ####
schlaegt_vor = tbl(con, "schlaegt_vor") %>% collect()
bewertet = tbl(con, "bewertet") %>% collect()
film = tbl(con, "film") %>% collect()

schlaegt_vor = schlaegt_vor %>% 
  select(vorname, id) %>% 
  distinct(vorname, id) %>% 
  arrange(vorname)

bewertet = bewertet %>% 
  select(id, wertung) %>% 
  filter(id != "#N/A") %>% 
  group_by(id) %>% 
  summarise(avg = mean(wertung))

df = left_join(schlaegt_vor, bewertet, "id")
df = left_join(x = df, y = film[c("id", "titel")], by = "id")

df = df %>% 
  filter(!is.na(avg)) %>% 
  group_by(vorname) %>% 
  summarise(avg = mean(avg))

df$avg = round(df$avg, 2)

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(vorname, avg), 
                         y = avg
  ),
  stat = "identity") +
  geom_text(aes(x = vorname,
                y = 0,
                label = paste0(" ", vorname, " (", avg, ")")),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Wessen Vorschläge wurden wie gut bewertet?",
       x = "vorname",
       y = "Durchschnittliche Wertung") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )

plt

ggsave(filename = "Wertung.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

#### Welcher Film wurde am häufigsten vorgeschlagen? ####
schlaegt_vor = tbl(con, "schlaegt_vor") %>% collect()
film = tbl(con, "film") %>% collect()

tmp = schlaegt_vor %>% 
  select(vorname, id) %>% 
  group_by(id) %>% 
  summarise(freq = n())

df = left_join(tmp, film[c("id", "titel")], "id") %>% 
  select(titel, freq) %>% 
  arrange(desc(freq)) %>% 
  slice(1:10)

df$titel = str_replace(string = df$titel, pattern = ": ", replacement = ":\n ") #Temporary fix for long titles

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(titel, freq), 
                         y = freq
  ),
  stat = "identity") +
  geom_text(aes(x = titel,
                y = 0,
                label = paste0(" ", titel, " (", freq, ")" )
                ),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0
            ) +
  labs(title = "Welcher Film wurde am häufigsten vorgeschlagen?",
       x = "vorname",
       y = "Durchschnittliche Wertung") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

plt

ggsave(filename = "Vorschlag.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

#### Wessen Vorschläge kommen am häufigsten durch die Murmelbahn ####
schlaegt_vor = tbl(con, "schlaegt_vor") %>% collect()
film = tbl(con, "film") %>% collect()

df = schlaegt_vor %>% 
  select(id, vorname, murmeled) %>% 
  group_by(vorname) %>% 
  summarise(gewinner = sum(murmeled), verlierer = sum(!murmeled)) %>% 
  mutate(rate = gewinner / (gewinner + verlierer)) %>% 
  filter(rate < 1)

df$rate = round(df$rate * 100, 2)

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(vorname, rate), 
                         y = rate
  ),
  stat = "identity") +
  geom_text(aes(x = vorname,
                y = 0,
                label = paste0(" ", vorname, " (", rate, "%)" )),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Wer hat am meisten Glück bei der Murmelbahn?",
       x = "vorname",
       y = "Durchschnittliche Wertung") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

plt

ggsave(filename = "Glück.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

#### Was ist unser Lieblingsgenre? ####
schlaegt_vor = tbl(con, "schlaegt_vor")
film = tbl(con, "film")
bewertet = tbl(con, "bewertet")
genre = tbl(con, "genre")
gehoert_zu = tbl(con, "gehoert_zu")
filmabend = tbl(con, "filmabend")

# Welches Genre wurde am häfigsten geschaut?
df = left_join(filmabend, gehoert_zu, by = "id") %>% 
  left_join(genre, by = "bezeichnung") %>%
  select(bezeichnung) %>% 
  group_by(bezeichnung) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  collect() %>% 
  slice(1:10)

df$freq = as.numeric(df$freq)
plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(bezeichnung, freq), 
                         y = freq
  ),
  stat = "identity") +
  geom_text(aes(x = bezeichnung,
                y = 0,
                label = paste0(" ", bezeichnung, " (", freq, ")" )),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Welches Genre wurde am häufigsten angesehen?",
       x = "Genre",
       y = "Häufigkeit") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )
plt

ggsave(filename = "Genre Häufigkeit.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

##### Wer hat den längsten Nachnamen? ####
df = tbl(con, "teilnehmer")
df = df %>% select(nachname) %>% filter(nachname != "unknown") %>% collect()
df$length = nchar(df$nachname)
df = df %>% distinct(nachname, length)

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(nachname, length), 
                         y = length
  ),
  stat = "identity") +
  geom_text(aes(x = nachname,
                y = 0,
                label = paste0(" ", nachname)),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Längster Nachname",
       x = "Nachname",
       y = "Länge") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

plt

ggsave(filename = "Nachname.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

##### Wer hat die meisten Vokale im Namen? ####
df = tbl(con, "teilnehmer")
df = df %>% filter(nachname != "unknown") %>% collect()
df$fullName = paste0(df$vorname, df$nachname)
vowels = str_extract_all(df$fullName, "[aeiouAEIOUäöüÄÖÜ]{1,}")
df$length = NA
for (i in 1:nrow(df)) {
  df$length[i] = length(vowels[[i]])
}

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(vorname, length), 
                         y = length
  ),
  stat = "identity") +
  geom_text(aes(x = vorname,
                y = 0,
                label = paste0(" ", vorname, " ", nachname, " (" , length, ")")),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Die meisten Vokale im Namen") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

plt

ggsave(filename = "Vokale.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")
##### Wer hat die meisten aufeinanderfolgenden Konsonanten im Namen? ####

df = tbl(con, "teilnehmer")
df = df %>% filter(nachname != "unknown") %>% collect()
df$fullName = paste0(df$vorname, df$nachname)
notVowels = str_extract_all(df$fullName, "[^aeiouAEIOUäöüÄÖÜ]{1,}")
df$length = NA
for (i in 1:nrow(df)) {
  df$length[i] = max(nchar(notVowels[[i]]))
}

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(vorname, length), 
                         y = length
  ),
  stat = "identity") +
  geom_text(aes(x = vorname,
                y = 0,
                label = paste0(" ", vorname, " ", nachname, " (" , length, ")")),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Die meisten aufeinanderfolgenden Konsonanten im Namen") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

plt

ggsave(filename = "Konsonanten.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

#### Wer bewertet wessen Vorschläge wie gut? ####
# Einschränkung: Es werden nur die als Vorschlagende eines Films gewertet,
# die ihn an dem Abend vorgeschlagen haben, an dem er gewählt wurde.

schlaegt_vor = tbl(con, "schlaegt_vor") %>% select(!upload_time) %>% collect()
film = tbl(con, "film") %>% select(!upload_time) %>% collect()
bewertet = tbl(con, "bewertet") %>% select(!upload_time) %>% collect()
stimmt_fuer = tbl(con, "stimmt_fuer") %>% select(!upload_time) %>% collect()

df = left_join(schlaegt_vor, film, by = "id") %>% 
  left_join(stimmt_fuer, by = "id", 
            suffix = c(".schlaegt_vor", ".stimmt_fuer")) %>% 
  filter(sieger & stimmdatum == vorschlagsdatum) %>% 
  select(c("vorname.schlaegt_vor", "id", "titel", )) %>% 
  left_join(bewertet, by = "id",
            suffix = c(".schlaegt_vor", ".bewertet")) %>% 
  group_by(vorname.schlaegt_vor, vorname) %>% 
  summarise(avg = mean(wertung)) %>% 
  rename(vorschlagender = vorname.schlaegt_vor,
         bewerter = vorname) %>% 
  filter(!is.na(avg) & vorschlagender != "Ben")

df$clr = sapply(df$avg, FUN = function(x){
  if(x < 8) "white" else "black"
})

plt = ggplot(df, aes(vorschlagender, bewerter, fill= avg)) + 
  geom_tile(show.legend = F) +
  geom_text(aes(x = vorschlagender,
                y = bewerter,
                label = round(avg, 2)),
            angle = 0,
            size = 3.5,
            color = df$clr,
            hjust = 0.5) +
  scale_fill_distiller(palette = "Greys") +
  labs(title = "Wer wessen Vorschläge wie gut bewertet",
       x = "Vorschlagender",
       y = "Bewerter") +
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        axis.title = element_text(size = 5), 
        axis.text = element_text(size = 8)
  )

plt

ggsave(filename = "Wertungsmatrix.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")
