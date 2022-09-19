library(DBI)
library(dbplyr)
library(dplyr)
library(plotly)
library(RPostgres)
library(reshape2)

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
                label = vorname),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = -0.1) +
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
                label = paste0(vorname, " (", avg, ")")),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = -0.1) +
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

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(titel, freq), 
                         y = freq
  ),
  stat = "identity") +
  geom_text(aes(x = titel,
                y = 0,
                label = paste0(titel, " (", freq, ")" )),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = -0.1) +
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
                label = paste0(vorname, " (", rate, "%)" )),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = -0.1) +
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

# Welches Genre wurde am häfigsten geschaut?
df = left_join(film, gehoert_zu, by = "id") %>% 
  left_join(genre, by = "bezeichnung") %>% 
  filter(!is.na(zeigedatum) & !is.na(bezeichnung)) %>% 
  select(id, titel, zeigedatum, bezeichnung) %>% 
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
                label = paste0(bezeichnung, " (", freq, ")" )),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = -0.1) +
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
