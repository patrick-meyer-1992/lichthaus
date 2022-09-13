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
df = df %>% select(name) %>% filter(name != "unknown") %>% collect()
df$length = nchar(df$name)

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(name, length), 
                         y = length
                         ),
           stat = "identity") +
  geom_text(aes(x = name,
                y = 0,
                label = name),
            angle = 90,
            size = 20,
            color = "white",
            hjust = -0.1) +
  labs(title = "Längster Vorname",
       x = "Name",
       y = "Länge") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )

plt

##### Wessen Vorschläge wurden im Schnitt am besten bewertet? ####
schlaegt_vor = tbl(con, "schlaegt_vor") %>% collect()
bewertet = tbl(con, "bewertet") %>% collect()
film = tbl(con, "film") %>% collect()

schlaegt_vor = schlaegt_vor %>% 
  select(name, id) %>% 
  distinct(name, id) %>% 
  arrange(name)

bewertet = bewertet %>% 
  select(id, rating) %>% 
  filter(id != "#N/A") %>% 
  group_by(id) %>% 
  summarise(avg = mean(rating))

df = left_join(schlaegt_vor, bewertet, "id")
df = left_join(x = df, y = film[c("id", "titel")], by = "id")

df = df %>% 
  filter(!is.na(avg)) %>% 
  group_by(name) %>% 
  summarise(avg = mean(avg))

df$avg = round(df$avg, 2)

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(name, avg), 
                         y = avg
  ),
  stat = "identity") +
  geom_text(aes(x = name,
                y = 0,
                label = paste0(name, " (", avg, ")")),
            angle = 90,
            size = 20,
            color = "white",
            hjust = -0.1) +
  labs(title = "Wessen Vorschläge wurden wie gut bewertet?",
       x = "Name",
       y = "Durchschnittliche Wertung") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )

plt

#### Welcher Film wurde am häufigsten vorgeschlagen? ####
schlaegt_vor = tbl(con, "schlaegt_vor") %>% collect()
film = tbl(con, "film") %>% collect()

test = schlaegt_vor %>% 
  select(name, id) %>% 
  group_by(id) %>% 
  summarise(freq = n())

df = left_join(test, film[c("id", "titel")], "id") %>% 
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
            size = 12,
            color = "white",
            hjust = -0.1) +
  labs(title = "Welcher Film wurde am häufigsten vorgeschlagen?",
       x = "Name",
       y = "Durchschnittliche Wertung") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

plt

#### Wessen Vorschläge kommen am häufigsten durch die Murmelbahn ####
schlaegt_vor = tbl(con, "schlaegt_vor") %>% collect()
film = tbl(con, "film") %>% collect()

df = schlaegt_vor %>% 
  select(id, name, murmeled) %>% 
  group_by(name) %>% 
  summarise(gewinner = sum(murmeled), verlierer = sum(!murmeled)) %>% 
  mutate(rate = gewinner / (gewinner + verlierer)) %>% 
  filter(rate < 1)

df$rate = round(df$rate * 100, 2)

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(name, rate), 
                         y = rate
  ),
  stat = "identity") +
  geom_text(aes(x = name,
                y = 0,
                label = paste0(name, " (", rate, "%)" )),
            angle = 90,
            size = 12,
            color = "white",
            hjust = -0.1) +
  labs(title = "Wer hat am meisten Glück bei der Murmelbahn?",
       x = "Name",
       y = "Durchschnittliche Wertung") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

plt
