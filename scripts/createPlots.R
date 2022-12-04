library(DBI)
library(dbplyr)
library(dplyr)
library(plotly)
library(RPostgres)
library(reshape2)
library(stringr)
library(cowplot)
library(magick)
library(gridExtra)

pth = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pth)

##### Connect to database ####
db = 'lichthaus'
host_db = 'lichthaus.ddns.net' 
db_port = '54320' 
db_user = 'patrick'
db_password = readLines(con = "../secrets.txt", warn = F)[2]

con = dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 

##### Download tables #### 
teilnehmer = tbl(con, "teilnehmer") %>% collect()
schlaegt_vor = tbl(con, "schlaegt_vor") %>% select(!upload_time) %>% collect() 
film = tbl(con, "film") %>% select(!upload_time) %>% collect()
bewertet = tbl(con, "bewertet") %>% select(!upload_time) %>% collect()
genre = tbl(con, "genre") %>% select(!upload_time) %>% collect()
gehoert_zu = tbl(con, "gehoert_zu") %>% select(!upload_time) %>% collect()
filmabend = tbl(con, "filmabend") %>% select(!upload_time) %>% collect()
schauspieler = tbl(con, "schauspieler") %>% select(!upload_time) %>% collect()
spielt_mit = tbl(con, "spielt_mit") %>% select(!upload_time) %>% collect()
stimmt_fuer = tbl(con, "stimmt_fuer") %>% select(!upload_time) %>% collect()
vorschlagende = tbl(con, "vorschlagende") %>% collect()

##### Wer hat den längsten Vornamen? ####
df = teilnehmer
df = df %>% select(vorname) %>% filter(vorname != "unknown")
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
df = schlaegt_vor %>% 
  left_join(stimmt_fuer, by = "id") %>% 
  filter(sieger &
         vorschlagsdatum == stimmdatum) %>% 
  select(vorname.x, id) %>%
  rename(vorname = vorname.x) %>% 
  distinct() %>% 
  left_join(bewertet %>% select(!vorname), by = "id") %>% 
  filter(!is.na(wertung)) %>% 
  group_by(vorname) %>% 
  summarise(avg = round(mean(wertung), 2))


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

##### Welcher Film wurde am häufigsten vorgeschlagen? ####
tmp = schlaegt_vor %>% 
  select(vorname, id) %>% 
  group_by(id) %>% 
  summarise(freq = n())

df = left_join(tmp, film[c("id", "titel", "image_link")], "id") %>% 
  select(titel, freq, image_link) %>% 
  arrange(desc(freq)) %>% 
  slice(1:10) %>% 
  arrange(freq)

top_img = c()
bottom_img = c()
# Choose top images and combine them
for (i in 6:10) {
  titel = df$titel[i]
  rating = df$freq[i]
  s = if (str_length(df$titel[i]) > 20) 11 else 18
  img = magick::image_read(path = df$image_link[i]) # Initialize image
  img = image_scale(image = img, geometry = "190") # Cut to...
  img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
  img = image_border(image = img, geometry = "0x45") # Add grey...
  img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
  img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
  img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
  img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
  img = image_border(image = img, color = "white", geometry = "0x50") # Add white border on top + bottom
  img = image_crop(image = img, geometry = paste0("0x390")) # Remove white border from bottom
  
  top_img = image_join(top_img, img)
}
top_img = image_append(top_img)
#top_img = image_annotate(image = top_img, text = paste0("Top"), size = 20, gravity = "north")

# Choose flop images and combine them
for (i in 1:5) {
  titel = df$titel[i]
  rating = df$freq[i]
  s = if (str_length(df$titel[i]) > 20) 11 else 18
  img = magick::image_read(path = df$image_link[i]) # Initialize image
  img = image_scale(image = img, geometry = "190") # Cut to...
  img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
  img = image_border(image = img, geometry = "0x45") # Add grey...
  img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
  img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
  img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
  img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
  img = image_border(image = img, color = "white", geometry = "0x30") # Add white border on top
  
  bottom_img = image_join(bottom_img, img)
}
bottom_img = image_append(bottom_img)
#bottom_img = image_annotate(image = bottom_img, text = "Flop", size = 20, gravity = "north")

comb_image = image_append(c(top_img, bottom_img), stack = T)
image_write(comb_image, path = paste0("..\\results\\Vorschlag", ".png"), format = "png")


##### Wessen Vorschläge kommen am häufigsten durch die Murmelbahn ####
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

##### Was ist unser Lieblingsgenre? ####
# Welches Genre wurde am häufigsten geschaut?
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

# Welches Genre wurde am besten bewertet? 
df = 
  left_join(bewertet, gehoert_zu, by = "id") %>% 
  select(bezeichnung, wertung) %>% 
  group_by(bezeichnung) %>% 
  summarise(avg = round(mean(wertung), 2)) %>% 
  arrange(desc(avg)) %>% 
  slice(1:10)

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(bezeichnung, avg), 
                         y = avg
  ),
  stat = "identity") +
  geom_text(aes(x = bezeichnung,
                y = 0,
                label = paste0(" ", bezeichnung, " (", avg, ")" )),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Welches Genre wurde am besten bewertet?",
       x = "Genre",
       y = "Wertung") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )
plt

ggsave(filename = "Genre Wertung.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

##### Wer hat den längsten Nachnamen? ####
df = teilnehmer
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
df = teilnehmer
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

df = teilnehmer
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

##### Wer bewertet wessen Vorschläge wie gut? ####
# Einschränkung: Es werden nur die als Vorschlagende eines Films gewertet,
# die ihn an dem Abend vorgeschlagen haben, an dem er gewählt wurde.
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

##### Wer hat die meisten unterschiedlichen Vorschläge eingebracht? ####
df = # Anzahl individueller Vorschläge
  schlaegt_vor %>% 
  select(vorname, id) %>%
  distinct() %>% 
  group_by(vorname) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  slice(1:8)

df_total = # Anzahl Vorschläge insgesamt
  schlaegt_vor %>% 
  select(vorname, id) %>% 
  group_by(vorname) %>% 
  summarise(freq_total = n()) %>% 
  arrange(desc(freq_total)) %>% 
  slice(1:8)

df = left_join(df, df_total, by = "vorname")
df = mutate(df, ratio = round(freq / freq_total * 100, 2))

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(vorname, ratio), 
                         y = ratio
  ),
  stat = "identity") +
  geom_text(aes(x = vorname,
                y = 0,
                label = paste0(" ", vorname, " (", ratio, "%)" )),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Wer hat die meisten unterschiedlichen Vorschläge eingebracht?",
       x = "Name",
       y = "Vorschläge") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )
plt

ggsave(filename = "Anzahl Vorschläge.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")


##### Wer hat am häufigsten für die Filme gestimmt, die an dem Abend auch gewonnen haben?####
df = # Anzahl Abstimmungen für Sieger des Abends
  stimmt_fuer %>% 
  filter(sieger) %>% 
  group_by(vorname) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  slice(1:8)

df_total = # Anzahl Abstimmungen insgesamt
  stimmt_fuer %>% 
  group_by(vorname) %>% 
  summarise(freq_total = n()) %>% 
  arrange(desc(freq_total)) %>% 
  slice(1:8)

df = left_join(df, df_total, by = "vorname")
df = mutate(df, ratio = round(freq / freq_total * 100, 2))

plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(vorname, ratio), 
                         y = ratio
  ),
  stat = "identity") +
  geom_text(aes(x = vorname,
                y = 0,
                label = paste0(" ", vorname, " (", ratio, "%)" )),
            angle = 90,
            size = 3.5,
            color = "white",
            hjust = 0) +
  labs(title = "Wer hat am häufigsten für den Sieger des Abends gestimmt?",
       x = "Name",
       y = "Abstimmung") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )
plt

ggsave(filename = "Königsmacher.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")
##### Welcher Film hat am längsten gebraucht vom ersten Vorschlag bis zur Ausstrahlung? ####
df = 
  left_join(filmabend, schlaegt_vor, by = "id") %>% 
  select(id, datum, vorschlagsdatum) %>% 
  arrange(id, datum, vorschlagsdatum) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(diff = datum - vorschlagsdatum) %>% 
  left_join(film, by = "id") %>% 
  select(titel, diff, image_link) %>% 
  arrange(desc(diff)) %>% 
  slice(1:10) %>% 
  arrange(diff)

top_img = c()
bottom_img = c()
# Choose top images and combine them
for (i in 6:10) {
  titel = df$titel[i]
  rating = df$diff[i]
  s = if (str_length(df$titel[i]) > 20) 11 else 18
  img = magick::image_read(path = df$image_link[i]) # Initialize image
  img = image_scale(image = img, geometry = "190") # Cut to...
  img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
  img = image_border(image = img, geometry = "0x45") # Add grey...
  img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
  img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
  img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
  img = image_annotate(img, paste0("(", rating, " Tage)"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
  img = image_border(image = img, color = "white", geometry = "0x50") # Add white border on top + bottom
  img = image_crop(image = img, geometry = paste0("0x390")) # Remove white border from bottom
  
  top_img = image_join(top_img, img)
}
top_img = image_append(top_img)
#top_img = image_annotate(image = top_img, text = paste0("Top"), size = 20, gravity = "north")

# Choose flop images and combine them
for (i in 1:5) {
  titel = df$titel[i]
  rating = df$diff[i]
  s = if (str_length(df$titel[i]) > 20) 11 else 18
  img = magick::image_read(path = df$image_link[i]) # Initialize image
  img = image_scale(image = img, geometry = "190") # Cut to...
  img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
  img = image_border(image = img, geometry = "0x45") # Add grey...
  img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
  img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
  img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
  img = image_annotate(img, paste0("(", rating, " Tage)"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
  img = image_border(image = img, color = "white", geometry = "0x30") # Add white border on top
  
  bottom_img = image_join(bottom_img, img)
}
bottom_img = image_append(bottom_img)
#bottom_img = image_annotate(image = bottom_img, text = "Flop", size = 20, gravity = "north")

comb_image = image_append(c(top_img, bottom_img), stack = T)
image_write(top_row, path = paste0("..\\results\\Längste Wartezeit", ".png"), format = "png")


##### Welche Filme werden wie gut bewertet, wenn man die Person berücksichtigt, die ihn vorgeschlagen hat? ####
df = 
  left_join(schlaegt_vor, stimmt_fuer, by = "id") %>% 
  filter(sieger & vorschlagsdatum == stimmdatum) %>% 
  rename(vorschlagender = vorname.x, abstimmer = vorname.y) %>% 
  arrange(id, vorschlagender) %>% 
  distinct(vorschlagender, id, .keep_all = T) %>% 
  select(vorschlagender, id) %>% 
  left_join(bewertet, by = "id") %>% 
  rename(bewerter = vorname) %>% 
  group_by(id) %>% 
  summarise(avg = round(mean(wertung), 2)) %>% 
  arrange(desc(avg)) %>% 
  slice(1:10) %>% 
  left_join(film, by = "id") %>% 
  select(titel, avg, image_link) %>% 
  arrange(avg)

top_img = c()
bottom_img = c()
# Choose top images and combine them
for (i in 6:10) {
  titel = df$titel[i]
  rating = df$avg[i]
  s = if (str_length(df$titel[i]) > 20) 11 else 18
  img = magick::image_read(path = df$image_link[i]) # Initialize image
  img = image_scale(image = img, geometry = "190") # Cut to...
  img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
  img = image_border(image = img, geometry = "0x45") # Add grey...
  img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
  img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
  img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
  img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
  img = image_border(image = img, color = "white", geometry = "0x50") # Add white border on top + bottom
  img = image_crop(image = img, geometry = paste0("0x390")) # Remove white border from bottom
  
  top_img = image_join(top_img, img)
}
top_img = image_append(top_img)
#top_img = image_annotate(image = top_img, text = paste0("Top"), size = 20, gravity = "north")

# Choose flop images and combine them
for (i in 1:5) {
  titel = df$titel[i]
  rating = df$avg[i]
  s = if (str_length(df$titel[i]) > 20) 11 else 18
  img = magick::image_read(path = df$image_link[i]) # Initialize image
  img = image_scale(image = img, geometry = "190") # Cut to...
  img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
  img = image_border(image = img, geometry = "0x45") # Add grey...
  img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
  img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
  img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
  img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
  img = image_border(image = img, color = "white", geometry = "0x30") # Add white border on top
  
  bottom_img = image_join(bottom_img, img)
}
bottom_img = image_append(bottom_img)
comb_image = image_append(c(top_img, bottom_img), stack = T)

image_write(comb_image, path = paste0("..\\results\\Beste Filmwertungen (mit Vorschlagenden)", ".png"), format = "png")


##### Welche Filme werden wie gut bewertet, wenn man die Person ignoriert, die ihn vorgeschlagen hat? ####
df = 
  left_join(schlaegt_vor, stimmt_fuer, by = "id") %>% 
  filter(sieger & vorschlagsdatum == stimmdatum) %>% 
  rename(vorschlagender = vorname.x, abstimmer = vorname.y) %>% 
  arrange(id, vorschlagender) %>% 
  distinct(vorschlagender, id, .keep_all = T) %>% 
  select(vorschlagender, id) %>% 
  left_join(bewertet, by = "id") %>% 
  rename(bewerter = vorname) %>% 
  filter(vorschlagender != bewerter) %>% 
  group_by(id) %>% 
  summarise(avg = round(mean(wertung), 2)) %>% 
  arrange(desc(avg)) %>% 
  slice(1:10) %>% 
  left_join(film, by = "id") %>% 
  select(titel, avg, image_link) %>% 
  arrange(avg)

top_img = c()
bottom_img = c()
# Choose top images and combine them
for (i in 6:10) {
  titel = df$titel[i]
  rating = df$avg[i]
  s = if (str_length(df$titel[i]) > 20) 11 else 18
  img = magick::image_read(path = df$image_link[i]) # Initialize image
  img = image_scale(image = img, geometry = "190") # Cut to...
  img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
  img = image_border(image = img, geometry = "0x45") # Add grey...
  img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
  img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
  img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
  img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
  img = image_border(image = img, color = "white", geometry = "0x50") # Add white border on top + bottom
  img = image_crop(image = img, geometry = paste0("0x390")) # Remove white border from bottom
  
  top_img = image_join(top_img, img)
}
top_img = image_append(top_img)
#top_img = image_annotate(image = top_img, text = paste0("Top"), size = 20, gravity = "north")

# Choose flop images and combine them
for (i in 1:5) {
  titel = df$titel[i]
  rating = df$avg[i]
  s = if (str_length(df$titel[i]) > 20) 11 else 18
  img = magick::image_read(path = df$image_link[i]) # Initialize image
  img = image_scale(image = img, geometry = "190") # Cut to...
  img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
  img = image_border(image = img, geometry = "0x45") # Add grey...
  img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
  img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
  img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
  img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
  img = image_border(image = img, color = "white", geometry = "0x30") # Add white border on top
  
  bottom_img = image_join(bottom_img, img)
}
bottom_img = image_append(bottom_img)
comb_image = image_append(c(top_img, bottom_img), stack = T)

image_write(comb_image, path = paste0("..\\results\\Beste Filmwertungen (ohne Vorschlagenden)", ".png"), format = "png")


# plt = ggplot(data = df) +
#   geom_bar(mapping = aes(x = reorder(titel, avg), 
#                          y = avg
#   ),
#   stat = "identity") +
#   geom_text(aes(x = titel,
#                 y = 0,
#                 label = paste0(" ", titel, " (", avg, ")")),
#             angle = 90,
#             size = 3.5,
#             color = "white",
#             hjust = 0) +
#   labs(title = "Beste Filme (ohne Wertung des Vorschlagenden)",
#        x = "Titel",
#        y = "Durchschnittliche Wertung") +
#   theme(plot.title = element_text(hjust = 0.5, size = 12),  
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank()
#   )
# 
# plt
# 
# ggsave(filename = "Beste Filmwertungen (ohne Vorschlagenden).png", 
#        plot = plt, 
#        path = "..\\results", 
#        width = 1500, 
#        height = 1188, 
#        units = "px")


##### Wie beeinflusst welches Genre die Wertung der einzelnen Teilnehmer (Regressionkoeffizienten)? ####
df = 
  read.csv("../results/predictedRatings.csv") %>% 
  tibble()

minRange = -10
maxRange = 10

lst = list()
for (candidate in distinct(df, vorname) %>% pull()) {
  tmp = df %>% filter(vorname == candidate)
  subplt = 
    ggplot(
      data = tmp
      ) + 
    geom_bar(
      mapping = aes(
        x = genre, 
        y = Coefficient
      ),
      stat = "identity"
    ) +
    # geom_text(
    #   aes(
    #     x = genre,
    #     y = 0, 
    #     label = paste0(Coefficient)
    #   ),
    #   size = 5,
    #   color = "white",
    #   hjust = 0.5
    # ) +
    labs(title = candidate,
         y = "Koeffizient") +
    theme(plot.title = element_text(hjust = 0.5, size = 12),  
          axis.title = element_blank()
    ) +
    ylim(minRange, maxRange)
  
  lst[[length(lst) + 1]] = subplt
}

plt = plot_grid(plotlist = lst, ncol = 1)

ggsave(filename = "Genrekoeffizienten.png", 
       plot = plt, 
       path = "..\\results", 
       width = 4500, 
       height = 3564, 
       units = "px")



##### Wessen Bewertungen haben die höchste Korrelation mit den Ratings von imdb? ####
df = 
  left_join(bewertet, film, by = "id") %>% 
  select(vorname, id, wertung, imdb_rating) %>% 
  #filter(vorname == "Timo")
  group_by(vorname) %>% 
  summarise(correlation = round(cor(x = wertung, y = imdb_rating, method = "pearson"), 2),
            significance = cor.test(wertung, imdb_rating, method = "pearson")[["p.value"]],
            low_conf = cor.test(wertung, imdb_rating, method = "pearson")[["conf.int"]][1],
            high_conf = cor.test(wertung, imdb_rating, method = "pearson")[["conf.int"]][2]) %>% 
  mutate(direction = if_else(correlation < 0, 270, 90))


plt = ggplot(data = df) +
  geom_bar(mapping = aes(x = reorder(vorname, correlation), 
                         y = correlation
  ),
  stat = "identity") +
  geom_text(aes(x = vorname,
                y = 0,
                label = paste0(" ", vorname, " (", correlation, ")"),
                angle = direction),
            size = 3,
            color = "lightgrey",
            hjust = 0) +
  labs(title = "Korrelation mit imdb-ratings",
       x = "Name",
       y = "Korrelationskoeffizient") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

plt

ggsave(filename = "Korrelation mit imdb-Ratings.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")

##### Die Top 5 und Worst 5 Vorschläge von jedem (gem. Wertung jedes Teilnehmers) ####
df =
  left_join(vorschlagende, select(bewertet, !vorname), by = "id") %>% 
  select(id, vorname, wertung) %>% 
  group_by(id, vorname) %>% 
  summarise(avg = round(mean(wertung), 2),) %>% 
  left_join(select(film, id, titel, image_link), by = "id") %>% 
  filter(!is.na(avg))

for(name in unique(df$vorname)){
  top_img = c()
  flop_img = c()
  
  # Filter candidate
  tmp = 
    df %>% 
    filter(vorname == name) %>% 
    arrange(desc(avg))
  
  if(nrow(tmp) > 5) {
    # Choose top and flop rows
    n = nrow(tmp)
    top = tmp[1:5,] %>% 
      arrange(avg)
    flop = tmp[(n-4):n,] %>% 
      arrange(avg)
    
    
    # Choose top images and combine them
    for (i in 1:nrow(top)) {
      titel = top$titel[i]
      rating = top$avg[i]
      s = if (str_length(top$titel[i]) > 20) 11 else 18
      img = magick::image_read(path = top$image_link[i]) # Initialize image
      img = image_scale(image = img, geometry = "190") # Cut to...
      img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
      img = image_border(image = img, geometry = "0x45") # Add grey...
      img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
      img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
      img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
      img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
      img = image_border(image = img, color = "white", geometry = "0x50") # Add white border on top + bottom
      img = image_crop(image = img, geometry = paste0("0x390")) # Remove white border from bottom
      
      top_img = image_join(top_img, img)
    }
    top_img = image_append(top_img)
    top_img = image_annotate(image = top_img, text = paste0(name, "\n", "Top"), size = 20, gravity = "north")
    
    # Choose flop images and combine them
    for (i in 1:nrow(flop)) {
      titel = flop$titel[i]
      rating = flop$avg[i]
      s = if (str_length(flop$titel[i]) > 20) 11 else 18
      img = magick::image_read(path = flop$image_link[i]) # Initialize image
      img = image_scale(image = img, geometry = "190") # Cut to...
      img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
      img = image_border(image = img, geometry = "0x45") # Add grey...
      img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
      img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
      img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
      img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
      img = image_border(image = img, color = "white", geometry = "0x30") # Add white border on top
      
      flop_img = image_join(flop_img, img)
    }
    flop_img = image_append(flop_img)
    flop_img = image_annotate(image = flop_img, text = "Flop", size = 20, gravity = "north")
    
    comb_image = image_append(c(top_img, flop_img), stack = T)
    image_write(comb_image, path = paste0("..\\results\\Top-Flop-Vorschläge_", name, ".png"), format = "png")
  }
}




##### Histogram der Wertungen jedes Teilnehmers ####

nms = 
  bewertet %>% 
  distinct(vorname) %>% 
  arrange(vorname) %>% 
  pull()

l = list()

for (name in nms) {
  df = 
    bewertet %>% 
    filter(vorname == name) %>% 
    group_by(wertung) %>% 
    summarise(n = n()) %>% 
    mutate(ratio = round((n / sum(n)) * 100, 2))
  
  df = 
    left_join(tibble(wertung = 1:10), df, by = "wertung") %>% 
    replace(is.na(.), 0)
  
  plt = ggplot(data = df) +
    geom_bar(mapping = aes(x = wertung,
                           y = ratio),
             stat = "identity") +
    geom_text(aes(x = wertung,
                  y = ratio,
                  label = paste0(ratio, "%"),
                  angle = 90),
              size = 3,
              color = "black",
              hjust = 0) +
    labs(title = name,
         x = "Wertung",
         y = "Relative Häufigkeit") +
    scale_x_discrete(limits = as.character(c(1:10))) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)),
                       limits = c(0,35)) +
    theme(plot.title = element_text(hjust = 0.5, size = 12),  
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
  
  l[[length(l) + 1]] = plt
}

plt = grid.arrange(l[[1]], 
                   l[[2]], 
                   l[[3]], 
                   l[[4]], 
                   l[[5]], 
                   l[[6]], 
                   l[[7]], 
                   nrow = 2,
                   top = "Verteilungen der Wertungen")
plt

ggsave(filename = "Wertungsverteilung.png", 
       plot = plt, 
       path = "..\\results", 
       width = 3000, 
       height = 2376, 
       units = "px")

##### Die 5 längsten und 5 kürzesten Vorschläge eines jeden Teilnehmers ####
df =
  left_join(select(vorschlagende, vorname, id), 
            select(film, id, titel, laufzeit, image_link), by = "id")  %>% 
  filter(!is.na(laufzeit))

for(name in unique(df$vorname)){
  top_img = c()
  flop_img = c()
  
  # Filter candidate
  tmp = 
    df %>% 
    filter(vorname == name) %>% 
    arrange(desc(laufzeit))
  
  if(nrow(tmp) > 5) {
    # Choose top and flop rows
    n = nrow(tmp)
    top = tmp[1:5,] %>% 
      arrange(laufzeit)
    flop = tmp[(n-4):n,] %>% 
      arrange(laufzeit)
    
    
    # Choose top images and combine them
    for (i in 1:nrow(top)) {
      titel = top$titel[i]
      rating = top$laufzeit[i]
      s = if (str_length(top$titel[i]) > 20) 11 else 18
      img = magick::image_read(path = top$image_link[i]) # Initialize image
      img = image_scale(image = img, geometry = "190") # Cut to...
      img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
      img = image_border(image = img, geometry = "0x45") # Add grey...
      img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
      img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
      img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
      img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
      img = image_border(image = img, color = "white", geometry = "0x50") # Add white border on top + bottom
      img = image_crop(image = img, geometry = paste0("0x390")) # Remove white border from bottom
      
      top_img = image_join(top_img, img)
    }
    top_img = image_append(top_img)
    top_img = image_annotate(image = top_img, text = paste0(name, "\n", "Längste"), size = 20, gravity = "north")
    
    # Choose flop images and combine them
    for (i in 1:nrow(flop)) {
      titel = flop$titel[i]
      rating = flop$laufzeit[i]
      s = if (str_length(flop$titel[i]) > 20) 11 else 18
      img = magick::image_read(path = flop$image_link[i]) # Initialize image
      img = image_scale(image = img, geometry = "190") # Cut to...
      img = image_crop(image = img, geometry = paste0("0x281+0")) # ...correct size
      img = image_border(image = img, geometry = "0x45") # Add grey...
      img = image_crop(image = img, geometry = paste0("0x326+0+45")) # ...border on bottom
      img = image_border(image = img, color = "white", geometry = "5x0") # Add white border on sides
      img = image_annotate(img, paste0(titel), size = s, gravity = "south", color = "black", location = "+0+24") # Annotate
      img = image_annotate(img, paste0("(", rating, ")"), size = 18, gravity = "south", color = "black", location = "+0+2") # Annotate
      img = image_border(image = img, color = "white", geometry = "0x30") # Add white border on top
      
      flop_img = image_join(flop_img, img)
    }
    flop_img = image_append(flop_img)
    flop_img = image_annotate(image = flop_img, text = "Kürzeste", size = 20, gravity = "north")
    
    comb_image = image_append(c(top_img, flop_img), stack = T)
    #print(comb_image)
    image_write(comb_image, path = paste0("..\\results\\Längste-Kürzeste-Vorschläge_", name, ".png"), format = "png")
  }
}
comb_image

#### Wer stimmt am häufigsten für den gleichen Film wie jemand anderes? Gibt es "Cluster"? ####
df =
  stimmt_fuer %>% 
  left_join(stimmt_fuer, by = c("id", "stimmdatum")) %>% 
  select(!contains("sieger.")) %>% 
  select(!contains("wahldurchgang.")) %>% 
  #filter(vorname.x != vorname.y) %>% 
  filter(vorname.x != "unknown" & vorname.y != "unknown") %>% 
  filter(vorname.x != "Nikita" & vorname.y != "Nikita") %>% 
  filter(vorname.x != "Marco" & vorname.y != "Marco") %>% 
  group_by(vorname.x, vorname.y) %>% 
  summarise(n = n()) %>% 
  group_by(vorname.x) %>% 
  mutate(ratio = round(n / sum(n), 2)) 

plt = ggplot(df, aes(vorname.x, vorname.y, fill= ratio)) + 
  geom_tile(show.legend = F) +
  geom_text(aes(
    label = paste0(round(ratio, 2), "%")),
    angle = 0,
    size = 3.5,
    #color = df$clr,
    hjust = 0.5) +
  scale_fill_distiller(palette = "Greys") +
  labs(title = "Wer wie oft für den gleichen Film stimmt wie jemand anderes",
       x = "Abstimmer",
       y = "Teilnehmer") +
  theme(plot.title = element_text(hjust = 0.5, size = 12), 
        axis.title = element_text(size = 5), 
        axis.text = element_text(size = 8)
  )

plt

ggsave(filename = "Einigkeit.png", 
       plot = plt, 
       path = "..\\results", 
       width = 1500, 
       height = 1188, 
       units = "px")
