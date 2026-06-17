library(tidyverse)
library(jsonlite)
library(readxl)
library(fuzzyjoin)

### Thesen

thesen_ordner <- "~/Dokumente/Datensätze/Voto Party Positions/data_in_vaa/thesen"

thesen_dateien <- list.files(thesen_ordner, pattern = "\\.xlsx?$", full.names = TRUE)

thesen_df <- map_df(thesen_dateien, ~ read_excel(.x) %>% mutate(source = basename(.x))) %>%
  select(1:9) %>%
  mutate(source = tools::file_path_sans_ext(source)) %>%
  separate(source, into = c("prefix", "gmd_name", "gmd_key"),
           sep = "_", remove = TRUE) %>%
  select(-prefix) %>%
  select(-Erläuterungen) %>%
  rename(statement = these_long) %>%
  mutate(across(where(is.character), str_squish)) %>%
  filter(!grepl("mayor", these_id)) %>% # Thesen zu den Stichwahlen entfernen
  mutate(statement = gsub(
    "Gehweg-Parken soll konsequent geahndet werden\\. Mouseover: Für Autos und Fahrräder\\.",
    "(Gehweg-Parken)[Von Autos und Fahrrädern.] soll konsequent geahndet werden.",
    statement
  ))
  
### Antworten

answer_folder <-  "~/Dokumente/Datensätze/Voto Party Positions/data_out_vaa/partypos"

answer_dateien <- list.files(answer_folder, pattern = "\\.json?$", full.names = TRUE)

answer_df <- map_df(answer_dateien, ~ fromJSON(.) %>% mutate(source = tools::file_path_sans_ext(basename(.x)))) %>%
  separate(source, into = c("prefix", "num", "gmd_name"),
           sep = "_", remove = TRUE) %>%
  select(-c("prefix", "num", "instance")) %>%
  relocate(c("gmd_name", "party_name"), .before = "statement") %>%
  filter(str_detect(tolower(party_name), "(afd|bsw|cdu|die partei|linke|fdp|gr(ü|u)ne|spd|volt)")) %>%
  mutate(
    party_name = case_when(
      str_detect(party_name, regex("gr(ü|u)ne|b'?90/\\s*die\\s*gr(ü|u)nen", ignore_case = TRUE)) ~ "GRÜNE",
      str_detect(party_name, regex("volt", ignore_case = TRUE)) ~ "VOLT",
      str_detect(party_name, regex("die\\s*linke", ignore_case = TRUE)) ~ "DIE LINKE",
      TRUE ~ party_name
    )
  ) %>%
  mutate(across(where(is.character), str_squish)) %>%
  filter(!grepl("bürgermeist", statement, ignore.case = T)) %>%
  mutate(statement = str_replace(statement, "\\(Tempo 30-Zonen\\)\\[[^]]*\\]", "Tempo 30-Zonen"))

### Merge

voto_kommwahl <- stringdist_left_join(answer_df, thesen_df, by = c("statement", "gmd_name"), method = "jw", max_dist = 0.15) %>%
  select(-c("statement.y", "gmd_name.y")) %>%
  rename(statement = statement.x) %>%
  rename(gmd_name = gmd_name.x) %>%
  select(-gmd_key) %>%
  rename(polarity = 7) %>%
  relocate(c("these_id", "these_politikfeld", "polarity", "these_title"), .before = statement) %>%
  mutate(these_title = gsub("Gernderstern", "Genderstern", these_title))

write_csv(voto_kommwahl, file = "voto_kommwahl.csv")

voto_kommwahl_bridge <- voto_kommwahl %>% filter(grepl("bridge", these_id)) %>% arrange(these_id)

write_csv(voto_kommwahl_bridge, file = "voto_kommwahl_bridge.csv")

