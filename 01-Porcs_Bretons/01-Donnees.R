# Packages ----

library(tidyverse)
library(openxlsx)
library(data.table)

# Données ----

# Recensement agricole 2022 - en milliers de têtes
# Source : ministère en charge de l'Agriculture, Agreste, statistique agricole annuelle
RPA <-
  read.xlsx("https://www.insee.fr/fr/statistiques/fichier/2012795/TCRD_073.xlsx",
            startRow = 4,cols = c(1:6)) |> 
  rename(
    DEP = X1, DEP_LIB = X2
  ) |> 
  (\(.){slice(.,-nrow(.))})()


# Recensement de la population - populations légales de 2021
temp <- tempfile(fileext = ".zip")
download.file("https://www.insee.fr/fr/statistiques/fichier/7739582/ensemble.zip",
              temp)
RP <-
  fread(cmd = paste0("unzip -p ",temp," donnees_departements.csv")); rm(temp)


# Mise en forme
# On rapporte la population en milliers pour le RP / On ne garde que les départements pour le RPA
# On donne une population artificielle pour Mayotte
donnees <-
  full_join(
    select(RP, DEP, PTOT) |> mutate(PTOT = PTOT / 1000),
    select(RPA, -c("DEP_LIB","Caprins")) |> filter(str_starts(DEP,"[0-9]")),
    by = "DEP"
  ) |> 
  mutate(PTOT = ifelse(is.na(PTOT),1000,PTOT))

rm(list=ls()[ls() != "donnees"])
