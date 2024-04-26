# Packages ----

library(tidyverse)
library(sf)
library(showtext)
library(ggtext)
library(camcorder)
library(magick)

# Données ----

source("01-Porcs_Bretons/01-Donnees.R",
       encoding = "UTF-8")


# Préparation ----

font_add_google("Atkinson Hyperlegible", "atkinson")
font_paths(Sys.getenv("font.path"))
font_add("montserratbold","Montserrat-Bold.ttf")
font_add("montserratmedium","Montserrat-Medium.ttf")
showtext_auto()
showtext_opts(dpi = 600)


# Fonds de cartes ----

sf.dep <-
  st_read("00-Fonds/departements.gpkg") |> 
  mutate(libelle = str_replace(libelle,"-","-<br>"))



# Différentes espèces ----

vaches <-
  left_join(sf.dep,
            mutate(donnees, indicatrice = ifelse(Bovins > PTOT,"Oui","Non"),
                   indicatrice = factor(indicatrice,c("Oui","Non"))),
            by = c("code" = "DEP"))

vaches.label <-
  vaches |> 
  filter(indicatrice == "Oui") |> 
  (\(.){
    cbind(st_drop_geometry(.) |> 
            mutate(libelle = paste0("<span style='font-family:montserratbold'>",libelle,"</span>"),
                   .keep = "used"),
          st_centroid(.) |> 
            st_coordinates() |> 
            data.frame())
  })()

porcs <-
  left_join(sf.dep,
            mutate(donnees, indicatrice = ifelse(Porcins > PTOT,"Oui","Non"),
                   indicatrice = factor(indicatrice,c("Oui","Non"))),
            by = c("code" = "DEP"))

porcs.label <-
  porcs |> 
  filter(indicatrice == "Oui") |> 
  (\(.){
    cbind(st_drop_geometry(.) |> 
            mutate(libelle = paste0("<span style='font-family:montserratbold'>",libelle,"</span>"),
                   .keep = "used"),
          st_centroid(.) |> 
            st_coordinates() |> 
            data.frame())
  })()

moutons <-
  left_join(sf.dep,
            mutate(donnees, indicatrice = ifelse(Ovins > PTOT,"Oui","Non"),
                   indicatrice = factor(indicatrice,c("Oui","Non"))),
            by = c("code" = "DEP"))

moutons.label <-
  moutons |> 
  filter(indicatrice == "Oui") |> 
  (\(.){
    cbind(st_drop_geometry(.) |> 
            mutate(libelle = paste0("<span style='font-family:montserratbold'>",libelle,"</span>"),
                   .keep = "used"),
          st_centroid(.) |> 
            st_coordinates() |> 
            data.frame())
  })()


couleurs <-
  c("vaches" = "#f0b76f",
    "porcs" = "#e85466",
    "moutons" = "#b65579")

# Cartes ----
camcorder::gg_record(width = 21, height = 29.7,
                     units = "cm", dpi = 600)



carte <- function(espece,
                  donnees.in,
                  donnees.label){
  

  couleur = couleurs[[which(names(couleurs) == espece)]]
  
  
  ggplot() +
    geom_sf(data = donnees.in,
            aes(fill = indicatrice),
            key_glyph = draw_key_rect) +
    geom_richtext(data = donnees.label,
                  aes(x= X, y = Y,
                      label = libelle),
                  size = 10*.36,
                  color = "black",
                  label.color = NA,
                  fill = NA) +
    annotate("richtext",
             -Inf, -Inf,
             label = "<span style='font-family:montserratbold'>Sources : </span><span style='font-family:montserratmedium'>Insee, Recensement de la population - Agreste, statistique agricole annuelle</span><br><span style='font-family:montserratbold'>Traitements : </span><span style='font-family:montserratmedium'>Jean Dupin - @JeanDup1n</span>",
             hjust = 0, vjust = -.25,
             label.color = NA,
             fill = NA,
             size = 10*.36,
             color = "black") +
    annotate("richtext",
             109225.97, 7310480,
             label = paste0("<span style='font-family:montserratbold'>Plus de <span style='color:",couleur,"'>",espece,"</span><br>ou plus d'habitants ?</span>"),
             label.color = NA,
             fill = NA,
             hjust = 0,
             size = 28*.36,
             color = "black") +
    # Scales
    scale_fill_manual(values = c(couleur,"#d7dad4"),
                      labels = c(paste0("Plus de ",espece), "Plus d'habitants")) +
    guides(fill = guide_legend(title.position = "top",
                               direction = "vertical",
                               override.aes = list(size = 5))) +
    labs(fill = "") +
    # Theme
    theme_void() +
    theme(legend.text = element_markdown(
      family = "montserratmedium",
      size = 18,
      color = "black"
    ),
    legend.key.spacing.y = unit(3,"mm"),
    legend.title = element_text(
      family = "atkinson",
      size = 18,
      face = "bold",
      color = "black",
      hjust = .5,
      margin = margin(b = 0,unit = "mm")
    ),
    legend.position = c(.2,.7),
    legend.key.width = unit(.2,"cm"),
    legend.key.height = unit(.2,"cm"),
    plot.background = element_rect(fill = "#F2F2F2",
                                   color = "#F2F2F2"),
    panel.background = element_rect(fill = "#F2F2F2",
                                    color = "#F2F2F2")) +
    coord_sf(st_bbox(sf.dep)[c(1,3)],
             st_bbox(sf.dep)[c(2,4)] + c(0,439488),
             crs = 2154)
  
}

temp <-
  tempfile(fileext = '.png')

p.vaches <-
  carte("vaches",
        vaches,
        vaches.label)
ggsave(temp,
       plot = p.vaches,
       width = 21, height = 29.7,
       units = "cm", dpi = 600)
system(
  paste0(Sys.getenv("ffmpeg"),
         " -y -i ",
         temp,
         ' -vf "crop=4960:6215:0:800"',
         " ",here::here("01-Porcs_Bretons","vaches.png")
  ))

p.porcs <-
  carte("porcs",
        porcs,
        porcs.label)
ggsave(temp,
       plot = p.porcs,
       width = 21, height = 29.7,
       units = "cm", dpi = 600)
system(
  paste0(Sys.getenv("ffmpeg"),
         " -y -i ",
         temp,
         ' -vf "crop=4960:6215:0:800"',
         " ",here::here("01-Porcs_Bretons","porcs.png")
  ))

p.moutons <-
  carte("moutons",
        moutons,
        moutons.label)
ggsave(temp,
       plot = p.moutons,
       width = 21, height = 29.7,
       units = "cm", dpi = 600)
system(
  paste0(Sys.getenv("ffmpeg"),
         " -y -i ",
         temp,
         ' -vf "crop=4960:6215:0:800"',
         " ",here::here("01-Porcs_Bretons","moutons.png")
         ))

image_write(
  image_append(c(image_read("01-Porcs_Bretons/moutons.png"),
                 image_read("01-Porcs_Bretons/porcs.png"),
                 image_read("01-Porcs_Bretons/vaches.png"))),
  path = "01-Porcs_Bretons/combinaison.png",
  format = "png")

# FIN ----