# ressorts des tribunaux judiciaires
# fond de carte France avec les départements d'outre mer
# -------------------------------------------------------
# code pour Github

# inspiration https://rstudio-pubs-static.s3.amazonaws.com/543650_d382de059cea47cebecf1198eb551f7a.html


library(tidyverse)
library(sf)
library(readxl)

# fichier ADMIN-EXPRESS France entière
# source : https://geoservices.ign.fr/documentation/diffusion/telechargement-donnees-libres.html#admin-express
communes <- st_read("ADMIN-EXPRESS_2-4__SHP__FRA_2020-12-15/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2020-12-15/ADE_2-4_SHP_WGS84G_FRA/COMMUNE.shp")
# ressorts des tribunaux : 
# https://www.observatoire-des-territoires.gouv.fr/perimetre-des-ressorts-des-tribunaux-judiciaires
# géographie 2020
ressorts <- read_excel("com2020.xlsx",skip=4)



# joindre communes avec les ressorts

communes <- communes %>%
  left_join(ressorts, by = c("INSEE_COM"="codgeo")) 

# faire union des communes / ressorts


tj_shp <- communes %>%
  group_by(INSEE_REG,INSEE_DEP,tj,tj_libgeo) %>%
  summarize(N=n()) %>%
  ungroup()

tj_simple <- tj_shp %>% 
  rmapshaper::ms_simplify(keep = 0.1,
                          keep_shapes = TRUE)

tj_simple <- tj_simple %>%
  st_transform(crs= 3857)


# changer position des DOM

# Fonction pour déplacer une géométrie
place_geometry <- function(geometry, position, scale = 1) {
  # prend en entrée une géométrie existante : 'geometry'
  # déplace cette géométrie au point 'position'
  # par défaut, pas de changement d'échelle
  
  # Nouvelle géométrie
  output_geometry <- (geometry - st_centroid(geometry)) * scale + st_centroid(geometry) +
    # translation
    position
  
  # Ajouter le système de coordonnées
  st_crs(output_geometry) <- st_crs(geometry)
  return(output_geometry)
}

fm <- tj_simple %>% filter(nchar(as.character(INSEE_DEP))==2)
tj_100213 <- tj_simple %>% filter(tj == "100213") # basse terre
tj_100214 <- tj_simple %>% filter(tj == "100214") # Pointe à pitre
tj_100215 <- tj_simple %>% filter(tj == "100215") # Fort de France

tj_100217 <- tj_simple %>% filter(tj == "100217") # Saint denis réunion
tj_100218 <- tj_simple %>% filter(tj == "100218") # saint pierre reunion

tj_941809 <- tj_simple %>% filter(tj == "941809") # Mayotte mamoudzou

tj_100216 <- tj_simple %>% filter(tj == "100216") # Guyane

# position finale de la Guadeloupe que la carte de la métropole
position_tj_100213 <- c(st_bbox(fm)$xmin - st_bbox(tj_100213)$xmin  # correspondance des 'xmin'
                        - 150000,  # décalage axe X
                        st_bbox(fm)$ymin - st_bbox(tj_100213)$ymin  # correspondance des 'ymin' 
                        + 1.25 * 230000)  # décalage axe Y

position_tj_100214 <- c(st_bbox(fm)$xmin - st_bbox(tj_100214)$xmin  # correspondance des 'xmin'
                        - 110000,  # décalage axe X
                        st_bbox(fm)$ymin - st_bbox(tj_100214)$ymin  # correspondance des 'ymin' 
                        + 1.25 * 253500)  # décalage axe Y

# Modifier la géométrie de l'ojet 'sf' :
# on utilise la fonction précédente, en utilisant 
# la géométrie originale de la Martinique, et en la 
# déplaçant au nouveau point défini, avec un changement
# d'échelle.
tj_100213_n <- tj_100213 %>% 
  mutate(geometry = place_geometry(geometry = st_geometry(tj_100213),
                                   position = position_tj_100213,
                                   scale = 2.5))
tj_100214_n <- tj_100214 %>% 
  mutate(geometry = place_geometry(geometry = st_geometry(tj_100214),
                                   position = position_tj_100214,
                                   scale = 2.5))


# martinique

# position finale de la Martinique que la carte de la métropole
position_tj_100215 <- c(st_bbox(fm)$xmin - st_bbox(tj_100215)$xmin  # correspondance des 'xmin'
                        - 150000,  # décalage axe X
                        st_bbox(fm)$ymin - st_bbox(tj_100215)$ymin  # correspondance des 'ymin' 
                        + 100000)  # décalage axe Y
# Modifier la géométrie de l'ojet 'sf' :
# on utilise la fonction précédente, en utilisant 
# la géométrie originale de la Martinique, et en la 
# déplaçant au nouveau point défini, avec un changement
# d'échelle.
tj_100215_n <- tj_100215 %>% 
  mutate(geometry = place_geometry(geometry = st_geometry(tj_100215),
                                   position = position_tj_100215,
                                   scale = 2.5))


# Réunion
position_tj_100217 <- c(st_bbox(fm)$xmin - st_bbox(tj_100217)$xmin - 150000,  # position X
                        st_bbox(tj_100214_n)$ymax - st_bbox(tj_100217)$ymin + 1.25 * 130000)  # position Y

tj_100217_n <- tj_100217 %>% 
  mutate(geometry = place_geometry(geometry = st_geometry(tj_100217),
                                   position = position_tj_100217,
                                   scale = 2.5))

# Réunion
position_tj_100218 <- c(st_bbox(fm)$xmin - st_bbox(tj_100218)$xmin - 148000,  # position X
                        st_bbox(tj_100214_n)$ymax - st_bbox(tj_100218)$ymin + 1.25 * 90000)  # position Y

tj_100218_n <- tj_100218 %>% 
  mutate(geometry = place_geometry(geometry = st_geometry(tj_100218),
                                   position = position_tj_100218,
                                   scale = 2.5))

# Mayotte
position_tj_941809 <- c(st_bbox(fm)$xmin - st_bbox(tj_941809)$xmin - 150000,  # position X
                        st_bbox(tj_100217_n)$ymax - st_bbox(tj_941809)$ymin + 1.25 * 130000)  # position Y

tj_941809_n <- tj_941809 %>% 
  mutate(geometry = place_geometry(geometry = st_geometry(tj_941809),
                                   position = position_tj_941809,
                                   scale = 3))

# Guyane
position_tj_100216 <- c(st_bbox(fm)$xmin - st_bbox(tj_100216)$xmin - 260000,  # position X
                        st_bbox(tj_941809_n)$ymax - st_bbox(tj_100216)$ymin +  1.3 * 50000)  # position Y

tj_100216_n <- tj_100216 %>% 
  mutate(geometry = place_geometry(geometry = st_geometry(tj_100216),
                                   position = position_tj_100216,
                                   scale = 0.4))


all_sf <- plyr::rbind.fill(fm, tj_100214_n, tj_100213_n, tj_100215_n, tj_100217_n, tj_100218_n, tj_941809_n, tj_100216_n)

# Cartes avec échelle non respectée : modifier les cartes avec les géométries voulues
all_sf_unscale <- all_sf
all_sf_unscale$geometry[all_sf_unscale$NAME_2 == "100213"] <- tj_100213_n$geometry
all_sf_unscale$geometry[all_sf_unscale$NAME_2 == "100214"] <- tj_100214_n$geometry
all_sf_unscale$geometry[all_sf_unscale$NAME_2 == "100215"] <- tj_100215_n$geometry
all_sf_unscale$geometry[all_sf_unscale$NAME_2 == "100217"] <- tj_100217_n$geometry
all_sf_unscale$geometry[all_sf_unscale$NAME_2 == "100218"] <- tj_100218_n$geometry
all_sf_unscale$geometry[all_sf_unscale$NAME_2 == "941809"] <- tj_941809_n$geometry
all_sf_unscale$geometry[all_sf_unscale$NAME_2 == "100216"] <- tj_100216_n$geometry
# Transformer en objet 'sf'
all_sf_unscale <- st_as_sf(all_sf_unscale)

ressorts_tribunaux <- all_sf_unscale %>%
  mutate(annee_geographie = 2020)

save(ressorts_tribunaux, file = "carte_tribunaux_fra.RData")

#load("carte_tribunaux_fra.RData")


# une carte vide, pour vérifier que ça fonctionne
ressorts_tribunaux %>% 
  ggplot() +
  geom_sf() +
  coord_sf(datum = NA) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0))) +
  labs(title = "Ressorts des tribunaux judiciaires",
       caption = "échelle des DOM non respectée. Données : IGN, Ministère de la justice, Observatoire des territoires.") +
  theme(plot.title.position = "plot",
        panel.background = element_blank(),
        axis.title = element_blank())
#ggsave("~/Desktop/ressorts.png", width=1200/130, height=675/130,dpi=130)

