library(sf)
library(geojsonsf)
library(hrbrthemes)
library(lubridate)
library(tidyverse)
library(glue)
library(tidytext)
#https://www.data.gouv.fr/en/datasets/r/9eb02ac9-4bce-4fa8-a6f7-451c5b366f66

villes <- tribble(~ville,   ~lat,        ~lon,
                  "Paris",  48.862725,   2.287592,
                  "Lyon",   45.7578137,  4.8320114,
                  "Marseille",43.2961743,5.3699525,
                  "Rennes", 48.1113387, -1.6800198,
                  "Nantes", 47.2186371, -1.5541362)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
villes  <- st_as_sf(x = villes,                         
               coords = c("lon", "lat"),
               crs = projcrs)



# il faut télécharger l'annuaire de l'éducation
# https://www.data.gouv.fr/en/datasets/annuaire-de-leducation/
# et l'enregistrer dans le dossier /data

es <- geojson_sf("data/fr-en-annuaire-education.geojson")

es <- es %>%
  st_crop(xmin=-10, xmax=10, ymin=40, ymax=55)

es <- st_transform(es, crs = "+init=epsg:2154" ) 


grid <- sf::st_make_grid(es, cellsize = 15000, square = FALSE)

# Convert to sf object
grid = sf::st_as_sf(data.table::data.table(id_hex=1:length(grid), 
                                           geom=sf::st_as_text(grid)), wkt='geom')
st_crs(grid) <- st_crs(es)

jointure <- sf::st_join(grid, es, join=sf::st_contains) 

jointure_grid <- jointure %>%
  dplyr::group_by(id_hex) %>% 
  dplyr::summarise(sum_prive = sum(nombre_d_eleves[statut_public_prive=="Privé"], na.rm=T),
                   sum_total = sum(nombre_d_eleves, na.rm=T) ) %>%
  mutate(part_eleves_prives = sum_prive/sum_total)




jointure_grid %>%
  filter(!is.na(part_eleves_prives)) %>%
  mutate(part_eleves_prives = case_when(part_eleves_prives<.8 ~ part_eleves_prives,
                                     TRUE ~ .8)) %>%
  mutate(part_eleves_publics = 1-part_eleves_prives) %>%
  ggplot() +
  geom_sf(aes(fill=part_eleves_prives,color=part_eleves_prives)) +
  scale_fill_distiller(palette="Spectral",
                       labels = scales::percent_format(accuracy=1)) +
  scale_color_distiller(palette="Spectral",
                        labels = scales::percent_format(accuracy=1)) +
  scale_x_continuous(expand=expansion(mult=c(0,.5))) +
  scale_y_continuous(expand=expansion(mult=c(0,0))) +
  geom_sf(data=villes, fill = "white", color="gray",size=10,shape=1) +
  geom_sf_text(data=villes,aes(label=ville)) +
  coord_sf(datum=NA) +
  labs(title = "Part des élèves scolarisé.e.s en dehors de l'École publique",
       subtitle = "De la maternelle au post-bac lycée, France sans les DROM.",
       caption = "Source : Annuaire de l'éducation sur data.gouv.fr. Cartes, calculs, et erreurs : B. Coulmont",
       fill=NULL,color=NULL,x=NULL,y=NULL) +
  theme_ipsum(plot_margin = margin(5, 20, 5, 5),
              plot_title_margin = 5 ,
              subtitle_margin   = 5,
              strip_text_size   = 12,
              caption_size = 8,
              plot_title_size = 18,
              subtitle_size = 12,
              base_family = "Helvetica") +
  theme(legend.position = c(.7,.6),
        legend.key.size = unit(1.25, "cm"),
        legend.key.width = unit(0.5,"cm"),
        plot.background = element_rect(fill="white",color="white"))




# il faut télécharger le fichier des prénoms
# https://www.insee.fr/fr/statistiques/2540004?sommaire=4767262
# et l'enregistrer dans le dossier /data

fpn <- read_csv2("data/prenoms2020/nat2019.csv", locale=locale(encoding = "utf8"), na="" )

liste_prenoms <- fpn %>%
  group_by(preusuel) %>%
  summarize(N=n()) %>%
  mutate(preusuel = str_to_lower(preusuel)) %>%
  filter(nchar(preusuel)>2)

nom_etablissements <- es %>% 
  st_drop_geometry() %>%
  select(nom_etablissement,statut_public_prive) %>%
  filter(statut_public_prive %in% c("Public","Privé")) %>%
  unnest_tokens(mots,nom_etablissement) %>%
  group_by(statut_public_prive,mots) %>%
  summarize(N=n()) 
  
nom_etablissements %>% 
  semi_join(liste_prenoms %>% select(preusuel), by = c("mots"="preusuel")) %>%
  group_by(statut_public_prive) %>%
  mutate(rang = rank(-N, ties.method = "random")) %>%
  filter(rang<21) %>%
  select(-N) %>%
  mutate(mots = str_to_title(mots)) %>%
  group_by(mots) %>%
  mutate(nombre = n()) %>%
  ggplot(aes(statut_public_prive,rang,label=mots,color= as.factor(nombre))) +
  geom_text(size=6) +
  scale_y_reverse(breaks = c(1,5,10,15,20)) +
  scale_color_manual(values = c("firebrick1","deepskyblue")) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(face="bold",hjust=0,size=15),
        axis.text = element_text(face="bold",size=15),
        legend.position = "none",
        plot.title.position = "plot") +
  labs(y="Rang",x=NULL,
       title = "Les « prénoms » les plus fréquents dans les noms d'établissements scolaires",
       subtitle = "... en fonction du statut, public ou privé, de l'établissement. En bleu, les « prénoms » en commun.",
       caption = "Source : Annuaire de l'éducation. Erreurs et calculs : B. Coulmont")
