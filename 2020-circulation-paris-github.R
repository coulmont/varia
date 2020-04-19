# évolution de la circulation routière (voitures, camions)
# à paris en 2020

# packages
library(tidyverse)
#library(skim)
library(gganimate)
library(lubridate)
library(sf)
library(classInt)
library(hrbrthemes)

# données
# comptages-routiers-permanents : https://opendata.paris.fr/explore/dataset/comptages-routiers-permanents/information/?disjunctive.libelle&disjunctive.libelle_nd_amont&disjunctive.libelle_nd_aval&disjunctive.etat_trafic
# du 1er mars 2020 à la date limite (ici 1 avril 2020)
circulation <- read_csv2("~/Downloads/comptages-routiers-permanents.csv")
# référentiel : sur opendata.paris.fr
referentiel <- read_sf("~/Downloads/referentiel-comptages-routiers/referentiel-comptages-routiers.shp")

# frontière des arrondissements
paris_arr <- read_sf("~/Dropbox/data/arrondissements/",
                     layer="parisarr")
paris_arr <- sf::st_transform(paris_arr, "+init=epsg:2154")


# -- quelles rues ont vu leur circulation chuter le plus ?
# --
# --

circulation_xy <- circulation %>%
  filter(!is.na(q)) %>% # on enlève les compteurs sans données
  mutate(heure = hour(t_1h),
         jour = floor_date(t_1h,unit="day"),
         jour_semaine = weekdays(t_1h)) %>%
  #  select(heure,jour,jour_semaine) %>%
  # découpage en deux périodes
  mutate(periode = case_when(t_1h < ymd("2020-03-16") & jour_semaine %in% c("Lundi","Mardi","Mercredi","Jeudi","Vendredi") ~ "avant-confinement",
                             t_1h > ymd("2020-03-18") & jour_semaine %in% c("Lundi","Mardi","Mercredi","Jeudi","Vendredi") ~ "après-confinement",
                             TRUE ~ "autre")) %>%
  filter(periode %in% c("après-confinement","avant-confinement")) %>%
  select(iu_ac,q,periode,heure) %>%
  group_by(iu_ac,periode,heure) %>%
  # N : nombre de relevés distincts (normalement un par heure, sauf panne)
  summarize(N=n(),
            q = mean(q, na.rm=T)) %>%
  group_by(iu_ac,heure) %>%
  # on calcule un ratio "après"/"avant"
  summarize(N = mean(N),
         ratio = first(q)/last(q)) %>%
  group_by(iu_ac) %>%
  summarize(total = sum(N),
         ratio = mean(ratio)) %>%
  # rendre quasi transparents les compteurs qui n'ont pas été tout le temps opérationnels
  mutate(alpha_bin = case_when(total < 100 ~ .1,
                               TRUE ~ 1))

# jointure avec le référentiel géographique
circulation_paris <- referentiel %>%
  left_join(circulation_xy, by = "iu_ac")

# découpage en classes, ici avec Fisher
bornes <- classIntervals(circulation_paris$ratio,n=5,style="fisher")$brks

# suite du découpage / discrétisation
circulation_paris <- circulation_paris %>%
  mutate(ratio_bin = as.factor(cut(ratio,  bornes) ) )

# simplifier la discrétisation (vérifier)
levels(circulation_paris$ratio_bin) = c("7 fois moins","4 fois moins","3 fois moins","2,5 fois moins","1,25 fois moins")

# ne garder qu'une partie des données
# celles qui sont produites par des compteurs en fonction
circulation_paris <- circulation_paris %>% filter(total>200) %>% filter(!is.na(ratio_bin))

# cartographie
paris_arr %>% 
  ggplot() +             # on commence par tracer les arrondissements
  geom_sf(fill="#eeeeee55") +
  # puis les routes à compteur
  geom_sf(data=circulation_paris %>% filter(total>200) %>% filter(!is.na(ratio_bin)),
          aes(color=ratio_bin, alpha = I(alpha_bin)),
          size=2,
          lineend="round") +
  # palette de couleurs, ici viridis
  scale_color_viridis_d(option="C") +
#  scale_color_brewer(palette="RdYlGn",direction=-1) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(color = "Diminution",
       title = "Paris : Diminution de la circulation depuis le début du confinement",
       subtitle = "Comparaison des jours de semaine, avant le 16 mars, puis après le 17 mars 2020",
       caption = "Données : opendata.paris.fr -- Calculs et erreurs : B. Coulmont.\nSegments transparents quand le nombre de comptage est limité.") +
  theme_ipsum(base_family = "Helvetica",
              plot_margin = margin(10, 10, 10, 10),
              base_size = 14) +
  theme(legend.position = c(.9,.8),
        #panel.grid = element_blank(),
        #panel.border = element_rect(fill=NA,color="#ffffff00"),
        panel.background = element_rect(fill="white",color="white"))

ggsave("~/Desktop/circulation_paris_confinement.png",width=10,height=7,dpi=100)


