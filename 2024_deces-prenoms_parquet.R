# 2024
# importation données décès
# à partir de l'agrégation des décès
# https://www.data.gouv.fr/fr/datasets/agregation-des-fichiers-des-personnes-decedees/
# code version brouillon

library(tidyverse)
library(duckdb)
library(glue)
library(arrow)
library(ggrepel)

dossier_donnees_externes <- '~/Documents/data/insee_deces/inst/extdata'
dossier_donnees_deces <- file.path(dossier_donnees_externes, 'deces')
dossier_cible_donnees <-    '~/Documents/data/insee_deces/data_deces'


# Créer les données
if(!dir.exists(dossier_donnees_externes)) dir.create(dossier_donnees_externes)
if(!dir.exists(dossier_donnees_deces)) dir.create(dossier_donnees_deces)
if(!dir.exists(dossier_cible_donnees)) dir.create(dossier_cible_donnees)

# Liste des URLs des fichiers de patients décédés
urls_listes_deces <- "https://object.files.data.gouv.fr/data-pipeline-open/deces/deces.parquet"

dl_fichier <- function(
    url_dl,
    dossier_cible = dossier_donnees_deces 
) {
  
  nom_fichier <- basename(url_dl)
  chemin_fichier <- file.path(dossier_cible, nom_fichier)
  
  if(!file.exists(chemin_fichier)) {
    message("Téléchargement via l'url ", url_dl)
    curl::curl_download(
      url = url_dl, 
      destfile = chemin_fichier, 
      quiet = FALSE
    )
    message("Téléchargement terminé. Taille : ", file.size(chemin_fichier), " octets")
  } else {message('Fichier déjà présent')}
  
  chemin_fichier
}

chemins_fichiers_deces <- lapply(urls_listes_deces, dl_fichier)

# options(arrow.skip_nul = TRUE)
# deces <- read_parquet("~/Documents/data/insee_deces/inst/extdata/deces/deces.parquet")



###
# insertion sur la base duckdb
# créer la base
# vérification de l'état des bases
setwd("~/Documents/data/insee_deces/duckdb/")
dbdir <- "~/Documents/data/insee_deces/duckdb/base_deces.db"
con = dbConnect(duckdb::duckdb(), dbdir=dbdir, read_only=FALSE)
#con <- DBI::dbConnect(duckdb::duckdb(), dbdir)
DBI::dbListTables(con) 


chemin_donnees <- "~/Documents/data/insee_deces/inst/extdata/deces/" # dossier où se trouve le .parquet
chemin_duckdb  <- "~/Documents/data/insee_deces/duckdb/base_deces.db"
path_data_sql <- DBI::SQL(chemin_donnees)
dbExecute(
  con,
  glue_sql(
    'CREATE OR REPLACE VIEW insee_dc_parquet AS ',
    'SELECT * FROM read_parquet("{path_data_sql}/deces.parquet")',
    .con=con
  ) )




dc <- tbl(con,from="insee_dc_parquet")


# fichier des prénoms 2024
# ------------------------

fpn <- read_csv2("~/Dropbox/data/prenoms/prenoms_2024/nat2023.csv", locale=locale(encoding = "utf8"), na="" )
# il faut ensuite enlever tous les caractères accentués
fpn <- fpn %>% mutate(preusuel = stringi::stri_trans_general(preusuel, "Latin-ASCII"))

# probleme des prénoms accentués : faire la somme
fpn <- fpn %>% group_by(sexe,annais,preusuel) %>%
  summarize(nombre = sum(nombre))
fpn <- fpn %>% mutate(taille=nchar(preusuel)) # taille des prénoms

# déterminer le rang des prénoms, en ne considérant pas les _PRENOMS_RARES
# rang par sexe
fpn <- fpn %>%  mutate(type= (preusuel=="_PRENOMS_RARES")) %>%
  group_by(type,annais,sexe) %>%
  mutate(rang = rank(-nombre,ties.method = "random") ) %>%
  ungroup() %>%
  mutate( rang = case_when(preusuel=="_PRENOMS_RARES" ~ as.integer(25000),
                           TRUE ~ rang ) ) %>%
  group_by(annais,sexe) %>% arrange(rang) %>%
  mutate(somme_cum=cumsum(nombre),   # somme cumulée)
         total_cum=sum(nombre)) %>%
  mutate(p_cum=somme_cum/total_cum,
         p=nombre/sum(nombre)) %>%
  ungroup()

# rang pour les 2 sexes
fpn <- fpn %>%  mutate(type= (preusuel=="_PRENOMS_RARES")) %>%
  group_by(type,annais) %>%
  mutate(rang2s = rank(-nombre,ties.method = "random") ) %>%
  ungroup() %>%
  mutate( rang2s = case_when(preusuel=="_PRENOMS_RARES" ~ as.integer(25000),
                             TRUE ~ rang2s ) ) %>%
  group_by(annais) %>% arrange(rang2s) %>% #### ATTENTION c'est la somme cumulée à partir du rang
  mutate(somme_cum2s=cumsum(nombre),   # somme cumulée)
         total_cum2s=sum(nombre)) %>%
  mutate(p_cum2s=somme_cum2s/total_cum2s,
         p2s=nombre/sum(nombre)) %>%
  ungroup()


fpn$annais <- as.numeric(as.character(fpn$annais))

# ----------
# comparaison prénoms / décès

annee_etudiee <- 1970
rang_max_etudie <- 15
prenoms_70 <- fpn %>%
  mutate(sexe = ifelse(sexe==1,"M","F")) %>%
  filter(annais==annee_etudiee) %>%
  filter(rang < rang_max_etudie) %>%
  arrange(sexe,nombre) %>%
  select(preusuel,sexe,nombre) %>%
  mutate(indicateur = paste(preusuel,sexe))

naissances_70 <- dc %>%
  mutate(annee_naissance = str_sub(date_naissance,1,4)) %>%
  filter(annee_naissance == annee_etudiee) %>%
  collect()
  
  
  # filter(naissance_annee==annee_etudiee) %>%
  # #  select(naissance_annee) %>%
  # #  head() %>%
  # collect()

deces_70 <- naissances_70 %>%
  select(prenoms,sexe,date_deces,pays_naissance) %>%
  # on enlève les personnes nées hors de France
  filter(pays_naissance %in% c("FRANCE METROPOLITAINE","GUADELOUPE","LA REUNION","MARTINIQUE","REUNION","GUYANE","REUNION(LA)")) %>%
  # on détecte le premier prénom
  mutate(prenom = str_remove_all(prenoms,",[A-Z-]*")) %>%
  mutate(keep = paste(prenom,sexe) %in% prenoms_70$indicateur) %>%
  filter(keep==TRUE)

deces_70 <- deces_70 %>%
  mutate(deces_annee = as.numeric(str_sub(date_deces,1,4))) %>%
  mutate(deces_annee = deces_annee+1) %>%
  group_by(deces_annee,sexe,prenom) %>%
  summarize(N=n()) %>%
  group_by(sexe,prenom) %>%
  arrange(deces_annee) %>%
  mutate(total_decedes = cumsum(N)) %>%
  ungroup() 
#  mutate(sexe = as.numeric(sexe))

initial_70 <- deces_70 %>%
  filter(deces_annee == annee_etudiee+1) %>%
  mutate(deces_annee = annee_etudiee) %>%
  mutate(N=0,
         total_decedes = 0)

deces_70 <- initial_70 %>%
  rbind(deces_70)

deces_comparaison <- deces_70 %>%
  left_join(prenoms_70, by = c("sexe","prenom"="preusuel"))

deces_comparaison <- deces_comparaison %>%
  mutate(vivants = nombre-total_decedes) %>%
  mutate(p_vivants = vivants/nombre) %>%
  mutate(sexe = ifelse(sexe==1,"Hommes","Femmes")) %>%
  mutate(couleur = case_when(prenom %in% c("FRANCK","KARINE") ~ "firebrick1",
                             prenom %in% c("FLORENCE","OLIVIER") ~ "deepskyblue",
                             TRUE ~ "gray40")) %>%
  mutate(epaisseur = case_when(prenom %in% c("FRANCK","KARINE") ~ 1,
                               prenom %in% c("FLORENCE","OLIVIER") ~ 1,
                               TRUE ~ .2)) %>%
  mutate(prenom = str_to_title(prenom))


minmax <- deces_comparaison %>%
  filter(deces_annee==max(deces_annee)) %>%
  #group_by(sexe) %>%
  filter( (p_vivants == max(p_vivants) | p_vivants == min(p_vivants))) %>%
  select(prenom,p_vivants) %>%
  mutate(prenom = str_to_title(prenom),
         p_decedes = round(100-100*p_vivants,1)) %>%
  mutate(p_decedes = formatC(p_decedes,decimal.mark =",",drop0trailing=TRUE))

deces_comparaison %>%
  ggplot(aes(deces_annee,p_vivants, group = prenom, color=I(couleur))) +
  geom_line(aes(linewidth = I(epaisseur))) +
  # facet_wrap(~sexe, scales = "free") +
  geom_text_repel(data = deces_comparaison %>% filter(deces_annee == max(deces_annee)),
                  aes(label = prenom), adj=0, direction = "y",
                  size=2.5,segment.size=0,
                  fontface=2,
                  family = "Helvetica Neue",
                  box.padding = .01) +
  scale_x_continuous(expand = expansion(add = c(1,8))) +
  scale_y_continuous(labels = scales::percent_format(accuracy=.1),
                     expand = expansion(add=c(.01,.001))) +
  labs(title = glue("Proportion de personnes nées en {annee_etudiee} encore vivantes"),
       subtitle = glue("Par prénom de naissance, pour les {2*(rang_max_etudie-1)} prénoms les plus fréquents en {annee_etudiee}."),
       y = NULL, x = NULL,
       caption = glue("Lecture : {minmax %>% filter(p_vivants == min(p_vivants)) %>% .$p_decedes}% des {minmax %>% filter(p_vivants == min(p_vivants)) %>% .$prenom} nés en {annee_etudiee} sont déjà décédés. C'est le cas de {minmax %>% filter(p_vivants == max(p_vivants)) %>% .$p_decedes}% des {minmax %>% filter(p_vivants == max(p_vivants)) %>% .$prenom}.\nGraphique, calculs et erreurs : B. Coulmont.\nDonnées : Fichier des personnes décédées et Fichier des prénoms, Insee")) +
  # theme_ipsum() +
  hrbrthemes::theme_ipsum( plot_margin = margin(5, 5, 5, 5),
                           base_family = "Helvetica Neue",
                           plot_title_size = 14,
                           subtitle_size = 11,
                           base_size = 12,
                           plot_title_margin = 5,
                           subtitle_margin = 5,
                           axis_title_size = 12,
                           axis_text_size = 12) +
  theme(legend.position = "none",
        #text = element_text(family = "Helvetica Neue"),
        plot.background = element_rect(fill="white"),
        plot.title.position = "plot") 

res <- 200
ggsave(filename = "~/Desktop/deces_prenoms.png",width=1100/res,height=2400/res,dpi=res)


