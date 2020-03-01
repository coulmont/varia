library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(ngram)

#dc <- read_csv("/Volumes/disque de sauvegarde/insee_deces.csv")
dc <- read_csv("~/Downloads/insee_deces.csv")

# variables dérivées
# ------------------
dc$nombre_prenoms <- sapply(dc$prenom, wordcount)
dc <- dc %>% mutate(jour_deces = wday(date_deces,label=TRUE,abbr=TRUE,week_start = 1, locale = "fr_FR"))
dc <- dc %>%
  mutate(annee_deces = year(date_deces)) %>%
  mutate(decennie_deces = 10 * (annee_deces%/%10))
dc <- dc %>% 
  mutate(annee_naissance = substr(date_naissance,1,4))
dc <- dc %>%
  mutate(age_deces = as.numeric(annee_deces)-as.numeric(annee_naissance))
dc <- dc %>%
  mutate(particules = grepl("\\b(D|DE|DES|DU)\\b",nom,ignore.case=T))



# jour du décès en fonction de l'âge au décès
# -------------------------------------------


# filtrage : on enleve quelques valeurs aberrantes
tmp <- dc %>%
  filter(annee_deces>1970 ) %>%
  filter(age_deces>(-0.1),age_deces<110) %>%
  mutate(age_deces = 10* floor(age_deces/10) ) %>%
  mutate(age_deces = ifelse(age_deces>80,80,age_deces)) %>%
  group_by(decennie_deces,jour_deces,date_deces,age_deces) %>%
  summarise(N=n())

# il faut pondérer : les années n'ont pas le même nombre de lundis...
tmp <- tmp %>% group_by(decennie_deces,jour_deces,age_deces) %>%
  summarize(N=sum(N),
            nombre_jours = n()) %>%
  group_by(decennie_deces,age_deces) %>%
  mutate(nb_jours_maxi = max(nombre_jours))

tmp <- tmp %>%
  mutate(N_pondere = N * (nb_jours_maxi / nombre_jours))

tmp <- tmp %>% 
  group_by(decennie_deces,age_deces) %>%
  mutate(total = sum(N_pondere)) %>%
  ungroup() %>%
  mutate(p_decennal = N_pondere/total)

p <- tmp %>%
  mutate(decennie_deces= as.factor(decennie_deces) ) %>%
  mutate(age_deces = as.factor(age_deces)) %>%
  mutate(age_deces = fct_recode(age_deces,"0 - 9 ans"="0","10 - 19 ans"="10",
                                            "20 - 29 ans"="20", "30 - 39 ans"="30",
                                "40 - 49 ans"="40","50 - 59 ans"="50","60 - 69 ans"="60",
                                "70 - 79 ans"="70","80 ans et plus"="80")) %>%
  ggplot(aes(jour_deces,p_decennal,group=decennie_deces,color=decennie_deces)) +
  geom_line(size=.8) +
  geom_point(color="white",size=3) +
  geom_point() +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  facet_wrap(~age_deces) +
  labs(title = "Le jour de la mort, en fonction de l'âge au décès",
       subtitle = "Proportion des décès qui ont lieu tel jour de la semaine, en fonction de l'âge",
       caption = "Source : Insee, Fichier des décès. Calcul : B. Coulmont.",
       y="",x="",color="Période") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), plot_title_margin=5 , subtitle_margin=5) 

g <- ggplotGrob(p)
g$layout$l[g$layout$name == "title"] <- 2
g$layout$l[g$layout$name == "subtitle"] <- 2
grid::grid.draw(g)


ggsave("~/Desktop/mourir.png",plot=grid::grid.draw(g),width=110/13,height=110/13,dpi=130)
