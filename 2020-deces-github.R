library(tidyverse)
library(lubridate)
#library(ngram)
library(hrbrthemes)

# données en provenance de
# data.gouv.fr : fichier des personnes décédées
# insee.fr : fichier provisoire des personnes décédées : https://www.insee.fr/fr/information/4470857
# données agrégées par jour
load("dc_2020.Rdata")


# Bisextilisation -------------------------------------
# pour superposer les années l'une sur l'autre, il faut
# créer des faux 29 février 
# et leur affecter une valeur moyenne

ving_neuf_fevriers <- dc_jour %>%
  #  mutate(date_fictive = dmy(paste(jour,mois,"2020",sep="-"))) %>%
  filter((mois == 2 & jour %in% c(28,29))  | (mois == 3 & jour ==1)) %>%
  complete(annee, nesting(jour, mois)) %>% 
  group_by(annee) %>%
  arrange(annee,mois,jour) %>%
  mutate(N = case_when(is.na(N) ~  as.integer(round(.5*(lead(N) + lag(N)))) ,
                       TRUE ~ N) ) %>%
  group_by(jour) %>%
  mutate(maxi = max(N),
         mini = min(N)) 

# jointure du fichier des 29 février
dc_jour <- dc_jour %>%
  filter(! (mois == 2 & jour == 29)) %>%
  bind_rows(ving_neuf_fevriers %>% filter(mois == 2 & jour == 29))


# graphique 1

dc_jour %>% 
  ungroup() %>%
  # la date fictive, c'est une "fausse" année 2020 qui sert d'abscisse
  mutate(date_fictive = dmy(paste(jour,mois,"2020",sep="-"))) %>% 
  filter(annee<2020) %>%
  ggplot(aes(date_fictive,N, group = annee, color = I(couleur))) +
  geom_line(color="black",alpha=.5,size=.1) +
  geom_ribbon(data = . %>% filter(annee == 2016) ,
              aes(ymin = mini, ymax = maxi), alpha=.2, color=NA) +
  geom_line(data = dc_insee_jour, aes(date_fictive, N),
            color= "firebrick1") + 
  geom_line(data = . %>% group_by(date_fictive) %>% 
              summarize(N=mean(N)) %>% ungroup() %>%
              mutate(annee=2010),
            aes(date_fictive,N),
            size=.5, color="black") +
  geom_line(data = . %>% filter(annee==2003),
            color="navyblue", size=.2) +
  # annotations
  annotate(geom="text",x= ymd("2020-08-15"), y = 3500, label = "Canicule de 2003", adj =0 ) +
  annotate(geom="text",x= ymd("2020-04-15"), y = 2500, 
           label = "2020\nchiffres provisoires", adj=0, color="firebrick1") +
  annotate(geom="text",x= ymd("2020-11-01"), y = 1250, label = "Moyenne\n2001-2019", adj=0, size=4) +
  geom_segment(data = data.frame(x=ymd("2020-10-30"), y=1250, 
                                 xend = ymd("2020-10-01"), yend = 1490, 
                                 annee=2010),
               aes(x=x,y=y,xend=xend,yend=yend), color = "black", 
               arrow = arrow(length = unit(0.1, "inches"), type="closed") ) +
  # finalisation du graphique
  scale_x_date(date_breaks = "2 months", date_labels = "%B", expand = c(0.01,0.01)) +
  labs(title = "Nombre quotidien de décès en France, 2001-2020",
       subtitle = "En rouge, l'année 2020, en gris les années 2001 à 2019. En bleu foncé : 2003",
       y="",x="",
       caption = "Sources : Fichier des décès sur data.gouv.fr et Fichier des décès sur insee.fr (24 avril 2020) | Graphique : B. Coulmont") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), 
              plot_title_margin=5 , 
              subtitle_margin=5,
              base_family = "Helvetica") +
  theme(plot.title.position="plot")

# enregistrement du graphique :
ggsave("~/Desktop/deces-2001-2020-blog.png",width=1100/120,height=700/120,dpi=120)
