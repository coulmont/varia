# les prénoms des élus
# B. Coulmont

# code non corrigé

# ----- NOTES ------
# 1 : le fichier des élus est sur data.gouv.fr : je l'appelle ici rne_cm
# 2 : les fichiers des prénoms sont sur le site de l'insee


library(tidyverse)
library(lubridate)
library(stringi)
library(ggrepel)
library(hrbrthemes)
#library(ggridges)
library(patchwork)


# chargement des fichiers et premiers nettoyages
# ----------------------------------------------

fpn <- read_csv2("data/prenoms_2022/nat2021.csv", locale=locale(encoding = "utf8"), na="" )
# il faut ensuite enlever tous les caractères accentués
fpn <- fpn %>% mutate(preusuel = stri_trans_general(preusuel, "Latin-ASCII"))
# probleme des prénoms accentués : faire la somme
fpn <- fpn %>% group_by(sexe,annais,preusuel) %>%
  summarize(nombre = sum(nombre)) %>%
  ungroup()


fpd <- read_csv2("data/prenoms_2022/dpt2021.csv", locale=locale(encoding = "utf8"), na="" )
# il faut ensuite enlever tous les caractères accentués
fpd <- fpd %>% mutate(preusuel = stri_trans_general(preusuel, "Latin-ASCII"))
# probleme des prénoms accentués : faire la somme
fpd <- fpd %>% group_by(sexe,annais,preusuel,dpt) %>%
  summarize(nombre = sum(nombre)) %>%
  ungroup()

# Répertoire national des élus, conseils municipaux
rne_cm <-  read_delim("data/rne-cm.csv",delim="\t") %>% janitor::clean_names()

rne_cm <- rne_cm %>%
  mutate(prenom = stri_trans_general(prenom_de_lelu, "Latin-ASCII"),
         prenom = str_replace(prenom, "(?s) .*", "") )

# on enleve tous les prénoms composés, réduit à premiere composante
rne_cm <- rne_cm %>%
  mutate(prenom = stri_trans_general(prenom_de_lelu, "Latin-ASCII"),
         prenom = str_replace(prenom, "(?s) .*", ""),
         prenom = str_replace(prenom,  "\\-.*", "") )

fpd <- fpd %>%
  mutate(prenom = str_replace(preusuel,  "\\-.*", "") )

fpn <- fpn %>%
  mutate(prenom = str_replace(preusuel,  "\\-.*", "") )





# version finale
# avec tous les contrôles
# ---------------------------------------------------------------

distrib_elus <- rne_cm %>%
  mutate(annais = as.numeric(str_sub(date_de_naissance,7,10))) %>%
  mutate(code_du_departement = case_when(code_du_departement %in% c("92","93","94","95") & annais<1968 ~ "75",
                                         TRUE ~ code_du_departement) ) %>%
  mutate(code_du_departement = ifelse(code_du_departement %in% c("2A","2B"), "20",code_du_departement)) %>%
  filter(annais>1924,annais<2004) %>%
  group_by(annais,code_sexe,code_du_departement) %>%
  summarize(N_elus=n()) %>%
  ungroup()

distrib_prenoms <- fpd %>%
  mutate(annais = as.numeric(annais)) %>%
  mutate(code_sexe = ifelse(sexe==1,"M","F")) %>%
  group_by(annais,code_sexe,dpt) %>%
  summarize(N_prenoms=sum(nombre)) %>%
  ungroup()


ponderation <- distrib_elus %>%
  left_join(distrib_prenoms, by = c("code_sexe","annais",
                                    "code_du_departement"="dpt"))  %>%
  mutate(rapport = N_elus/N_prenoms)


prenoms_prevus <- fpd %>%
  mutate(annais = as.numeric(annais)) %>%
  mutate(code_sexe = ifelse(sexe==1,"M","F")) %>%
  mutate(code_du_departement = dpt) %>%
  left_join(ponderation %>% select(annais,code_sexe,code_du_departement,rapport),
            by = c("annais","code_sexe","code_du_departement")) %>%
  filter(!is.na(rapport)) %>%
  mutate(nombre_pondere = nombre*rapport) %>%
  group_by(prenom) %>%
  summarize(nombre_prevu = sum(nombre_pondere)) %>%
  ungroup() %>%
  mutate(prenom = str_to_title(prenom))


prenoms_elus <- rne_cm %>%
  group_by(prenom) %>%
  summarize(n_elus = n())

comparaison <- prenoms_elus %>%
  left_join(prenoms_prevus)


comparaison <- comparaison %>%
  mutate(rapport = n_elus/nombre_prevu)


prenoms_a_garder <- comparaison %>%
  filter(nombre_prevu>290|n_elus>290) %>%
  select(prenom)

res <- comparaison %>% mutate(controles = "Année, sexe, département")

p_5 <- comparaison %>%
  filter(nombre_prevu>290|n_elus>290) %>%
  mutate(taille = 5*abs(log(rapport)) + .3*log(n_elus) ) %>% 
  ggplot(aes(rapport,n_elus)) +
  geom_vline(xintercept = 1,size=.2,lty=2) +
  geom_segment(aes(y=n_elus,yend=nombre_prevu,x=rapport,xend=rapport,
                   color = rapport),
               size=1,
               arrow = arrow(angle = 30,ends="last",type="closed",
                             length = unit(.1,"cm"))) +
  ggrepel::geom_text_repel(aes(label=prenom,size=I(taille)),
                           #size=3,
                           box.padding = 0.001) +
  
  scale_colour_gradient2(midpoint = 1,low = "firebrick1",high="navyblue") +
  scale_x_log10(breaks = c(.5,.8,1,1.25),
                minor_breaks=NULL,
                labels = c("Moitié moins","1,25 fois moins","Autant","1,25 fois plus")) +
  scale_y_log10() +
  labs(title = "Quels prénoms sont plus (ou moins) fréquents que ce qui est prévu",
       subtitle = "Quand on contrôle par le département, l'année de naissance et le sexe",
       caption = "Données : Ministère de l'intérieur et Insee. Calculs et erreurs : B. Coulmont",
       x="Rapport (nombre d'élus)/(nombre attendu)",y="Nombre d'élus") +
  theme_ipsum( plot_margin = margin(0, 5, 0, 0),
               plot_title_size = 16,
               subtitle_size = 14,
               base_size = 10,
               axis_title_size = 12,
               axis_text_size = 8,
               strip_text_size = 10,
               base_family = "Helvetica")  +
  theme(legend.position="none",
    plot.background = element_rect(fill="white",color="white"),
    plot.title.position = "plot") 


#p_5




# version 1 : comparaison des prénoms, sans contrôles
# ---------------------------------------------------


prenoms_national <- fpn %>%
  mutate(prenom = str_to_title(prenom) ) %>%
  group_by(prenom) %>%
  summarize(n_nat=sum(nombre)) %>%
  ungroup() %>%
  mutate(p_nat=n_nat/sum(n_nat))

prenoms_elus <- rne_cm %>%
  group_by(prenom) %>%
  summarize(n_elus = n()) %>%
  ungroup() %>%
  mutate(p_elus = n_elus/sum(n_elus))


comparaison_1 <- prenoms_elus %>%
  left_join(prenoms_national) %>%
  mutate(rapport = p_elus/p_nat) %>%
  mutate(nombre_prevu = p_nat * nrow(rne_cm))

res <- bind_rows(res, comparaison_1 %>% mutate(controles = "Aucun"))

p_1 <- comparaison_1 %>%
  filter(prenom %in% c(prenoms_a_garder$prenom)) %>%
  mutate(taille = 2*abs(log(rapport)) + .3*log(n_elus) ) %>% 
  ggplot(aes(rapport,n_elus)) +
  geom_vline(xintercept = 1,size=.2,lty=2) +
  geom_segment(aes(y=n_elus,yend=nombre_prevu,x=rapport,xend=rapport,
                   color = rapport),
               size=.5,
               arrow = arrow(angle = 30,ends="last",type="closed",
                             length = unit(.1,"cm"))) +
  ggrepel::geom_text_repel(aes(label=prenom,size=I(taille)),
                           #size=3,
                           box.padding = 0.001) +
  
  scale_colour_gradient2(midpoint = 0,low = "firebrick1",high="navyblue",
                         trans="log") +
  scale_x_log10(breaks = c(.25,.5,.8,1,1.25,2,4),
                minor_breaks = NULL,
                labels = c("4 fois moins","Moitié moins","1,25 fois moins",
                           "Autant","1,25 fois plus","2 fois plus","4 fois plus")) +
  scale_y_log10() +
  labs(title = "Quels prénoms sont plus (ou moins) fréquents que ce qui est prévu",
       subtitle = "Quand on compare avec les naissances depuis 1900",
       caption = "Données : Ministère de l'intérieur et Insee. Calculs et erreurs : B. Coulmont",
       x="Rapport (nombre d'élus)/(nombre attendu)",y="Nombre d'élus") +
  theme_ipsum( plot_margin = margin(0, 5, 0, 0),
               plot_title_size = 16,
               subtitle_size = 14,
               base_size = 10,
               axis_title_size = 12,
               axis_text_size = 8,
               strip_text_size = 10,
               base_family = "Helvetica")  +
  theme(legend.position="none",
        plot.background = element_rect(fill="white",color="white"),
        plot.title.position = "plot") 



# res <- 130
# ggsave("~/Desktop/prenoms_elus_01.png",width=1100/res,height=700/res,dpi=res)




# version 2 : on contrôle par le sexe
# ------------------------------------------------

distrib_elus <- rne_cm %>%
  group_by(code_sexe) %>%
  summarize(N_elus=n())


distrib_prenoms <- fpn %>%
  mutate(code_sexe = ifelse(sexe==1,"M","F")) %>%
  group_by(code_sexe) %>%
  summarize(N_prenoms=sum(nombre))


ponderation <- distrib_elus %>%
  left_join(distrib_prenoms, by = c("code_sexe"))  %>%
  mutate(rapport = N_elus/N_prenoms)


prenoms_prevus <- fpn %>%
  mutate(code_sexe = ifelse(sexe==1,"M","F")) %>%
  #mutate(annais = as.numeric(annais)) %>%
  left_join(ponderation %>% select(code_sexe,rapport),
            by = c("code_sexe")) %>%
  filter(!is.na(rapport)) %>%
  mutate(nombre_pondere = nombre*rapport) %>%
  group_by(prenom) %>%
  summarize(nombre_prevu = sum(nombre_pondere)) %>%
  ungroup() %>%
  mutate(prenom = str_to_title(prenom))


prenoms_elus <- rne_cm %>%
  group_by(prenom) %>%
  summarize(n_elus = n())

comparaison_2 <- prenoms_elus %>%
  left_join(prenoms_prevus)


comparaison_2 <- comparaison_2 %>%
  mutate(rapport = n_elus/nombre_prevu)


res <- bind_rows(res, comparaison_2 %>% mutate(controles = "Sexe"))

p_2 <- comparaison_2 %>%
  filter(prenom %in% c(prenoms_a_garder$prenom)) %>%
  mutate(taille = 2*abs(log(rapport)) + .3*log(n_elus) ) %>% 
  ggplot(aes(rapport,n_elus)) +
  geom_vline(xintercept = 1,size=.2,lty=2) +
  geom_segment(aes(y=n_elus,yend=nombre_prevu,x=rapport,xend=rapport,
                   color = rapport),
               size=.5,
               arrow = arrow(angle = 30,ends="last",type="closed",
                             length = unit(.1,"cm"))) +
  ggrepel::geom_text_repel(aes(label=prenom,size=I(taille)),
                           #size=3,
                           box.padding = 0.001) +
  
  scale_colour_gradient2(midpoint = 0,low = "firebrick1",high="navyblue",
                         trans="log") +
  scale_x_log10(breaks = c(.25,.5,.8,1,1.25,2,4),
                minor_breaks = NULL,
                labels = c("4 fois moins","Moitié moins","1,25 fois moins",
                           "Autant","1,25 fois plus","2 fois plus","4 fois plus")) +
  scale_y_log10() +
  labs(title = "Quels prénoms sont plus (ou moins) fréquents que ce qui est prévu",
       subtitle = "Quand on prend en compte le sexe des élu.e.s",
       caption = "Données : Ministère de l'intérieur et Insee. Calculs et erreurs : B. Coulmont",
       x="Rapport (nombre d'élus)/(nombre attendu)",y="Nombre d'élus") +
  theme_ipsum( plot_margin = margin(0, 5, 0, 0),
               plot_title_size = 16,
               subtitle_size = 14,
               base_size = 10,
               axis_title_size = 12,
               axis_text_size = 8,
               strip_text_size = 10,
               base_family = "Helvetica")  +
  theme(legend.position="none",
        plot.background = element_rect(fill="white",color="white"),
        plot.title.position = "plot") 

#p_2

# version 3 : on contrôle par le département
# --------------------------------------------



distrib_elus <- rne_cm %>%
  mutate(annais = as.numeric(str_sub(date_de_naissance,7,10))) %>%
  mutate(code_du_departement = case_when(code_du_departement %in% c("92","93","94","95") & annais<1968 ~ "75",
                                         TRUE ~ code_du_departement) ) %>%
  mutate(code_du_departement = ifelse(code_du_departement %in% c("2A","2B"), "20",code_du_departement)) %>%
  filter(annais>1924,annais<2004) %>%
  group_by(code_du_departement) %>%
  summarize(N_elus=n())

distrib_prenoms <- fpd %>%
  group_by(dpt) %>%
  summarize(N_prenoms=sum(nombre))


ponderation <- distrib_elus %>%
  left_join(distrib_prenoms, by = c("code_du_departement"="dpt"))  %>%
  mutate(rapport = N_elus/N_prenoms)


prenoms_prevus <- fpd %>%
  mutate(annais = as.numeric(annais)) %>%
  mutate(code_sexe = ifelse(sexe==1,"M","F")) %>%
  mutate(code_du_departement = dpt) %>%
  left_join(ponderation %>% select(code_du_departement,rapport),
            by = c("code_du_departement")) %>%
  filter(!is.na(rapport)) %>%
  mutate(nombre_pondere = nombre*rapport) %>%
  group_by(prenom) %>%
  summarize(nombre_prevu = sum(nombre_pondere)) %>%
  ungroup() %>%
  mutate(prenom = str_to_title(prenom))


prenoms_elus <- rne_cm %>%
  group_by(prenom) %>%
  summarize(n_elus = n())

comparaison_3 <- prenoms_elus %>%
  left_join(prenoms_prevus)


comparaison_3 <- comparaison_3 %>%
  mutate(rapport = n_elus/nombre_prevu)

res <- bind_rows(res, comparaison_3 %>% mutate(controles = "Département"))

p_3 <- comparaison_3 %>%
  filter(prenom %in% c(prenoms_a_garder$prenom)) %>%
  mutate(taille = 2*abs(log(rapport)) + .3*log(n_elus) ) %>% 
  ggplot(aes(rapport,n_elus)) +
  geom_vline(xintercept = 1,size=.2,lty=2) +
  geom_segment(aes(y=n_elus,yend=nombre_prevu,x=rapport,xend=rapport,
                   color = rapport),
               size=.5,
               arrow = arrow(angle = 30,ends="last",type="closed",
                             length = unit(.1,"cm"))) +
  ggrepel::geom_text_repel(aes(label=prenom,size=I(taille)),
                           #size=3,
                           box.padding = 0.001) +
  
  scale_colour_gradient2(midpoint = 0,low = "firebrick1",high="navyblue",
                         trans="log") +
  scale_x_log10(breaks = c(.25,.5,.8,1,1.25,2,4),
                minor_breaks = NULL,
                labels = c("4 fois moins","Moitié moins","1,25 fois moins",
                           "Autant","1,25 fois plus","2 fois plus","4 fois plus")) +
  scale_y_log10() +
  labs(title = "Quels prénoms sont plus (ou moins) fréquents que ce qui est prévu",
       subtitle = "Quand on contrôle par le département",
       caption = "Données : Ministère de l'intérieur et Insee. Calculs et erreurs : B. Coulmont",
       x="Rapport (nombre d'élus)/(nombre attendu)",y="Nombre d'élus") +
  theme_ipsum( plot_margin = margin(0, 5, 0, 0),
               plot_title_size = 16,
               subtitle_size = 14,
               base_size = 10,
               axis_title_size = 12,
               axis_text_size = 8,
               strip_text_size = 10,
               base_family = "Helvetica")  +
  theme(legend.position="none",
        plot.background = element_rect(fill="white",color="white"),
        plot.title.position = "plot") 



# version 2_3 : on contrôle par le sexe et le département
# -------------------------------------------------------


distrib_elus <- rne_cm %>%
  mutate(annais = as.numeric(str_sub(date_de_naissance,7,10))) %>%
  mutate(code_du_departement = case_when(code_du_departement %in% c("92","93","94","95") & annais<1968 ~ "75",
                                         TRUE ~ code_du_departement) ) %>%
  mutate(code_du_departement = ifelse(code_du_departement %in% c("2A","2B"), "20",code_du_departement)) %>%
  filter(annais>1924,annais<2004) %>%
  group_by(code_sexe,code_du_departement) %>%
  summarize(N_elus=n())

distrib_prenoms <- fpd %>%
  mutate(annais = as.numeric(annais)) %>%
  mutate(code_sexe = ifelse(sexe==1,"M","F")) %>%
  group_by(code_sexe,dpt) %>%
  summarize(N_prenoms=sum(nombre))


ponderation <- distrib_elus %>%
  left_join(distrib_prenoms, by = c("code_sexe",
                                    "code_du_departement"="dpt"))  %>%
  mutate(rapport = N_elus/N_prenoms)


prenoms_prevus <- fpd %>%
  mutate(annais = as.numeric(annais)) %>%
  mutate(code_sexe = ifelse(sexe==1,"M","F")) %>%
  mutate(code_du_departement = dpt) %>%
  left_join(ponderation %>% select(code_sexe,code_du_departement,rapport),
            by = c("code_sexe","code_du_departement")) %>%
  filter(!is.na(rapport)) %>%
  mutate(nombre_pondere = nombre*rapport) %>%
  group_by(prenom) %>%
  summarize(nombre_prevu = sum(nombre_pondere)) %>%
  ungroup() %>%
  mutate(prenom = str_to_title(prenom))


prenoms_elus <- rne_cm %>%
  group_by(prenom) %>%
  summarize(n_elus = n())

comparaison_3bis <- prenoms_elus %>%
  left_join(prenoms_prevus)


comparaison_3bis <- comparaison_3bis %>%
  mutate(rapport = n_elus/nombre_prevu)

res <- bind_rows(res, comparaison_3bis %>% mutate(controles = "Sexe, département"))

# comparaison %>%
#   filter(nombre>300|n_elus>300) %>%
#   View()


p_3bis <- comparaison_3bis %>%
  filter(prenom %in% c(prenoms_a_garder$prenom)) %>%
  mutate(taille = 2*abs(log(rapport)) + .3*log(n_elus) ) %>% 
  ggplot(aes(rapport,n_elus)) +
  geom_vline(xintercept = 1,size=.2,lty=2) +
  geom_segment(aes(y=n_elus,yend=nombre_prevu,x=rapport,xend=rapport,
                   color = rapport),
               size=.5,
               arrow = arrow(angle = 30,ends="last",type="closed",
                             length = unit(.1,"cm"))) +
  ggrepel::geom_text_repel(aes(label=prenom,size=I(taille)),
                           #size=3,
                           box.padding = 0.001) +
  
  scale_colour_gradient2(midpoint = 0,low = "firebrick1",high="navyblue",
                         trans="log") +
  scale_x_log10(breaks = c(.25,.5,.8,1,1.25,2,4),
                minor_breaks = NULL,
                labels = c("4 fois moins","Moitié moins","1,25 fois moins",
                           "Autant","1,25 fois plus","2 fois plus","4 fois plus")) +
  scale_y_log10() +
  labs(title = "Quels prénoms sont plus (ou moins) fréquents que ce qui est prévu",
       subtitle = "Quand on contrôle par le département et le sexe",
       caption = "Données : Ministère de l'intérieur et Insee. Calculs et erreurs : B. Coulmont",
       x="Rapport (nombre d'élus)/(nombre attendu)",y="Nombre d'élus") +
  theme_ipsum( plot_margin = margin(0, 5, 0, 0),
               plot_title_size = 16,
               subtitle_size = 14,
               base_size = 10,
               axis_title_size = 12,
               axis_text_size = 8,
               strip_text_size = 10,
               base_family = "Helvetica")  +
  theme(legend.position="none",
        plot.background = element_rect(fill="white",color="white"),
        plot.title.position = "plot") 


#p_3bis

# version 4 : on contrôle par l'année de naissance
# ------------------------------------------------

distrib_elus <- rne_cm %>%
  mutate(annais = as.numeric(str_sub(date_de_naissance,7,10))) %>%
  filter(annais>1924,annais<2004) %>%
  group_by(annais) %>%
  summarize(N_elus=n())


distrib_prenoms <- fpn %>%
  mutate(annais = as.numeric(annais)) %>%
  group_by(annais) %>%
  summarize(N_prenoms=sum(nombre))


ponderation <- distrib_elus %>%
  left_join(distrib_prenoms, by = c("annais"))  %>%
  mutate(rapport = N_elus/N_prenoms)


prenoms_prevus <- fpn %>%
  mutate(annais = as.numeric(annais)) %>%
  left_join(ponderation %>% select(annais,rapport),
            by = c("annais")) %>%
  filter(!is.na(rapport)) %>%
  mutate(nombre_pondere = nombre*rapport) %>%
  group_by(prenom) %>%
  summarize(nombre_prevu = sum(nombre_pondere)) %>%
  ungroup() %>%
  mutate(prenom = str_to_title(prenom))


prenoms_elus <- rne_cm %>%
  group_by(prenom) %>%
  summarize(n_elus = n())

comparaison_4 <- prenoms_elus %>%
  left_join(prenoms_prevus)


comparaison_4 <- comparaison_4 %>%
  mutate(rapport = n_elus/nombre_prevu)




res <- bind_rows(res, comparaison_4 %>% mutate(controles = "Année"))



p_4 <- comparaison_4 %>%
  filter(prenom %in% c(prenoms_a_garder$prenom)) %>%
  mutate(taille = 2*abs(log(rapport)) + .3*log(n_elus) ) %>% 
  ggplot(aes(rapport,n_elus)) +
  geom_vline(xintercept = 1,size=.2,lty=2) +
  geom_segment(aes(y=n_elus,yend=nombre_prevu,x=rapport,xend=rapport,
                   color = rapport),
               size=.5,
               arrow = arrow(angle = 30,ends="last",type="closed",
                             length = unit(.1,"cm"))) +
  ggrepel::geom_text_repel(aes(label=prenom,size=I(taille)),
                           #size=3,
                           box.padding = 0.001) +
  
  scale_colour_gradient2(midpoint = 0,low = "firebrick1",high="navyblue",
                         trans="log") +
  scale_x_log10(breaks = c(.25,.5,.8,1,1.25,2,4),
                minor_breaks = NULL,
                labels = c("4 fois moins","Moitié moins","1,25 fois moins",
                           "Autant","1,25 fois plus","2 fois plus","4 fois plus")) +
  scale_y_log10() +
  labs(title = "Quels prénoms sont plus (ou moins) fréquents que ce qui est prévu",
       subtitle = "Quand on contrôle par l'année de naissance",
       caption = "Données : Ministère de l'intérieur et Insee. Calculs et erreurs : B. Coulmont",
       x="Rapport (nombre d'élus)/(nombre attendu)",y="Nombre d'élus") +
  theme_ipsum( plot_margin = margin(0, 5, 0, 0),
               plot_title_size = 16,
               subtitle_size = 14,
               base_size = 10,
               axis_title_size = 12,
               axis_text_size = 8,
               strip_text_size = 10,
               base_family = "Helvetica")  +
  theme(legend.position="none",
        plot.background = element_rect(fill="white",color="white"),
        plot.title.position = "plot") 








(p_1 + p_2) / (p_3 + p_4) / (p_3bis + p_5)


res %>%
  #filter(prenom %in% c(prenoms_a_garder$prenom)) %>%
  filter(prenom %in% c("Baptiste","Mohamed","Karim")) %>%
  mutate(controles = fct_relevel(controles,"Aucun","Sexe","Département","Sexe, département","Année","Année, sexe, département")) %>%
  arrange(controles) %>%
  filter(controles %in% c("Aucun","Année, sexe, département")) %>% 
  mutate(taille = 5*abs(log(rapport)) + .3*log(n_elus) ) %>%
  #mutate(taille = abs(log(rapport)) * log(n_elus) ) %>%
  
  ggplot(aes(rapport,nombre_prevu, group = prenom)) +
  geom_path(arrow = arrow(type = "closed",length = unit(.2,"cm")), alpha=.8) + geom_point(alpha=.2) +

  ggrepel::geom_text_repel(aes(label=prenom,size=2,color=controles),
                           
                           #size=3,
                           box.padding = 0.001) +

#  scale_colour_gradient2(midpoint = 1,low = "firebrick1",high="navyblue") +
  scale_x_log10(breaks = c(.5,.75,1,1.25),
                labels = c("Moitié moins","1,25 fois moins","Autant","1,25 fois plus")) +
  scale_y_log10() +
  labs(title = "Quels prénoms sont plus (ou moins) fréquents que ce qui est prévu",
       subtitle = "Quand on contrôle par l'année de naissance",
       caption = "Données : ministère de l'intérieur. Calculs : B. Coulmont",
       x="Rapport (nombre d'élus)/(nombre attendu)",y="Nombre d'élus") +
  theme_ipsum( plot_margin = margin(0, 5, 0, 0),
               plot_title_size = 16,
               subtitle_size = 14,
               base_size = 10,
               axis_title_size = 12,
               axis_text_size = 8,
               strip_text_size = 10,
               base_family = "Helvetica")  +
  theme(legend.position="none",
        plot.background = element_rect(fill="white",color="white"),
        plot.title.position = "plot") 


# graphique explicatif


comparaison_3 %>%
  filter(prenom %in% c(prenoms_a_garder$prenom)) %>%
  mutate(prenom = case_when(prenom %in% c("Jeannine","Bertrand","Jean") ~ prenom,
                            TRUE ~ NA_character_)) %>%
  filter(prenom %in% c("Jeannine","Bertrand","Jean")) %>%
  mutate(taille = 2*abs(log(rapport)) + .3*log(n_elus) ) %>% 
  ggplot(aes(rapport,n_elus)) +
  geom_vline(xintercept = 1,size=.2,lty=2) +
  geom_segment(aes(y=n_elus,yend=nombre_prevu,
                   x=rapport,xend=rapport,
                   color = rapport),
               size=.5,
               arrow = arrow(angle = 30,ends="last",type="closed",
                             length = unit(.1,"cm"))) +
  geom_text(aes(label=prenom,size=I(taille)),
            #size=3,
            box.padding = 0.001) +
  # annotations
  annotate(x=.22,y = 254, geom="text",label="254 Jeannine sont élues en 2022",
           adj=0,
           fontface="bold",
           color = "firebrick1") +
  annotate(x=.22,y = 1431, geom="text",label="Si les Jeannine avaient été choisies au hasard\ndans la population française\n1431 auraient été « élues ».",
           adj=0,
           fontface="bold",
           color = "firebrick1") +
  
  annotate(x=2.5,y = 1203, geom="text",label="1203 Bertrand sont élus en 2022",
           adj=1,
           fontface="bold",
           color = "navyblue") +
  annotate(x=2.5,y = 393, geom="text",label="Si les Bertrand avaient été choisis au hasard\ndans la population française\n393 auraient été « élus ».",
           adj=1,
           fontface="bold",
           color = "navyblue") +
  
  
  #
  scale_colour_gradient2(midpoint = 0,low = "firebrick1",high="navyblue",
                         trans="log") +
  scale_x_log10(breaks = c(.25,.5,.8,1,1.25,2,4),
                minor_breaks = NULL,
                labels = c("4 fois moins","Moitié moins","1,25 fois moins",
                           "Autant","1,25 fois plus","2 fois plus","4 fois plus")) +
  scale_y_log10(limits = c(90,NA)) +
  labs(title = "Quels prénoms sont plus (ou moins) fréquents que ce qui est prévu",
       subtitle = "Graphique explicatif",
       caption = "Données : Ministère de l'intérieur et Insee. Calculs et erreurs : B. Coulmont",
       x="Rapport (nombre d'élus)/(nombre attendu)",y="Nombre d'élus") +
  theme_ipsum( plot_margin = margin(0, 5, 0, 0),
               plot_title_size = 16,
               subtitle_size = 14,
               base_size = 10,
               axis_title_size = 12,
               axis_text_size = 8,
               strip_text_size = 10,
               base_family = "Helvetica")  +
  theme(legend.position="none",
        plot.background = element_rect(fill="white",color="white"),
        plot.title.position = "plot") 



resolution <- 130
ggsave("~/Desktop/prenoms_elus_explicatif.png",width=1100/resolution,height=700/resolution,dpi=resolution)


p_1
ggsave("~/Desktop/prenoms_elus_sans_controles.png",width=1100/resolution,height=700/resolution,dpi=resolution)


p_2
ggsave("~/Desktop/prenoms_elus_controle_sexe.png",width=1100/resolution,height=700/resolution,dpi=resolution)

p_3
ggsave("~/Desktop/prenoms_elus_controle_departement.png",width=1100/resolution,height=700/resolution,dpi=resolution)

p_3bis
ggsave("~/Desktop/prenoms_elus_controle_sexe_departement.png",width=1100/resolution,height=700/resolution,dpi=resolution)

p_4
ggsave("~/Desktop/prenoms_elus_controle_annee.png",width=1100/resolution,height=700/resolution,dpi=resolution)

p_5
ggsave("~/Desktop/prenoms_elus_tous_controles.png",width=1100/resolution,height=700/resolution,dpi=resolution)


