library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(ggforce)

# 2005 ... 2018
ac <- bind_rows(read_csv("https://www.data.gouv.fr/fr/datasets/r/6eee0852-cbd7-447e-bd70-37c433029405",
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/9a7d408b-dd72-4959-ae7d-c854ec505354",
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/96aadc9f-0b55-4e9a-a70e-c627ed97e6f7",
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/185fbdc7-d4c5-4522-888e-ac9550718f71", # 2015
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/85dfe8c6-589f-4e76-8a07-9f59e49ec10d", # 2014
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/18b1a57a-57bf-4bf1-b9ee-dfa5a3154225", # 2013
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/b2518ec1-6529-47bc-9d55-40e2effeb0e7", # 2012
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/37991267-8a15-4a9d-9b1c-ff3e6bea3625", # 2011
                         col_types = cols(.default = col_character())),
                
                read_csv("https://www.data.gouv.fr/fr/datasets/r/decdfe8c-38ff-4a06-b7fc-615785f2914d", # 2010
                         col_types = cols(.default = col_character())),
                
                read_delim("https://www.data.gouv.fr/fr/datasets/r/fdfacdb9-f48e-4759-bae5-48d063216acb", # 2009
                         col_types = cols(.default = col_character()), delim = "\t"),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/722ebb99-c8b2-4635-bf8d-125dd280ee42", # 2008
                         col_types = cols(.default = col_character())),  
                read_csv("https://www.data.gouv.fr/fr/datasets/r/6fc7b169-4dfe-442c-8c28-8bd773aeddf8", # 2007
                        col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/fafa33cf-50cb-4092-a819-d5209f684089", # 2006
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/a47866f7-ece1-4de8-8d31-3a1b4f477e08", # 2005
                         col_types = cols(.default = col_character())))



us <- bind_rows(read_csv("https://www.data.gouv.fr/fr/datasets/r/72b251e1-d5e1-4c46-a1c2-c65f1b26549a",
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/07bfe612-0ad9-48ef-92d3-f5466f8465fe",
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/e4c6f4fe-7c68-4a1d-9bb6-b0f1f5d45526",
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/b43a4237-9359-4217-b833-8d3dc29a6c24",
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/457c10ff-ea6c-4238-9af1-d8dc62b896d4", #2014
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/af4349c5-0293-4639-8694-b8b628bfc6c3", #2013
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/a19e060e-1c18-4272-ac4e-d4745ab8fade", #2012
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/bd946492-31b3-428e-8494-a1e203bdc9cc", #2011
                         col_types = cols(.default = col_character())),
                
                read_csv("https://www.data.gouv.fr/fr/datasets/r/c5e5664d-1483-41da-a4c6-5f1727d7a353", #2010
                         col_types = cols(.default = col_character())),
                
                read_csv("https://www.data.gouv.fr/fr/datasets/r/2387db3d-11ee-4df4-aa31-4c3c748244d4", #2009
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/433e26cf-d4c8-4dd9-b3f2-ecbc8a8f0509", #2008
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/c5c30fc2-9bfd-4bcd-b45b-f01a31f1d087", #2007
                         col_types = cols(.default = col_character())),
                read_csv("https://www.data.gouv.fr/fr/datasets/r/ebb4c37e-1616-497d-b5ed-f8113bed2ae7", #2006
                         col_types = cols(.default = col_character())),             
                read_csv("https://www.data.gouv.fr/fr/datasets/r/cecdbd46-11f2-41fa-b0bd-e6e223de6b3c", #2005
                         col_types = cols(.default = col_character())))


us <- us %>% left_join(ac %>% select(Num_Acc:hrmn))
us <- us %>%   
  mutate(an = str_pad(an,2, pad="0")) %>%
  mutate(an = paste("20",an,sep="")) %>%
  mutate(an = as.numeric(an),
         an_nais = as.numeric(an_nais),
         age = an-an_nais) %>%
  mutate(date = ymd(paste(an,mois,jour,sep="-")),
         hrmn = str_pad(hrmn, 4, pad = "0")) %>% # View()
  mutate(jour = wday(date,label=TRUE,abbr=TRUE,week_start = 1, locale = "fr_FR"))

heures_accidents <- us %>%   
  #  mutate(date = ymd(paste("2018",mois,jour,sep="-")) ) %>%
  #  mutate(jour = wday(date,label=TRUE,abbr=TRUE,week_start = 1, locale = "fr_FR")) %>%
  group_by(heure = as.numeric(str_sub(hrmn,1,2)),an)  %>%
  summarize(N=n(),
            age_moyen =  mean( an - an_nais, na.rm=T),
            age_median = median(an - an_nais, na.rm=T)) %>%
  ungroup() 

heures_accidents <- heures_accidents %>%
  bind_rows(  heures_accidents %>% filter(heure==0) %>%
                mutate(heure = 24))


heures_accidents <- heures_accidents %>%
  mutate(annot_titre = case_when(heure==3 ~ "3 heures du matin",
                                 heure==10 ~ "10 heures du matin",
                                 TRUE ~ NA_character_)) %>%
  mutate(annot_label = case_when(heure==3 ~ "Âge moyen : 30 ans",
                                 heure==10 ~ "Âge moyen : 46 ans",
                                 TRUE ~ NA_character_))
  
  
  
  
p <- heures_accidents %>%   
  ggplot(aes(heure,age_moyen,group=an,color=as.factor(an)) ) + 
  geom_line(size=1,alpha=.9) +
  scale_x_continuous(breaks = c(0,6, 12, 18, 24),
                     labels = c("Minuit","06 h","Midi","18 h","Minuit")) +
  scale_color_viridis_d(option = "B",begin=.2,end=.8) +
  labs(title = "Âge moyen des personnes accidentées",
       subtitle = "France, accidents de la route entre 2005 et 2018",
       caption = "Base de données accidents corporels de la circulation. N = 2,14 millions. Calculs : B. Coulmont",
       y="Âge moyen",x="Heure") +

  geom_mark_ellipse(aes(label = annot_titre, 
                     description=annot_label,
                     fill=heure, group=heure,
                   filter = !is.na(annot_titre) & an==2018),
                   expand=unit(3,"mm"),
                   radius = unit(3, "mm"),
                   label.buffer = unit(4,"mm")) +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), plot_title_margin=5 , subtitle_margin=5) +
 theme(legend.position = "none") + 
  NULL
p
g <- ggplotGrob(p)
g$layout$l[g$layout$name == "title"] <- 2
g$layout$l[g$layout$name == "subtitle"] <- 2
g$layout$l[g$layout$name == "caption"] <- 9
grid::grid.newpage()
grid::grid.draw(g)
ggsave("~/Desktop/accidents.png",grid::grid.draw(g),width=110/13,height=70/13,dpi=130)



# faire geom_density avec groupe par heure

library(ggridges)
p <- us %>%
  mutate(heure = as.numeric(str_sub(hrmn,1,2)),an)  %>% 
  ggplot( aes(x=age, y=heure, group=heure)) +
  geom_density_ridges(panel_scaling = T,height=4, fill="navyblue",alpha=.6,color=NA) +#alpha=0.6, bandwidth=.1) 
  
  scale_y_reverse(breaks = c(0,6,12,18)) +
  scale_x_continuous(limits = c(0,95)) +
  labs(title = "Distribution de l'âge des personnes accidentées",
       subtitle = "France, accidents de la route entre 2015 et 2018",
       caption = "Base de données accidents corporels de la circulation. N = 529990. Calculs : B. Coulmont",
       x="Âge",y="Heure") +
  
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), plot_title_margin=5 , subtitle_margin=5) +
  theme(legend.position = "none") 
p
g <- ggplotGrob(p)
g$layout$l[g$layout$name == "title"] <- 2
g$layout$l[g$layout$name == "subtitle"] <- 2
g$layout$l[g$layout$name == "caption"] <- 9
grid::grid.newpage()
grid::grid.draw(g)
ggsave("~/Desktop/accidents-density.png",grid::grid.draw(g),width=8,height=5,dpi=150)


## heure et jour de l'accident

heures_jours_accidents <- us %>%   
  #  mutate(date = ymd(paste("2018",mois,jour,sep="-")) ) %>%
  #  mutate(jour = wday(date,label=TRUE,abbr=TRUE,week_start = 1, locale = "fr_FR")) %>%
  group_by(jour,heure = as.numeric(str_sub(hrmn,1,2)))  %>%
  summarize(N=n(),
            age_moyen =  mean( an - an_nais, na.rm=T),
            age_median = median(an - an_nais, na.rm=T)) %>%
  ungroup() 

# heures_accidents <- heures_accidents %>%
#   bind_rows(  heures_accidents %>% filter(heure==0) %>%
#                 mutate(heure = 24))
# 
# 
# heures_accidents <- heures_accidents %>%
#   mutate(annot_titre = case_when(heure==3 ~ "6 heures du matin",
#                                  heure==10 ~ "10 heures du matin",
#                                  TRUE ~ NA_character_)) %>%
#   mutate(annot_label = case_when(heure==3 ~ "Âge moyen : 30 ans",
#                                  heure==10 ~ "Âge moyen : 46 ans",
#                                  TRUE ~ NA_character_))

heures_jours_accidents <- heures_jours_accidents %>%
  mutate(rang = row_number()) %>%
  mutate(moment = paste(jour,heure)) %>%
  mutate(moment = fct_reorder(moment,rang))



p <- heures_jours_accidents %>%   
  ggplot(aes(moment,age_moyen,group=1) ) + 
  geom_line(size=1,alpha=.9) +
  scale_x_discrete(breaks = c("Lun 0","Lun 12","Mar 0","Mar 12","Mer 0","Mer 12","Jeu 0","Jeu 12",
                              "Ven 0","Ven 12","Sam 0","Sam 12","Dim 0","Dim 12")) +
                    # labels = c("Minuit","06 h","Midi","18 h","Minuit")) 
  #scale_color_viridis_d(option = "B",begin=.2,end=.8) +
  labs(title = "Âge moyen des personnes accidentées",
       subtitle = "France, accidents de la route entre 2015 et 2018",
       caption = "Base de données accidents corporels de la circulation. N = 529990. Calculs : B. Coulmont",
       y="Âge moyen",x="Heure") +
  
  # geom_mark_ellipse(aes(label = annot_titre, 
  #                       description=annot_label,
  #                       fill=heure, group=heure,
  #                       filter = !is.na(annot_titre) & an==2018),
  #                   expand=unit(3,"mm"),
  #                   radius = unit(3, "mm"),
  #                   label.buffer = unit(40,"mm")) +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), plot_title_margin=5 , subtitle_margin=5) +
  theme(legend.position = "none") 
p
g <- ggplotGrob(p)
g$layout$l[g$layout$name == "title"] <- 2
g$layout$l[g$layout$name == "subtitle"] <- 2
g$layout$l[g$layout$name == "caption"] <- 9
grid::grid.newpage()
grid::grid.draw(g)
ggsave("~/Desktop/accidentsjh.png",grid::grid.draw(g),width=8,height=5,dpi=150)




