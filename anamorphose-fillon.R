
# sur macOS
system("brew install fftw")
devtools::install_github("omegahat/Rcartogram")
devtools::install_github('chrisbrunsdon/getcartr',subdir='getcartr')

library(Rcartogram)
library(getcartr)
library(rgdal)

paris <- readOGR("/Users/baptiste/Dropbox/data/2017-listes-paris/secteurs-des-bureaux-de-vote-3/","secteurs-des-bureaux-de-vote")
paris <- spTransform(paris, CRS ("+init=epsg:2154") ) # change la projection en LAMBERT
paris_carto <- quick.carto(spdf = paris, v = paris$nbr_elect_f, res=256)
paris <-  sf::st_as_sf(paris)
paris_carto <- sf::st_as_sf(paris_carto)

library(tidyverse)
library(classInt)

df <- read_csv2("~/Dropbox/procurations/paris/paris-2017/resultats_electoraux-2.csv")
df <- df %>% filter(`date du scrutin` %in% c("23/04/2017"))
df <- df %>% dplyr::select(2,3,4,6,7,8,9,10,12,14,15,16,17)
t1 <- df %>% filter(`date du scrutin` %in% c("23/04/2017")) %>%
  spread(key=`nom du candidat ou liste`,value=`nombre de voix du candidat ou liste obtenues pour le bureau de vote`)
t1 <- t1 %>% mutate(numbv=paste(`numero d'arrondissement 01 a 20`,`numero de bureau de vote 000 a 999`,sep="-"))

# on affecte les bons nombre de procuration
# dans les bureaux 10-1 et 16-46
# vérification faite à la préfecture

t1 <- t1 %>% mutate(`Nombre de procurations du bureau de vote`=ifelse(numbv=="10-1",81,
                                                                      ifelse(numbv=="16-46",44,`Nombre de procurations du bureau de vote`)) )


t1 <- t1 %>% mutate(p_fillon=100*FILLON/`nombre d'exprimes du bureau de vote`)

paris <- paris %>% left_join(dplyr::select(t1,numbv,p_fillon),by=c("id_bv"="numbv"))
paris_carto <- paris_carto %>% left_join(dplyr::select(t1,numbv,p_fillon),by=c("id_bv"="numbv"))

library(classInt)
nclr <- 9
class <- classIntervals(paris$p_fillon, nclr, style="fisher", dataPrecision=1, na.rm=T)
paris$bin_fillon <- cut(paris$p_fillon, breaks = class$brks, include.lowest=T)
ggplot(paris) + 
  geom_sf(aes(fill=bin_fillon),size=.1,color=NA) + 
  viridis::scale_fill_viridis(discrete=T,option="C",labels=paste(round(class$brks,1)[1:nclr]," à ", round(class$brks,1)[2:(nclr+1)] )) +
  labs(title="Fréquence du vote Fillon au premier tour",
       subtitle="Élection présidentielle de 2017",
       caption="Sources : opendata.paris.fr. Réalisation B. Coulmont.",
       fill="% des voix exprimées") +
  theme(text = element_text(color = "#333333")
        ,plot.title = element_text(size = 18, color = "#333333")
        ,plot.subtitle = element_text(size = 10)
        ,plot.caption = element_text(size = 8)
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,panel.grid.major = element_line(color = NA)
        ,legend.background = element_blank()
  ) 
ggsave("~/Desktop/fillon_paris.png",width=8,height=5.1637,dpi=137.5)

paris_carto$bin_fillon <- cut(paris$p_fillon, breaks = class$brks, include.lowest=T)
ggplot(paris_carto) + 
  geom_sf(aes(fill=bin_fillon),size=.1,color=NA) + 
  viridis::scale_fill_viridis(discrete=T,option="C",labels=paste(round(class$brks,1)[1:nclr]," à ", round(class$brks,1)[2:(nclr+1)] )) +
  labs(title="Fréquence du vote Fillon au premier tour - Cartogramme",
       subtitle="Élection présidentielle de 2017",
       caption="Sources : opendata.paris.fr. Réalisation B. Coulmont.",
       fill="% des voix exprimées") +
  theme(text = element_text(color = "#333333")
        ,plot.title = element_text(size = 18, color = "#333333")
        ,plot.subtitle = element_text(size = 10)
        ,plot.caption = element_text(size = 8)
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,panel.grid.major = element_line(color = NA)
        ,legend.background = element_blank()
  ) 
ggsave("~/Desktop/fillon_paris_carto.png",width=8,height=5.1637,dpi=137.5)
