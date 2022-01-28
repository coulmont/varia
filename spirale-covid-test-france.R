library(tidyverse)
library(broom)
library(glue)
library(hrbrthemes)
library(lubridate)

df <- read_csv2("https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c")


# --------------------------
#
# test positifs, quotidiens, 
# version polaire spirale
#
# --------------------------

decalage = 100000
pente = 500
debuts <- tibble(jour= seq(ymd('2020-01-01'),ymd('2020-05-12'),by='days'))

tests <- df %>%
  filter(cl_age90=="0") %>% # pour ne garder que "tous ages" sans les autres classes
  group_by(jour) %>%
  summarize(total = sum(`P`)) %>%
  ungroup()
tests <- bind_rows(debuts,tests)
tests <- tests %>% 
  mutate(increment = row_number()) %>%
  mutate(base = decalage +(pente *increment )) %>%
  mutate(total = total + decalage +(pente *increment )) %>%
  mutate(ligne = decalage +(pente *increment )) %>%
  mutate(graduation = 2*decalage +(pente *increment ))

#tests <- tests[1:(nrow(tests)-5),]


jours_enlever <- nrow(tests) - 366
dernier_jour <-  ymd(last(tests$jour))
tests <- tests %>%
  mutate(taille = (.1 + (increment/nrow(tests))^2 )/2)

p <- tests %>% 
  ggplot(aes(jour,total,weight=total)) + 
  # graduation janvier avril juillet octobre
  annotate(geom="segment",
           x=ymd("2020-01-01"),xend=ymd("2020-01-01"),
           y=0,yend=867500,lty=2,size=.1) +
  annotate(geom="label",label="janvier",color="#555555",
           x=ymd("2020-01-01"),
           label.size=NA,
           y=867500,lty=2,size=3) +
  annotate(geom="segment",
           x=ymd("2020-04-01"),xend=ymd("2020-04-01"),
           y=0,yend=567500,lty=2,size=.1) +
  annotate(geom="label",label="avril",color="#555555",
           x=ymd("2020-04-01"),
           label.size=NA,
           y=567500,lty=2,size=3) +
  annotate(geom="segment",
           x=ymd("2020-07-01"),xend=ymd("2020-07-01"),
           y=0,yend=607500,lty=2,size=.1) +
  annotate(geom="label",label="juillet",color="#555555",
           x=ymd("2020-07-01"),
           label.size=NA,
           y=607500,lty=2,size=3) +
  annotate(geom="segment",
           x=ymd("2020-10-01"),xend=ymd("2020-10-01"),
           y=0,yend=667500,lty=2,size=.1) +
  annotate(geom="label",label="octobre",color="#555555",
           label.size=NA,
           x=ymd("2020-10-01"),
           y=667500,lty=2,size=3) +
  # ligne à zéro
  geom_segment(data = . %>% filter(jour == first(jour)|jour==last(jour)),
               aes(x = first(jour),
                   xend=last(jour),
                   y = first(ligne),
                   yend=last(ligne)), size=.3,lty=1) +
  # # avec variation taille et épaisseur - tiges/points 
  geom_point(color="firebrick1",aes(size=2*I(taille))) +
  geom_segment(aes(x=jour,xend=jour,y=base,yend=total,size=I(taille)),color="firebrick1") +
  # graduation à 100 000
  geom_segment(data = . %>% filter(jour == first(jour)|jour==last(jour)),
               aes(x = first(jour),
                   xend=last(jour),
                   y = first(graduation),
                   yend=last(graduation)), size=.1,lty=3) +
  # graduation à 300 000
  geom_segment(data = . %>% filter(jour > ymd("2021-11-30")),
               aes(x = first(jour),
                   xend=last(jour),
                   y = 200000+first(graduation),
                   yend=200000+last(graduation)), 
               inherit.aes= F, size=.1,lty=3, alpha=.5) +
  # graduation à 500 000
  geom_segment(data = . %>% filter(jour > ymd("2021-11-30")),
               aes(x = first(jour),
                   xend=last(jour),
                   y = 400000+first(graduation),
                   yend=400000+last(graduation)), size=.1,lty=3, alpha=.2) +
  coord_polar() +
  # label des années sur la spirale
  annotate(geom = "label",
         x = ymd("2020-01-01"),
         y = 100000 ,
         label="2020") +
  annotate(geom = "text",
           x = ymd("2021-12-31"),
           y = 253500 ,
           label="2021") +
  annotate(geom = "text",
           x = ymd("2021-12-31"),
           y = 446952 ,
           label="2022") +
  annotate(geom = "text",
           x = dernier_jour,
           y = 567500 ,
           adj=0,
           label="   100 000 tests positifs",
           size=2.5,
           color="darkgray") +
  annotate(geom = "text",
           x = dernier_jour,
           y = 767500 ,
           adj=0,
           label="   300 000 tests positifs",
           size=2.5,
           color="darkgray") +
  annotate(geom = "text",
           x = dernier_jour,
           y = 967500 ,
           adj=0,
           label="   500 000 tests positifs",
           size=2.5,
           color="darkgray") +
  scale_x_date(expand = expansion(add=c(0,-jours_enlever)),
               breaks = NULL,
               date_labels = "%B") +
  scale_y_continuous(limits=c(0,NA),
                     expand = expansion(add=c(0,-170000)),
                     labels = NULL) +
  theme(plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white"),
        axis.ticks = element_line(unit(0,"cm")),
        plot.margin =  margin(-40,0,-100,0),
        panel.grid = element_blank(),
        plot.title.position="plot") +
  labs(x=NULL,y=NULL,
       caption = "Graphique B. Coulmont, inspiré par le New York Times et @soustha. Données SPF/data.gouv.fr")

p
cowplot::ggdraw(p) +
  cowplot::draw_label("Nombre quotidien de tests positifs", x = 0.05, y = 0.97, hjust = 0, vjust = 0,
                      fontfamily = "", fontface = "bold", size = 17,
                      colour = "black") +
  cowplot::draw_label("Données : SPF / data.gouv.fr", x = 0.05, y = 0.94, hjust = 0, vjust = 0,
                      fontfamily = "", fontface = "plain",  size = 10,
                      colour = "black") +
  cowplot::draw_label("Graphique (et erreurs) : B. Coulmont", x = 0.05, y = 0.91, hjust = 0, vjust = 0,
                      fontfamily = "", fontface = "plain", size = 10,
                      colour = "black") +
  cowplot::draw_label("Inspiration : New York Times et @soustha", x = 0.65, y = 0.02, hjust = 0, vjust = 0,
                      fontfamily = "", fontface = "plain",  size = 8,
                      colour = "#999999") 
#ggsave(glue("~/Desktop/testpositifs_polaire{today()}.png"),width=1200/130,height=1000/130,dpi=130)

ggsave(glue("~/Desktop/2testpositifs_polaire{today()}.png"),width=2400/260,height=2000/260,dpi=260)

