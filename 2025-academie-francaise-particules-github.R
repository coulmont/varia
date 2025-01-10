#### Académie francaise, suite
#### recuperation des fauteuils
#### ---------------------------

library(tidyverse)
#install.packages("WikidataQueryServiceR")
library(WikidataQueryServiceR)
library(hrbrthemes)
library(ggforce)
library(glue)


af_1 = query_wikidata('
SELECT ?x ?xLabel ?nomcompletLabel ?nomnaissanceLabel ?dn ?dd ?de ?df ?nodeLabel 
WHERE {    
{ BIND(wd:Q3067536 AS ?node)    # fauteuil 1
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
  OPTIONAL {?x wdt:P1559 ?nomcomplet.}
  OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

{ BIND(wd:Q70495940 AS ?node)    # fauteuil 2
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}   
  OPTIONAL {?x wdt:P1559 ?nomcomplet.}
  OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

{ BIND(wd:Q70495942 AS ?node)    # fauteuil 3
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

 { BIND(wd:Q70495944 AS ?node)    # fauteuil 4
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}   
  OPTIONAL {?x wdt:P1559 ?nomcomplet.}
  OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION
  
{ BIND(wd:Q70495946 AS ?node)    # fauteuil 5
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION
  
{ BIND(wd:Q70495948 AS ?node)    # fauteuil 6
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION
  
{ BIND(wd:Q70495951 AS ?node)    # fauteuil 7
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

{ BIND(wd:Q70495953 AS ?node)    # fauteuil 8
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

{ BIND(wd:Q70495954 AS ?node)    # fauteuil 9
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

{ BIND(wd:Q70495957 AS ?node)    # fauteuil 10
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}

    SERVICE wikibase:label {       
    bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr".     } }
')

af_2 = query_wikidata('
SELECT ?x ?xLabel ?nomcompletLabel ?nomnaissanceLabel ?dn ?dd ?de ?df ?nodeLabel 
WHERE { 

{ BIND(wd:Q70495959 AS ?node)    # fauteuil 11
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION
  
{ BIND(wd:Q70495960 AS ?node)    # fauteuil 12
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION
  
{ BIND(wd:Q70495963 AS ?node)    # fauteuil 13
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

{ BIND(wd:Q70495965 AS ?node)    # fauteuil 14
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
    UNION
    
{ BIND(wd:Q70495966 AS ?node)    # fauteuil 15
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
    UNION
    
    
  { BIND(wd:Q70495967 AS ?node)    # fauteuil 16
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495968 AS ?node)    # fauteuil 17
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495970 AS ?node)    # fauteuil 18
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495972 AS ?node)    # fauteuil 19
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495975 AS ?node)    # fauteuil 20
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
    


  
    SERVICE wikibase:label {       
    bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr".     } }
')


af_3 = query_wikidata('
SELECT ?x ?xLabel ?nomcompletLabel ?nomnaissanceLabel ?dn ?dd ?de ?df ?nodeLabel 
WHERE { 


{ BIND(wd:Q70495976 AS ?node)    # fauteuil 21
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION



{ BIND(wd:Q70495979 AS ?node)    # fauteuil 22
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION


{ BIND(wd:Q70495982 AS ?node)    # fauteuil 23
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}  
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION


  
{ BIND(wd:Q70495983 AS ?node)    # fauteuil 24
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495986 AS ?node)    # fauteuil 25
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495987 AS ?node)    # fauteuil 26
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495988 AS ?node)    # fauteuil 27
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495989 AS ?node)    # fauteuil 28
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495992 AS ?node)    # fauteuil 29
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70495994 AS ?node)    # fauteuil 30
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.}  
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}

  SERVICE wikibase:label {       
    bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr".     } }
')

af_4 = query_wikidata('
SELECT ?x ?xLabel ?nomcompletLabel ?nomnaissanceLabel ?dn ?dd ?de ?df ?nodeLabel 
WHERE { 

  { BIND(wd:Q70495997 AS ?node)    # fauteuil 31
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
    UNION


  { BIND(wd:Q70495998 AS ?node)    # fauteuil 32
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
        UNION
    
    { BIND(wd:Q70495999 AS ?node)    # fauteuil 33
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}   
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
    UNION
  

  { BIND(wd:Q70496000 AS ?node)    # fauteuil 34
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
    UNION
  

  { BIND(wd:Q70496003 AS ?node)    # fauteuil 35
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION


{ BIND(wd:Q70496004 AS ?node)    # fauteuil 36
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
  OPTIONAL {?x wdt:P1559 ?nomcomplet.}  
  OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
UNION

{ BIND(wd:Q70496007 AS ?node)    # fauteuil 37
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
  OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
  OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION


{ BIND(wd:Q70496009 AS ?node)    # fauteuil 38
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70496010 AS ?node)    # fauteuil 39
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.}  
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}
  UNION

  { BIND(wd:Q70496012 AS ?node)    # fauteuil 40
  ?x p:P39 ?stmt.    
  ?stmt ps:P39 ?node .    
  OPTIONAL {?stmt pq:P580 ?de.}   
  OPTIONAL {?stmt pq:P582 ?df.}    
  OPTIONAL {?x wdt:P569 ?dn.}    
  OPTIONAL {?x wdt:P570 ?dd.} 
    OPTIONAL {?x wdt:P1559 ?nomcomplet.} 
    OPTIONAL {?x wdt:P1477 ?nomnaissance.}}


SERVICE wikibase:label {       
  bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr".     } }
')


af <- bind_rows(af_1,
                af_2 ,
                af_3,
                af_4 %>% mutate(dn = ymd_hms(dn)))



af_fauteuils <- af %>%
  mutate(date_fin = case_when(is.na(df) ~ dd,
                              TRUE ~ df))

# on garde les 740 academiciens (743 academiciens-fauteuils car certains sont revenus)
af_reduit <- af_fauteuils %>%
  mutate(nom_etude = case_when(is.na(nomcompletLabel) ~ xLabel,
                               TRUE ~ nomcompletLabel)) %>%
  mutate(nom_naissance = case_when(is.na(nomnaissanceLabel) ~ xLabel,
                                   TRUE ~ nomnaissanceLabel)) %>%
  group_by(nodeLabel) %>%
  distinct(x,de,.keep_all = T) %>%
  ungroup() %>%
  # on met la date du jour pour les personnes vivantes
  mutate(date_fin = case_when(is.na(date_fin) ~ ymd_hms("2025-12-31 00:00:00"),
                              TRUE ~ date_fin)) %>%
  mutate(particule_nom = str_detect(nom_etude,"\\b(D|DE|DES|DU|d|de|des|du|De|Des|Du)\\b"),
         particule_nomnaissance = str_detect(nom_naissance,"\\b(D|DE|DES|DU|d|de|des|du|De|Des|Du)\\b")) %>%
  mutate(particule = particule_nom|particule_nomnaissance) %>%
  
  #select(x,xLabel,nodeLabel,particule,date_fin,de) %>%
  mutate(date_fin = ymd(date_fin),
         de=ymd(de))

# vérifications
# -------------

# af_reduit %>%
#   mutate(petit_nom = str_sub(xLabel,start = 1,end=25)) %>%
#   mutate(de = ymd(de),
#          date_fin = ymd(date_fin)) %>%
#   arrange(de) %>%
#   mutate(date_fin = case_when(is.na(date_fin) ~ ymd("2024-12-31"),
#                               TRUE ~ date_fin) ) %>%
#   mutate(petit_nom = fct_reorder(petit_nom,de)) %>%
#   ggplot(aes(y=petit_nom,yend=petit_nom)) +
#   geom_segment(aes(x=de,xend=date_fin),color="gray") +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         legend.position="none") +
#   facet_wrap(~nodeLabel)

# verification
# af_reduit %>%
#   mutate(temps_academie = date_fin-de) %>%
#   arrange(-temps_academie)

# vérification
# af_reduit %>%
#   ungroup() %>%
#   arrange(nodeLabel,de) %>%
#   group_by(nodeLabel) %>%
#   #  arrange(de) %>%
#   mutate(comparaison = lead(de,1)) %>%
#   mutate(duree = comparaison-date_fin) %>%
#   View()

# tentative de tracer les Broglie
# broglies <- af_reduit %>%
#   filter(str_detect(xLabel,"rogli"))

# graphique
# on génère les dates : 

dates_af <- tibble(date = seq(ymd('1634-03-13'), 
                              ymd('2025-12-31'), 
                              by='1 month')) ### ici : jour, mois, semestre...

# on calcule la proportion date après date
# en fonction de la date d'élection et de la date de fin
resultats <- NULL

for (i in 1:nrow(dates_af)) {
  resultats[[i]] <- af_reduit %>%
    filter(de<=ymd(dates_af$date[i]),
           date_fin>=ymd(dates_af$date[i])) %>%
    summarize(date = dates_af$date[i],
              N = n(),
              nb_particule = sum(particule))
}

# ce ne sont pas nécessairement des résultats "quotidiens"
resultats_dates <- bind_rows(resultats)

# création de la table "résultats dates"
resultats_annuels <- resultats_dates %>%
  mutate(p_ponctuel = nb_particule/N) %>%
  mutate(annee = year(date)) %>%
  group_by(annee) %>%
  summarize(total = sum(N),
            nb_moyen_fauteuils = mean(N),
            particules = sum(nb_particule),
            min_particules = min(p_ponctuel),
            max_particules = max(p_ponctuel)) %>%
  mutate(p = particules/total)


# pour les commentaires !! attention changer date de fin
commentaires <- tribble(~type,~annee,~texte,
                         1,1756,"En 1756, 3 Académiciens sur 4\navaient un nom à particule.",
                         2,2025,"En 2025, il n'y a plus aucun·ne\nAcadémicien·ne à particule.")

# jointure avec les résultats
resultats_annuels <- resultats_annuels %>%
  left_join(commentaires,by = "annee")

# que faire pendant la période révolutionnaire
# quand les Académies sont supprimées ?
resultats_annuels <- resultats_annuels %>%
  # Académie supprimée entre 1793 et 1803
  mutate(type_ligne = case_when(annee %in% c(1794:1802) ~ 2,
                                TRUE ~ 1 ))


# graphique
resultats_annuels %>% 
  ggplot(aes(annee,p)) +
    # ruban avec le min / max mensuel
  geom_ribbon(aes(ymin = min_particules,ymax=max_particules),
              alpha=.5,
              fill="deepskyblue") +
  # ligne noire
  geom_line(linewidth=.3) +
  #commentaires
  geom_mark_circle(aes(annee, p, 
                       label = texte, group=type,
                       filter = type %in% c(1,2)),
                   color="firebrick1",
                   expand = unit(1, "mm"),
                   label.buffer = unit(60, "mm"),
                   label.lineheight = 1,
                   con.size = 0.2,
                   con.linetype = 2,
                   con.type = "elbow",
                   con.border = "all",
                   label.fontsize = 8) +
  scale_y_continuous(limits = c(0,NA),
                     labels = scales::percent_format(accuracy = 1),
                     expand = expansion(add=c(0.01,0.01))) +
  scale_x_continuous(expand = expansion(add=c(5,10))) +
  labs(title = "Académie française : Proportion d’immortel·le·s à particule (1634-aujourd'hui)",
       subtitle = "… comme Jean Lefèvre d’Ormesson, Marguerite Yourcenar, Armand de Vignerot du Plessis ou Hélène Zourabichvili épouse Carrère dite Carrère d’Encausse",
       caption = "Champ : Académie française, 1634-aujourd'hui. Immortel·le·s vivant·e·s uniquement. Données : wikidata. Calculs et erreurs : B. Coulmont.\n…et je ne compte pas le marquis de Vargas Llosa comme porteur de particule.",
       y = NULL,x = NULL) +
  theme_ipsum( plot_margin = margin(15, 15, 5, 15),
               plot_title_margin=5 ,
               subtitle_margin=15,
               plot_title_size = 15,
               #subtitle_size = 11,
               base_family = "Source Sans Pro")  +
  theme(plot.title.position = "plot",
        plot.subtitle = element_text(size=8.5),
        plot.background = element_rect(fill="white",color="white"),
        plot.caption.position = "plot",
        axis.text.y = element_text(hjust = 1))

res <- 300
ggsave(glue("~/Desktop/academie-particules-{today()}.png"),width=2400/res,height=1600/res,dpi=res)

