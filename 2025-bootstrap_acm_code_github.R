# ---------------------------------
# stabilité des classes
# ---------------------------------

# une tentative de vérifier que les classes produites par une CAH après ACM
# sont assez stables.

# explications :  https://coulmont.com/blog/2025/02/16/la-stabilite-des-classes

# note : c'est du code pas du tout optimisé

# chargement des packages utiles
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(tidygraph)
library(igraph)
library(glue)

# on utilise des données qui sont dans le package FactoMiner
data("hobbies")
# on sélectionne les 400 premières lignes : création de l'objet df
df <- hobbies[sample.int(nrow(hobbies), size = 400, replace = FALSE),]
# ajout d'un identifiant
df <- df %>% mutate(id_num = row_number())


# calcul de l'ACM
res_acm <- MCA(df %>% select(-id_num), 
               graph = F, 
               quali.sup=19:22,quanti.sup=23)


# indiv : les individus
# récupérer les coordonnées des individus sur les deux premiers axes.
indiv <- as.data.frame(res_acm$ind$coord[,1:2]) 

# classification ascendante hiérarchique
# on garde juste trois clusters
res.hcpc = HCPC(res_acm, 
                graph=FALSE,
                nb.clust = 3)

# on affecte le numéro de cluster à indiv
indiv$cluster <- res.hcpc$data.clust$clust

# base avec les coordonnées des individus sur les axes 1 et 2
# et les numéros de cluster
df_clusters <- df %>% bind_cols(indiv)




# procédure de "bootstrap"
# ---------------------------------

# but : générer des échantillons plus petits à partir de l'échantillon de départ
# et réaliser la procédure précédente : ACM -- CAH

# on enlève 20% des lignes
df_clusters_bootstrap  <- NULL
n_simulations <- 1000 # nombre de simulations
n_garder <- 320       # nombre de lignes à garder

# boucle : n simulations ACM -- CAH
# avec n_simulations = 1000 ça peut prendre du temps
for (i in 1:n_simulations) {
  # création du sous-échantillon
  df_acm_bootstrap <- slice_sample(df, n = n_garder)
  # calcul de l'ACM
  res_acm_bootstrap <- MCA(df_acm_bootstrap %>% select(-id_num), 
                           graph = F, 
                           quali.sup=19:22,quanti.sup=23)
  # # résultat mis dans une liste
  # df_variables_bootstrap[[i]] <- as_tibble(res_acm_bootstrap$var$coord) %>% 
  #   mutate(var = rownames(res_acm_bootstrap$var$coord)) %>%
  #   mutate(boot=i)
  # classification ascendante hiérarchique
  res.hcpc_bootstrap = HCPC(res_acm_bootstrap, graph=FALSE,
                            nb.clust = 3)
  # on affecte le numéro de cluster et on met le tout dans une liste
  df_clusters_bootstrap[[i]] <- df_acm_bootstrap %>% 
    # clust : numéro de cluster
    bind_cols(clust = res.hcpc_bootstrap$data.clust$clust ) %>%
    mutate(boot=i) # boot, c'est le numéro de la simulation
}

# passage du format list au format table
df_simulations <- do.call(rbind.data.frame,df_clusters_bootstrap)
# on ne garde que le n°de simulation, l'identifiant de l'individu et le n° cluster
df_simulations <- df_simulations %>% select(boot,id_num,clust)


# But de l'opération suivante 
# regarder qui sont les voisins des individus
df_simulations <- df_simulations %>% left_join(df_clusters, by = "id_num")


# Bootstrap
# mais au format "réseau"

# proportion des tirages qui conduisent à "être dans le même cluster"
# au sens de "avoir les mêmes voisins"
# calculer "être dans la même composante" que le réseau de départ...
# ou comparer deux réseaux et déterminer quels liens sont différents

# il faut une réflexion en terme de réseau, car les n° de cluster peuvent changer
# dans certains sous-échantillons : le "cluster 2" de l'échantillon de départ
# peut parfois avoir les individus du "cluster 3" de certains sous-échantillons

# le réseau initial avec les données completes : les 3 clusters
g_initial <- df_clusters %>% 
  mutate(cluster = paste("cluster",cluster,sep="")) %>%
  select(id_num,cluster) %>%
  as_tbl_graph(directed=FALSE)

# avec tidygraph/igraph : obtenir le réseau des individus
# pour savoir qui sont les voisins de chaque individu
igraph::V(g_initial)$type <- igraph::V(g_initial)$name %in% df_clusters$id_num
g_projections<-igraph::bipartite_projection(g_initial)
g_individus_initial <- as_tbl_graph(g_projections$proj2, directed=FALSE)

differences <- NULL

# procédure de repérage des changements de cluster
# là aussi, ça peut prendre du temps
for (i in 1:n_simulations) {
  # le réseau du premier tirage
  edgelist <- df_simulations %>% 
    mutate(clust=paste("clust",clust,sep="")) %>% 
    select(id_num,clust,boot) %>% filter(boot == i)
  # création du réseau des individus
  g <- as_tbl_graph(edgelist,directed = FALSE)
  igraph::V(g)$type <- igraph::V(g)$name %in% edgelist$clust
  g_projections<-igraph::bipartite_projection(g)
  g_individus <- as_tbl_graph(g_projections$proj1, directed=FALSE)
  
  # réduire le graphe initial aux nodes présentes dans la simulation
  # afin de regarder qui sont les voisins
  g_individus_initial_reduit <- g_individus_initial %>%
    activate(nodes) %>%
    filter(name %in% (g_individus %>% activate(nodes) %>% as_tibble() %>% .$name))
  
  # on compare "réseau initial" et "reseau issu du sous-échantillon"
  # et on cherche les individus qui n'ont pas les mêmes voisins
  differences[[i]] <- graph_join(g_individus_initial_reduit,g_individus) %>%
    # on a fait la jointure de deux réseaux 
    convert(to_undirected) %>%
    convert(to_simple) %>%
    activate(edges) %>%
    mutate(weight = map_dbl(.orig_data, ~ sum(.x$weight))) %>%
    # on garde les "edges" qui ne sont pas dans le réseau initial (clusters)
    filter(weight == 1) %>%
    # si weight > 1 alors ça veut dire : même voisinage que réseau initial
    activate(nodes) %>%
    # on garde juste les "nodes" qui ont un degré élevé
    # justification : si Node(i) est hors cluster initial
    # alors Node(i) est en lien avec toutes les nodes de son nouveau cluster
    filter(centrality_degree() >= max( centrality_degree() ) & centrality_degree()>1 ) %>%
    as_tibble() %>%
    mutate(boot = i )
}

# on avait une liste, on passe au format tibble
differences_tbl <- do.call(rbind.data.frame,differences)


# on calcule la fréquence de changement de cluster, par individu
# amélioration possible : ne pas diviser par n_simulations
# mais seulement par le nombre de fois où l'individu (i) est dans une simulation
differences_n <- differences_tbl %>%
  group_by(name) %>%
  summarise(N=n()) %>%
  arrange(-N) %>%
  mutate(p = N/n_simulations)


# on visualise les individus qui changent souvent de cluster
# souvent étant : "plus de 5% du temps"
proportion_changement_cluster <- .05

df_clusters %>% left_join(differences_n %>% mutate(name = as.numeric(name)), 
                                by = c("id_num"="name")) %>%
  mutate(diff_clust = case_when(p>proportion_changement_cluster ~ 19,
                                #p<.03  ~ 3, #17,
                                TRUE ~ 8) ) %>%
  mutate(label_diff = case_when(p>proportion_changement_cluster ~ id_num,
                                #p<.03  ~ NA_real_, #17,
                                TRUE ~ NA_real_) ) %>% 
  ggplot(aes(`Dim 1`,`Dim 2`)) +
  #geom_point()
  geom_point(aes(color = as.factor(cluster), 
                  shape = I(diff_clust)),
              size=4,alpha=.7)  +
  #ggrepel::geom_label_repel(aes(label = label_diff),fill=NA) +
  scale_color_manual(values = c("navyblue","firebrick1","darkgreen")) +
  labs(title = glue("Visualisation des individus qui changent de cluster au moins dans {100*proportion_changement_cluster}% des simulations"),
       subtitle = glue("Méthode : {n_simulations} analyses des correspondances multiples et classifications ascendantes hiérarchiques\nen enlevant à chaque fois {100-100*round(n_garder/nrow(df),1)}% des individus au hasard")) +
  coord_fixed() + # pour que les échelles des axes soient identiques
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne horizontale y = 0
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne verticale x = 0
  theme_minimal() +
  theme(legend.position="none",
        plot.title.position = "plot",
        plot.background = element_rect(fill="white",color="white"))

#ggsave("~/Desktop/20250215-acm-simulations.png",width=8,height=6,dpi=300)

# visualisation de l'ACM originale
# --------------------------------

df_clusters %>%
  ggplot(aes(`Dim 1`,`Dim 2`)) +
  #geom_point()
  geom_point(aes(color = as.factor(cluster), 
                  #shape = I(diff_clust)
                  ),
              shape = 8,
              size=5,alpha=.7)  +
  #ggrepel::geom_label_repel(aes(label = label_diff),fill=NA) +
  scale_color_manual(values = c("navyblue","firebrick1","darkgreen")) +
  labs(title = glue("Résultats d'une ACM, axes 1 et 2"),
       subtitle = glue("Sur un échantillon de 400 individus\nLes couleurs indiquent les clusters après une CAH")) +
  coord_fixed() + # pour que les échelles des axes soient identiques
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne horizontale y = 0
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne verticale x = 0
  theme_minimal() +
  theme(legend.position="none",
        plot.title.position = "plot",
        plot.background = element_rect(fill="white",color="white"))

#ggsave("~/Desktop/20250215-acm.png",width=8,height=6,dpi=300)
