library(tidytext)

options(scipen = 9999)
theme_set(theme_classic())
ler_tuites <- function(path) {
  env <- new.env()
  load(path, env)
  env <- as.list(env)
  do.call(bind_rows, env) %>% 
    as_tibble()
}
sent2cat <- function(valor) {
  resp <- rep("neutro", length(valor))
  resp[valor < 0] <- "negativo"
  resp[valor > 0] <- "positivos"
  resp
}
tratar_rede <- function(grafo_, size = 0.2, arrow_width = 1, 
                        paleta = "Set1") {
  centralidade <- centr_degree(grafo_, "in")$res
  tamanhos <- floor((centralidade[centralidade != 0]) ^ (1/3))
  nomes_grupos <- vertex_attr(grafo_, "name", V(grafo_)[centralidade != 0])
  
  comunidades <- igraph::clusters(grafo_)
  n_comun <- length(unique(comunidades$membership))
  
  if (n_comun == 1) {
    comunidades <- grafo_ %>% 
      igraph::as_data_frame() %>% 
      select(from, to) %>% 
      graph_from_data_frame(FALSE) %>% 
      igraph::cluster_louvain()
    n_comun <- length(unique(comunidades$membership))
  }
  
  if (n_comun > 9) {
    cores <- colors()[seq(30, 600, length.out = n_comun)]
  } else {
    cores <- RColorBrewer::brewer.pal(n_comun, paleta)
  }
  
  vertex_attr(grafo_, "size") <- size
  vertex_attr(grafo_, "color") <- cores[comunidades$membership]
  vertex_attr(grafo_, "label") <- ""
  vertex_attr(grafo_, "size",  V(grafo_)[centralidade != 0]) <- tamanhos
  vertex_attr(grafo_, "label",  V(grafo_)[centralidade != 0]) <- nomes_grupos
  edge_attr(grafo_, "arrow.width") <- arrow_width
  grafo_
}
tuites <- dir("data/", full.names = TRUE) %>% 
  str_subset("usuarios", TRUE) %>% 
  map_df(ler_tuites) %>% 
  distinct() %>% 
  mutate(data = lubridate::round_date(created, "10 minutes"))
tidy_tuites <- tuites %>% 
  unnest_tokens(word, text) %>% 
  filter(! word %in% c(get_stopwords("pt")$word, 
                       "t.co", "https", "é", "rt", 
                       "ser", "pra", "sobre", "1")) %>% 
  left_join(lexiconPT::oplexicon_v3.0, by = c("word" = "term"))
rede <- ler_tuites("data/tweets_dos_usuarios.RData") %>% 
  select(screen_name, mentions_screen_name) %>% 
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name))
  
grafo <- rede %>% 
  graph_from_data_frame() %>% 
  tratar_rede()

library(rgexf)    
cg1 <- grafo
saveAsGEXF = function(g, filepath="converted_graph.gexf")
{
  require(igraph)
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(V(g)$label))
    V(g)$label <- as.character(V(g))
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
  
  # combine all graph attributes into a meta-data
  graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
  
  # generate the gexf object
  output <- write.gexf(nodes, edges, 
                       edgesWeight=E(g)$weight,
                       edgesAtt = edgesAtt,
                       nodesAtt = nodesAtt,
                       meta=c(list(creator="Gopalakrishna Palem", description="igraph -> gexf converted file", keywords="igraph, gexf, R, rgexf"), graphAtt))
  
  print(output, filepath, replace=T)
}
filepath <- "grafo.gexf"
saveAsGEXF(cg1, filepath)    

load("data/tweets_dos_usuarios.RData")
library(lexiconPT)
tidy_tuites_com_sentimentos <-  tidy_tuites %>% 
      group_by(term, id, data) %>% 
      summarise(sentimento = sum(polarity, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(sentimento = sent2cat(sentimento))

names(tidy_tuites_com_sentimentos)
library(lubridate)

p1 <- tidy_tuites_com_sentimentos %>% filter(term %in% c("#MoroCriminoso","EuApoioaLavaJato")) %>% filter(data > ymd_hm("2019-06-10 12:00")) %>%  filter(sentimento != "neutro") %>% group_by(term, data, sentimento) %>%summarise(tot = n()) %>% ggplot(aes(x = data, y = tot, fill = sentimento)) + geom_bar(stat = "identity") + facet_wrap(~term) + ylab("Número de tuítes") + ggtitle("Sentimentos dos tuítes")
png("figures/sentimentos_tuites_morocriminoso_apoiolavajato.png", width = 3200, height = 1800, res = 300)
print(p1)
dev.off()

p1 <- tidy_tuites_com_sentimentos %>% filter(term %in% c("Moro","Intercept")) %>% filter(data > ymd_hm("2019-06-10 12:00")) %>%  filter(sentimento != "neutro") %>% group_by(term, data, sentimento) %>%summarise(tot = n()) %>% ggplot(aes(x = data, y = tot, fill = sentimento)) + geom_bar(stat = "identity") + facet_wrap(~term) + ylab("Número de tuítes") + ggtitle("Sentimentos dos tuítes")
png("figures/sentimentos_tuites_moro_intercept.png", width = 3200, height = 1800, res = 300)
print(p1)
p1
dev.off()

p1 <- tidy_tuites_com_sentimentos %>% filter(term %in% c("Moro","Intercept","Dallagnol"))  %>%  filter(sentimento != "neutro") %>% group_by(term, sentimento) %>%summarise(tot = n()) %>% ggplot(aes(x = term, y = tot, fill = sentimento)) + geom_bar(stat = "identity", position = "fill") + ylab("Número de tuítes") + xlab("Termos") + ggtitle("Sentimentos dos tuítes")
png("figures/sentimentos_tuites_moro_intercept_dallagnol.png", width = 3200, height = 1800, res = 300)
print(p1)
dev.off()

p1 <- tidy_tuites_com_sentimentos %>% filter(term %in% c("#EuEstouComMoro","#EuToComOMoro","euestoucommoro"))  %>%  filter(sentimento != "neutro") %>% group_by(term, sentimento) %>%summarise(tot = n()) %>% ggplot(aes(x = term, y = tot, fill = sentimento)) + geom_bar(stat = "identity", position = "fill") + ylab("Número de tuítes") + xlab("Termos") + ggtitle("Sentimentos dos tuítes")
png("figures/sentimentos_tuites_estoucommoro.png", width = 3200, height = 1800, res = 300)
print(p1)
dev.off()

