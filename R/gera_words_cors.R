
library(stringr)
library(dplyr)
library(tidytext)
library(tidyr)
library(widyr)
library(shiny)
library(igraph)
library(ggraph)


#load("~/GitHub/vazajato/data/todas_hashtags_repetindo.RData")

dallagnol <- readRDS("~/GitHub/vazajato/data/dallagnol.rds")
dallagnol$termo<- "dallagnol"
df_tweets<- dallagnol
rm("dallagnol")

desmoronando <- readRDS("~/GitHub/vazajato/data/desmoronando.rds")
desmoronando$termo <- "#desmoronando"
df_tweets<-df_tweets%>% bind_rows(desmoronando)
rm("desmoronando")

euapoioalavajato <- readRDS("~/GitHub/vazajato/data/euapoioalavajato.rds")
euapoioalavajato$termo <- "#euapoioalavajato"
df_tweets <-df_tweets %>% bind_rows(euapoioalavajato)
rm("euapoioalavajato")


eutocomomoro <- readRDS("~/GitHub/vazajato/data/eutocomomoro.rds")
eutocomomoro$termo <- "#eutocomomoro"
df_tweets <- df_tweets %>% bind_rows(eutocomomoro)
rm(eutocomomoro)

intercept <- readRDS("~/GitHub/vazajato/data/intercept.rds")
intercept$termo <- "intercept"
df_tweets <- df_tweets%>% bind_rows(intercept)
rm(intercept)

moro <- readRDS("~/GitHub/vazajato/data/moro.rds")
moro$termo <- "moro"
df_tweets <- df_tweets %>% bind_rows(moro)
rm(moro)

morocriminoso <- readRDS("~/GitHub/vazajato/data/morocriminoso.rds")
morocriminoso$termo <- "#morocriminoso"
df_tweets <- df_tweets %>% bind_rows(morocriminoso)
rm(morocriminoso)

vazajato <- readRDS("~/GitHub/vazajato/data/vazajato.rds")
vazajato$termo <- "#vazajato"
df_tweets <- df_tweets%>% bind_rows(vazajato)
rm(vazajato)

df_tweets <- df_tweets%>%
  mutate(text = str_to_lower(text))


texto_df <- dplyr::data_frame(classe = df_tweets$termo,texto =df_tweets$text)

analise_tweets <- texto_df %>%
  unnest_tokens(palavra,texto) %>%
  count(classe, palavra, sort = TRUE) %>%
  ungroup()

total_palavras <- analise_tweets %>%
  group_by(classe) %>%
  summarize(total=sum(n))

analise_tweets<- analise_tweets %>% left_join(total_palavras)


analise_tweets <- analise_tweets %>%
  bind_tf_idf(palavra, classe, n)



stop_words_twitter <- unique(c(unique(analise_tweets$palavra[analise_tweets$idf==0]), stopwords::stopwords("pt")))

termos<- unique(df_tweets$termo)


analise_twitter_secoes <- dplyr::data_frame(classe = df_tweets$termo,texto =df_tweets$text)%>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_twitter)
#C:\Users\Fernando Barbalho\Documents\GitHub\vazajato\Rede_de_Palavras\Data

save(list=c("analise_twitter_secoes"),  file="~/GitHub/vazajato/Rede_de_Palavras/data/word_cors.RData")

# we need to filter for at least relatively common words first
word_cors <- analise_twitter_secoes %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)


save(list=c("analise_twitter_secoes"),  file="~/GitHub/vazajato/Rede_de_Palavras/data/word_cors.RData")

set.seed(2016)

word_cors %>%
  filter(correlation > .65) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



df_texto_url <- df_tweets%>%
  select(user_id, text, status_url, description) %>%
  mutate(text = str_to_lower(text))


save(list = "df_texto_url", file = "~/GitHub/vazajato/Rede_de_Palavras/data/df_texto_url.RData")

df_texto_url %>%
  str_subset(text,"^(?=.*\\bmoro\\b)(?=.*\\bdescuido\\b)(?=.*\\bdedinho\\b).*$")


str_subset(texto,"^(?=.*\\bmoro\\b)(?=.*\\bdescuido\\b)(?=.*\\bdedinho\\b).*$")

str_detect(string=df_todas$text, pattern = "tacla", negate = TRUE)


select(text)

str_detect(text, "tacla")



select(distinct(text))

group_by(select(distinct(text)))%>%
  summarise(
    n()
  )

