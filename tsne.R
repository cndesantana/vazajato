# Carregando dados

load("df_tweets_10000_vazajato.Rdat")

# Carregando Librarys: 

library(pacman)

p_load(caret, tidyverse, factoextra, epubr, tm, lexiconPT, broom, tidytext, widyr, irlba, 
       Rtsne,plotly)

require(forcats)

# Quais palavras estão mais próximas de cada um dos 4 termos pesquisados:

big <- df_tweets %>%
  group_by(screenName) %>%
  summarise(n = sum(retweetCount)) %>%
  top_n(1000, n) %>%
  arrange(-n) %>%
  select(screenName)
  
# Selecionando Stopwords: 

stop_words <- stopwords(kind = "pt") %>% 
  as.tibble()

# AS palavras que mais aparecem individualmente

unigram_probs <- df_tweets %>%
  tidytext::unnest_tokens(word, text, token = "tweets") %>% 
  right_join(big) %>%
  anti_join(stop_words, by= c("word" = "value")) %>% 
  filter(!word %in% c("rt", "é", "vagas", "disponíveis", "286", 
                      "adéli", "@makavelijones", "cristiane",
                      "=gt")) %>% 
  group_by(screenName) %>% 
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n)) %>% 
  filter(str_detect(word, "@") == FALSE) %>% 
  filter(str_detect(word, "https")== FALSE) %>% 
  ungroup() %>% 
  distinct(word, p, .keep_all = TRUE) 

head(unigram_probs)

# Matriz para tsne

tsne_matrix <- unigram_probs %>%
  tidytext::cast_sparse(screenName, word, p) %>% 
  as.matrix()

perfis <- rownames(tsne_matrix)

# Perfis:


  
# Criando um dataframe

aux <- perfis %>% 
  as.tibble() %>% 
  mutate(lula_ = factor(ifelse(value %in% lula$screenName,1,0))) 

# tsne

# Perplexidade entre 5 e 50

tsne <- Rtsne(tsne_matrix, dims = 2, perplexity= 30, 
              verbose=TRUE, max_iter = 1000)

tsne_final <- tsne$Y %>% 
  as.data.frame() %>% 
  bind_cols(aux) %>% 
  select(V1, V2, value, lula_)

# Gráfico 

pal <- c("darkblue", "blue", "red", "darkred")

plot_ly(tsne_final, type = 'scatter', mode = 'markers',
        text = ~value, color = ~lula_, colors = pal) %>% 
  add_trace(
    x = tsne_final$V1,
    y = tsne_final$V2,
    opacity = 0.6)
