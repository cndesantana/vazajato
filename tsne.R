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
  top_n(40, n) %>% 
  arrange(-n)
  
  
  
# Selecionando Stopwords: 

stop_words <- stopwords(kind = "pt") %>% 
  as.tibble()

# AS palavras que mais aparecem individualmente

unigram_probs <- df_tweets %>%
  tidytext::unnest_tokens(word, text, token = "tweets") %>% 
  anti_join(stop_words, by= c("word" = "value")) %>% 
  filter(!word %in% c("rt", "é", "vagas", "disponíveis", "286", 
                      "adéli", "@makavelijones", "cristiane",
                      "=gt")) %>% 
  group_by(screenName) %>% 
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n)) %>% 
  filter(str_detect(word, "@") == FALSE) %>% 
  filter(str_detect(word, "https")== FALSE)

head(unigram_probs)

# Matriz para tsne

tsne_matrix <- unigram_probs %>%
  tidytext::cast_sparse(screenName, word, p) %>% 
  as.matrix() %>% 
  
