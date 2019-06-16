
# Carregando Librarys: 

library(pacman)

p_load(caret, tidyverse, factoextra, epubr, tm, lexiconPT, broom, tidytext, widyr, irlba, 
       Rtsne,plotly, forcats, rsample, glmnet, lime, text2vec, xgboost)


# stopwords


stop_words <- stopwords(kind = "pt") %>% 
  as.tibble()

# Carregando

load("data/ThousandMiles.RData")

moro_criminoso <- df_tweets %>% 
  filter(term == "#MoroCriminoso") %>% 
  distinct_all()

load("data/ThousandMoroDefenders.RData")

moro_defenders <- df_tweets %>% 
  mutate(term = "MoroDefenders") %>% 
  distinct_all()

# Juntando

df <- bind_rows(moro_criminoso, moro_defenders) %>% 
  filter(isRetweet == FALSE) %>% 
  select(term, text) %>% 
  distinct()

# Ajustando

unigram_probs <- df %>%
  mutate(id_ = seq(1:nrow(.))) %>% 
  tidytext::unnest_tokens(word, text, token = "tweets") %>% 
  anti_join(stop_words, by= c("word" = "value")) %>% 
  filter(!word %in% c("rt", "é", "vagas", "disponíveis", "286", 
                      "adéli", "@makavelijones", "cristiane",
                      "=gt", "1", "2018", "sérgio", "mecan", 
                      "3ª", "oi", "1406", "al", "el", "deyem",
                      "bey", "2", "10062019", "conv", "6", "gt",
                      "muitos", "dia", "ta", "ter", "diz", "eh", "aí", 
                      "vi", "sim", "cada", "três", "após", "naquelas"
  )) %>% 
  group_by(id_, term) %>% 
  count(word, sort = TRUE) %>%
  filter(str_detect(word, "@") == FALSE) %>% 
  filter(str_detect(word, "https")== FALSE) %>% 
  filter(n > 1)

head(unigram_probs)

# Plotting 2

p1 <- df %>%
  mutate(id_ = seq(1:nrow(.))) %>% 
  tidytext::unnest_tokens(word, text, token = "tweets") %>% 
  anti_join(stop_words, by= c("word" = "value")) %>% 
  filter(!word %in% c("rt", "é", "vagas", "disponíveis", "286", 
                      "adéli", "@makavelijones", "cristiane",
                      "=gt", "1", "2018", "sérgio", "mecan", 
                      "3ª", "oi", "1406", "al", "el", "deyem",
                      "bey", "2", "10062019", "conv", "6", "gt",
                      "muitos", "dia", "ta", "ter", "diz", "eh", "aí", 
                      "vi", "sim", "cada", "três", "após", "naquelas",
                      "vai","ta", "ser", "pra", "q", "vamos", 
                      "tudo", "todo", "sobre", "ta", "sempre", "todos",
                      "ver", "tá", "agora", "nunca", "você", "nada", "vc", 
                      "ainda", "gente", "pode", "hoje", "casa", "cara")) %>% 
  group_by(term) %>% 
  count(word, sort = TRUE) %>%
  filter(str_detect(word, "@") == FALSE) %>% 
  filter(str_detect(word, "https")== FALSE) %>% 
  top_n(20, n) %>% 
  ggplot(aes(x = reorder(word,n), y = n)) +
  geom_col() +
  facet_wrap(~term, ncol = 1, scales = "free_y") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Termos")

ggsave(p1, file = "moro_words.jpg")


