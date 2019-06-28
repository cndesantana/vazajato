
# Carregando Librarys: 

library(pacman)

p_load(caret, tidyverse, factoextra, epubr, tm, lexiconPT, broom, tidytext, widyr, irlba, 
       Rtsne,plotly, forcats, rsample, glmnet, lime, text2vec, xgboost)


# stopwords


stop_words <- stopwords(kind = "pt") %>% 
  as.tibble()

# Carregando

load("ThousandMiles.RData")

moro_criminoso <- df_tweets %>% 
  filter(term == "#MoroCriminoso") %>% 
  distinct_all()

load("ThousandMoroDefenders.RData")

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

# Dividindo em teste e treino: 

books_split <- unigram_probs %>%
  select(id_, term) %>%
  initial_split()

train_data <- training(books_split)
test_data <- testing(books_split)


# Dividindo em Treino e Teste

books_split <- unigram_probs %>%
  select(id_, term) %>%
  initial_split()

train_data <- training(books_split)
test_data <- testing(books_split)

# Criando nossa matriz Esparsa

sparse_words <- unigram_probs %>%
  inner_join(train_data) %>%
  cast_sparse(id_, word, n) 

dim(sparse_words)

word_rownames <- as.integer(rownames(sparse_words))

# Dividindo

model_df <- train_data %>% 
  filter(id_ %in% word_rownames) %>% 
  distinct()

# Criando nosso Y

moro_criminoso <- model_df$term == "#MoroCriminoso"

#model

xgb_model <- xgb.train(list(max_depth = 12, eta = 0.1, 
                            objective = "binary:logistic",
                            eval_metric = "error", 
                            nthread = 1),
                       xgb.DMatrix(dtm_train, 
                                   label = training$term == "#MoroCriminoso"),
                       nrounds = 1000)


pred <- predict(xgb_model, get_matrix(testing$text)) %>% 
  as.tibble() %>% 
  mutate(class = as.factor(ifelse(value < .5, "MoroDefenders", "#MoroCriminoso")))

caret::confusionMatrix(as.factor(testing$term), pred$class)

sentences <- head(testing[testing$term == "MoroDefenders", "text"], 10)

explainer <- lime(training$text, xgb_model, get_matrix)

save(explainer, file = "explainer.RData")

#load("explainer.RData")

aux <- lime::explain(sentences[7], explainer, labels = 1, n_features = 5)

aux$label_prob[1]

interactive_text_explanations(explainer)
