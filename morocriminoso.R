
# Carregando Librarys: 

library(pacman)

p_load(caret, tidyverse, factoextra, epubr, tm, lexiconPT, broom, tidytext, widyr, irlba, 
       Rtsne,plotly, forcats, rsample, glmnet)

# Carregando

load("data/ThousandMiles.RData")

moro_criminoso <- df_tweets %>% 
  filter(term == "#MoroCriminoso") %>% 
  distinct_all()

### Carregando

load("data/ThousandMoroDefenders.RData")

moro_defenders <- df_tweets %>% 
  distinct_all()

table(df_tweets$term)

# Juntando

df <- bind_rows(moro_criminoso, moro_defenders) 

# Selecionando Stopwords: 

stop_words <- stopwords(kind = "pt") %>% 
  as.tibble()

# Contando palavras

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

# Nosso Glmnet que fará algo parecido a uma regressão logistica: 

model <- glmnet::cv.glmnet(sparse_words, moro_criminoso,
                           family = "binomial",
                           keep = TRUE, 
                           intercept = FALSE)
coef(model)

coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

# save(coefs, file = "coefs.RData")

#Plot MEDIUM

coefs %>%
  group_by(estimate > 0) %>%
  top_n(20, abs(estimate)) %>%
  ungroup() %>%
  mutate(x = fct_reorder(term, estimate)) %>% 
  ggplot(aes(x =fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(y = "Estimativa", x = "Termos")
  
  
ggsave("coef_termos.jpg")  
  
# testando

coefs %>% 
  filter(term == "deus")

# Encontrando o intercepto


# Realizando a classificação, ou seja, qual a probabilidade de tal paragráfo, do grupo de teste, ser do livro "O Estrangeiro"

classifications <- unigram_probs %>%
  inner_join(test_data) %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(id_) %>%
  summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(score))  # Retornar a probabilidade 

head(classifications)

# resultados

test_df <- test_data %>% 
  filter(id_ %in% classifications$id_) %>% 
  distinct()


test <- inner_join(classifications,test_df) %>% 
  mutate(term = as.factor(term))

### Escolhendo 0.5 como threshold para classificação:

test <- test %>%
  mutate(
    prediction = case_when(
      probability > 0.5 ~ "#MoroCriminoso",
      TRUE ~ "EuApoioaLavaJato"
    ),
    prediction = as.factor(prediction))

test



caret::confusionMatrix(test$prediction, test$term)


