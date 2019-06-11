# Carregando dados

load("df_tweets_10000_vazajato.Rdat")

# Carregando Librarys: 

library(pacman)

p_load(caret, tidyverse, factoextra, epubr, tm, lexiconPT, broom, tidytext, widyr, irlba, 
       Rtsne,plotly)

require(forcats)

# Quais palavras estão mais próximas de cada um dos 4 termos pesquisados:


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
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n)) %>% 
  filter(str_detect(word, "@") == FALSE) %>% 
  filter(str_detect(word, "https")== FALSE)

head(unigram_probs)

# skipgrams

tidy_skipgrams <- df_tweets %>%
  tidytext::unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words, by= c("word" = "value")) %>% 
  filter(!word %in% c("rt", "é", "vagas", "disponíveis", "286", 
                      "adéli", "@makavelijones", "cristiane", "=gt")) %>% 
  filter(str_detect(word, "@") == FALSE) %>% 
  filter(str_detect(word, "https")== FALSE) %>% 
  mutate(ngramID = id) %>% 
  unite(skipgramID, ngramID) 

# Calculando a probabilidade de duas palavras se encontrarem no mesmo twitter:

skipgram_probs <- tidy_skipgrams %>%
  widyr::pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  dplyr::mutate(p = n / sum(n))

# Criando Indicador: 

index_prob <- skipgram_probs %>%
  dplyr::filter(n > 20) %>%
  dplyr::rename(word1 = item1, word2 = item2) %>%
  dplyr::left_join(unigram_probs %>%
                     select(word1 = word, p1 = p),
                   by = "word1") %>%
  dplyr::left_join(unigram_probs %>%
                     select(word2 = word, p2 = p),
                   by = "word2") %>%
  dplyr::mutate(p_together = p / p1 / p2)


head(index_prob)

# Realizando PCA para determinar as palavras mais similares

pmi_matrix <- index_prob %>%
  dplyr::mutate(pmi = log10(p_together)) %>%
  tidytext::cast_sparse(word1, word2, pmi)

pmi_pca <- irlba::prcomp_irlba(pmi_matrix, n = 256)

word_vectors <- pmi_pca$x

rownames(word_vectors) <- rownames(pmi_matrix)

# save(word_vectors, file = "word_vectors.RData") # para shiny

# Encontrando similaridades: 

search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}

# Similaridades a Moro



p1 <- search_synonyms(word_vectors, word_vectors["moro",]) %>% 
  top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = similarity)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(title = "Palavras Ligadas a Moro")

p2 <- search_synonyms(word_vectors, word_vectors["dallagnol",]) %>% 
  top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = similarity)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(title = "Palavras Ligadas a Dallagnol")

p3 <- search_synonyms(word_vectors, word_vectors["#vazajato",]) %>% 
  top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = similarity)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(title = "Palavras Ligadas a #vazajato")

p4 <- search_synonyms(word_vectors, word_vectors["intercept",]) %>% 
  top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = similarity)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(title = "Palavras Ligadas a Intercept")


gridExtra::grid.arrange(p1,p2,p3,p4)



p5 <- search_synonyms(word_vectors, word_vectors["lula",]) %>% 
  top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = similarity)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(title = "Palavras Ligadas a Lula")

p6 <- search_synonyms(word_vectors, word_vectors["bolsonaro",]) %>% 
  top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = similarity)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(title = "Palavras Ligadas a Bolsonaro")

p7 <- search_synonyms(word_vectors, word_vectors["haddad",]) %>% 
  top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = similarity)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(title = "Palavras Ligadas a Haddad")

p8 <- search_synonyms(word_vectors, word_vectors["eleição",]) %>% 
  top_n(20, abs(similarity)) %>% 
  ggplot(aes(x = reorder(token, similarity), y = similarity, fill = similarity)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(title = "Palavras Ligadas a Eleição")


gridExtra::grid.arrange(p5, p6, p7, p8)
