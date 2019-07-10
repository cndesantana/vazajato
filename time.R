### Library

library(pacman)

p_load(tidyverse,reshape2, lubridate, scales)

# Load database

# df <- readRDS("data/datavazajato/vazajato.rds")

# Select

# nword numero de palavras por hora

point_time <- function(df, n_word, inicio, fim){ 

  opt <- theme_bw()+
    theme(axis.title = element_text(face = "bold", color = "black", size = 10),
          axis.text.x = element_text(face = "plain", color = "black", 
                                     size = 10, angle = 90),
          axis.text.y = element_text(face = "plain", color = "black", size = 10),
          legend.text=element_text(size=12, face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"))
  
  df_hash <- df %>% filter(is_retweet == FALSE) %>% 
  unnest(hashtags) %>% filter(!is.na(hashtags)) %>% 
  mutate(date = floor_date(created_at, "hour")) %>%
  filter(date >= inicio) %>% 
  filter(date <= fim) %>% 
  group_by(date, hashtags) %>% 
  summarise(n = n()) %>% top_n(n_word,n) %>%  
  arrange(date)

# Determinando inicio e fim

datebreaks <- seq(floor_date(inicio, "day"), 
                  floor_date(fim, "day"), by = "12 hours")
                             
  
ggplot(df_hash, aes(x = ymd_hms(date), y = hashtags, 
                    size = n)) +
  geom_point() +
  scale_x_datetime(breaks= datebreaks)  +
  theme_minimal() + 
  opt

  
}

point_time(df, n_word = 5, inicio = min(df$created_at), 
           fim = max(df$created_at))
