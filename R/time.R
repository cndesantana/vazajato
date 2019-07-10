### Library

library(pacman)

p_load(tidyverse,reshape2, lubridate, scales)

# Load database

# df <- readRDS("data/datavazajato/vazajato.rds")

# Select

# nword numero de palavras por hora

point_time <- function(df, n_word, inicio, fim, prop){ 

 top20hashtags <- c("LULANACADEIA",
		    "SOMOSTODOSMORO",
		    "DIA30PELOBRASIL",
		    "DIA30VEMPRARUA",
		    "BRASILNASRUAS",
		    "DETONATUDOMORO",
		    "MORO1MILHAO",
		    "MOROORGULHODOBRASIL",
		    "MOROMAOSLIMPAS",
		    "MORO",
		    "LAVAJATO",
		    "MOROCRIMINOSO",
		    "MOROCORRUPTO",
		    "MOROSUACASACAIU",
		    "MOROMENTIU",
		    "EUNAOCONFIONOMORO",
		    "LULALIVREURGENTE",
		    "LULALIVRE",
		    "TONTOSDOMBL",
		    "VAZAJATO")

 df_hash <- df %>% 
	  mutate(date = floor_date(created_at, "hour")) %>%
	  filter(date >= inicio) %>% 
	  filter(date <= fim) %>%
	  filter(!is.na(hashtags)) %>%
	  unnest(hashtags) %>% 
	  mutate(hashtags = toupper(hashtags)) %>%
	  filter(hashtags %in% top20hashtags) %>%  
	  group_by(date, hashtags) %>% 
	  summarise(n = n()) %>% top_n(n_word,n) %>%  
	  arrange(date)

  save(df_hash, file = "df_hash.rds")
# Determinando inicio e fim

  datebreaks <- seq(floor_date(inicio, "day"), 
                    floor_date(fim, "day"), by = "12 hours")
                               
# Caracteristias do plot
  opt <- theme_bw()+
    theme(axis.title = element_text(face = "bold", color = "black", size = 10),
          axis.text.x = element_text(face = "plain", color = "black", 
                                     size = 10, angle = 90),
          axis.text.y = element_text(face = "plain", color = "black", size = 10),
          legend.text=element_text(size=12, face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          strip.text.x = element_text(size = 12, face = "bold"),
          strip.text.y = element_text(size = 12, face = "bold"))
  
 

  p1 <- ggplot(df_hash, aes(x = ymd_hms(date), y = hashtags, 
                      size = n)) +
    geom_point() +
    scale_x_datetime(breaks= datebreaks)  +
    theme_minimal() + 
    opt
  
  png(paste0("timeline_",prop,".png"),width=3200,height=1800,res=300)
  print(p1)
  dev.off()
}

args = commandArgs(trailingOnly=TRUE)
prop <- as.numeric(args[1])
df <- readRDS("datavazajato/todashashtagspraarea.rds")
randpos <- sample(1:nrow(df),round(0.1*nrow(df)), replace=FALSE)
df <- df[randpos,]
point_time(df, n_word = 5, inicio = min(df$created_at), fim = max(df$created_at), prop)
