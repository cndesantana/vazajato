library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS("subset_tweets_Moro_0907.rds")
top20 <- dat %>% 
	filter(!is.na(hashtags)) %>% 
	unnest(hashtags) %>% 
	mutate(hashtags = toupper(hashtags)) %>% 
	mutate(data = round_date(created_at, "1 day")) %>% 
	group_by(hashtags) %>% 
	summarise(total = n()) %>% 
	arrange(total) %>% 
	tail(20) %>% 
	select(hashtags)

p1 <- dat %>% 
	filter(!is.na(hashtags)) %>% 
	unnest(hashtags) %>% 
	mutate(hashtags = toupper(hashtags)) %>% 
	mutate(hashtags=factor(hashtags, levels = c("LULANACADEIA",
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
						    "VAZAJATO"))) %>%
	filter(hashtags %in% (top20$hashtags)) %>% 
	mutate(data = round_date(created_at, "1 day")) %>% 
	group_by(data, hashtags) %>% 
	summarise(total = n()) %>% 
	tidyr::spread(key = hashtags, value = total, fill = 0) %>%  
	tidyr::gather(key = hashtags, value = total, - data) %>% 
	arrange(data,hashtags) %>% 
	ggplot(aes(x = data, y = total)) + 
	geom_area(aes(fill=hashtags),position='fill') + 
	theme(legend.position="bottom") + 
	labs(y = "Número de tuítes contendo hashtag", title = "Timeline das hashtags - Top 20", fill = "")+ 
	scale_colour_brewer(palette = "Spectral")

png("area_hashtags.png",width=3200,height=1800,res=300)
print(p1)
dev.off()
