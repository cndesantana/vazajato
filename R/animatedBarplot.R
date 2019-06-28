library(tidyverse)
library(gganimate)
library(lubridate)
library(tidytext)

setwd("/home/charles/GitRepos/vazajato/vazajato/datavazajato/")

getTidyGDP <- function(tweets){
  tweets$created_at <- floor_date(tweets$created_at,"15 minutes")
  tweets <- tweets %>% arrange(created_at)
  unique_created_at <- unique(tweets$created_at)
# we want the columns country_name value year
  tweets_pre_tidy <- data.frame()
  for(it in 1:length(unique_created_at)){
  #para cada minuto, identificar as hashtags e formata-las como se fossem uma frase
     t <- unique_created_at[it]
     myhashtags <- tweets %>% filter(created_at == t) %>% select(hashtags)
     sentence_hashtags <- paste(paste0("#",as.character(unlist(myhashtags)[!is.na(unlist(myhashtags))])),sep=" ",collapse=" ")
     if(it == 1){
        tweets_pre_tidy <- data.frame(sentences = sentence_hashtags, time = t)
     }else{
        tweets_pre_tidy <- rbind(tweets_pre_tidy, data.frame(sentences = paste_into_sentence(sentence_hashtags, as.character(tweets_pre_tidy$sentences[it-1])), time = t))
        #tweets_pre_tidy <- rbind(tweets_pre_tidy, data.frame(sentences = sentence_hashtags, time = t))
     }
  }
  tweets_tidy <- data.frame()
  #para cada "frase" das hashtags, calcular o unigram, para identificar o ranking das hashtags. guardar o unigrama (hashtag, value) e o tempo em um data.frame
  df_unigram <- 
          tweets_pre_tidy %>% 
          group_by(time) %>%
          unnest_tokens(word, sentences, token = "ngrams", n = 1) %>%
	  count(word, sort = TRUE)      
  #para o minuto seguinte, acumular a frase com a frase anterior. Assim vamos computar o acumulado de citacoes de cada hashtag
  names(df_unigram) <- c("year","country_name","value")
  return(df_unigram)
}

fromTwitterToHashtagAnimation <- function(tweets, output = "gif"){
   tweets_tidy <- getTidyGDP(tweets)
   tweets_tidy <- tweets_tidy %>% filter(year >= ymd_hms("2019-06-19 10:00:00"))
   tweets_tidy <- tweets_tidy %>% filter(year <= ymd_hms("2019-06-19 20:00:00"))

   animateTidyWords(tweets_tidy, output, filename = "animation_hashtag",mysubtitle = "Top 10 Hashtags em tuítes contendo termo 'Moro'", mycaption = "Dadoscope - https://medium.com/dadoscope")
} 

animateTidyWords <- function(tweets_tidy, output = "gif", filename = "animation", mysubtitle = "", mycaption = ""){
   formatted_tweets <- tweets_tidy %>%
     group_by(year) %>%
     mutate(rank = rank(-value) * 1,
            Value_rel = value/value[rank==1],
            Value_lbl = paste0(" ",value)) %>%
     group_by(country_name) %>%
     filter(rank <=10) %>%
     ungroup()
   maxvalue <- max(as.numeric(formatted_tweets$value))
   cat("Saving RDS file",sep="\n")
   saveRDS(formatted_tweets, file = "df_formatted.rds") 
   formatted_tweets <- formatted_tweets %>% filter(rank <= 10)
   formatted_tweets <- formatted_tweets %>% 
	   group_by(country_name) %>%
	   arrange(year) %>%
	   mutate(prev.rank = lag(rank)) %>%
	   ungroup() %>%
	   group_by(year) %>%
	   arrange(rank, prev.rank) %>%
	   mutate(x = seq(1, n())) %>%
	   ungroup()
   staticplot = ggplot(formatted_tweets, aes(x=x, y = value,group = country_name, fill = as.factor(country_name), color = as.factor(country_name))) + 
#	   geom_tile(aes(y = value/2, height = value, width = 0.9)) + 
	   geom_col()+ 
	   geom_text(aes(y = 0, label = paste(country_name," ")),col="black",vjust = 0.2,hjust = 1, size = 8)+
	   geom_text(aes(y = value, label = Value_lbl, hjust = +1),size=8, col = "black") +
	   scale_y_continuous(labels = scales::comma, limits=c(0,maxvalue)) + 
	   scale_x_reverse() +
	   coord_flip(expand = FALSE, clip="off") + 
	   theme(axis.line = element_blank(), 
		 axis.text.x=element_blank(), 
		 axis.text.y=element_blank(), 
		 axis.ticks = element_blank(), 
		 axis.title.x = element_blank(), 
		 axis.title.y = element_blank(), 
		 legend.position = "none", 
		 panel.background=element_blank(), 
		 panel.border=element_blank(),
		 panel.grid.major=element_blank(),
		 panel.grid.minor=element_blank(),
		 plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
		 plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
		 plot.caption =element_text(size=10, hjust=0.5, face="italic", color="grey"), 
		 plot.margin = margin(2,2, 2, 10, "cm"))
   anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
   view_follow(fixed_x = FALSE)  +
   labs(title = 'Moro no Senado {closest_state}',
        subtitle  =  mysubtitle,
        caption  = mycaption)

   if(output == "gif"){
      # For GIF
      animate(anim, length(formatted_tweets$year), fps = 10,  width = 977, height = 550,
              renderer = gifski_renderer("animation_lowres.gif"))
   }
   else{
      # For MP4
      animate(anim,length(formatted_tweets$year), fps = 10,  width = 3200, height = 1800,
              renderer = ffmpeg_renderer()) -> for_mp4
      anim_save("animation.mp4", animation = for_mp4 )
   }
}

paste_into_sentence <- function(a, b){
   c <- paste(a, b,sep=" ",collapse=" ")
   return(c)
}

#to test
#files <- c("tweets_Moro_201906191010.rds", "tweets_Moro_201906191110.rds", "tweets_Moro_201906191210.rds", "tweets_Moro_201906191230.rds","tweets_Moro_201906191250.rds","tweets_Moro_201906191310.rds","tweets_Moro_201906191330.rds","tweets_Moro_201906191350.rds","tweets_Moro_201906191410.rds","tweets_Moro_201906191430.rds","tweets_Moro_201906191450.rds","tweets_Moro_201906191510.rds","tweets_Moro_201906191530.rds","tweets_Moro_201906191550.rds","tweets_Moro_201906191730.rds","tweets_Moro_201906192110.rds")
files<-system("ls tweets_Moro*.rds",intern=TRUE)
tweets <- data.frame()
#files <- files[-c(1:11,144)]
for(f in files){
  cat(f,sep="\n")
  aux <- readRDS(f)
  if(ncol(aux) >= 90){
    tweets <- rbind(tweets, aux[,1:90])
  }
}
fromTwitterToHashtagAnimation(tweets)
