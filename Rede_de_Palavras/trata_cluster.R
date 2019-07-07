library(rtweet)
library(purrr)
df_tl<- rtweet::get_timeline(475381701)


df_tweets %>%         
  group_by(termo, user_id) %>%
  summarise(
    contagem= n()
  ) %>%
  arrange(desc(contagem)) %>%
  group_by(termo) %>%
  summarise(
    conta_user= n()
  ) %>% summarise(
    sum(conta_user)
  )


df_tweets %>%
  select("termo", usuario = distinct("user_id"))
         

df_termo_user<- df_tweets %>%
  distinct(termo,user_id) 


(df_trabalho %>% filter(termo=="#eutocomomoro")%>% distinct(user_id))$user_id[1:75]


df_trabalho<- df_termo_user %>%
              filter(termo %in% c("#eutocomomoro","#morocriminoso"))

726769395436019713

cont<<-0

df_cluster<- map_dfr(unique(df_trabalho$termo), function(termo_q){
  

  
  df_influencers<- map_dfr((df_trabalho %>% filter(termo==termo_q)%>% distinct(user_id))$user_id[1:75], function(user_t){
    
    
    cont<<- cont+1
    
    print(user_t)
    print((cont/15000)*100)
    
    
    df_tl<- try(rtweet::get_timeline(user_t))
    if(inherits(df_tl,"try-error") ){
      return()
    } else if( length(df_tl)==0){
      return()
    }
    
    influencers<- unlist(df_tl$mentions_screen_name)
    tibble( nome=rep(user_t,length(influencers)), influencers= influencers)
    
    
  })
  
  df_influencers$termo <- rep(termo_q,NROW(df_influencers))
  df_influencers
})

quantile((df_cluster %>%
        filter(!is.na(influencers)) %>%
  group_by(termo, influencers) %>%
  summarise(
    contagem = n()

  ))$contagem,0.75)
  



library(forcats)

df_factor_influencers<-
  
  df_cluster %>%
  filter(!is.na(influencers)) %>%
  group_by(influencers) %>%
  summarise(
    contagem = n()
    
  ) %>%
  #arrange(desc(contagem))%>%
  mutate(influencers=fct_reorder(influencers,contagem))

factor(data$Carreira, levels = data$Carreira[order(data$Valor)])

df_cluster$influencers<- factor(df_cluster$influencers, levels = df_factor_influencers$influencers)

df_factor_influencers<- df_cluster %>%
  filter(!is.na(influencers)) %>%
  group_by(influencers) %>%
  summarise(
    contagem = n()
    
  ) %>%
  arrange(desc(contagem))%>%
  mutate(influencers=fct_reorder(influencers,contagem)) 



#As duas hastags
df_cluster %>%
  filter(!is.na(influencers)) %>% 
  group_by(termo, influencers) %>%
    summarise(
      contagem = n()
    ) %>%
    arrange(influencers) %>%
  top_n(20) %>%
   ggplot(aes(x= influencers, y=contagem)) +
  geom_col(fill= "lightblue") +
  facet_grid(termo ~ .) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

 
#Somente hashtag #eutocommoro
df_cluster %>%
  filter(!is.na(influencers),
         termo == "#eutocomomoro") %>% 
  group_by(termo, influencers) %>%
  summarise(
    contagem = n()
  ) %>%
  arrange(influencers) %>%
  top_n(20) %>%
  ggplot(aes(x= influencers, y=contagem)) +
  geom_col(fill= "lightblue") +
  #facet_grid(termo ~ .) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  )


#Somente hashtag #morocriminoso
df_cluster %>%
  filter(!is.na(influencers),
         termo == "#morocriminoso") %>% 
  group_by(termo, influencers) %>%
  summarise(
    contagem = n()
  ) %>%
  arrange(influencers) %>%
  top_n(20) %>%
  ggplot(aes(x= influencers, y=contagem)) +
  geom_col(fill= "lightblue") +
  #facet_grid(termo ~ .) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  )


save(list = "df_cluster", file = "~/GitHub/vazajato/Rede_de_Palavras/data/df_cluster.RData")



#1113875475905036291
#	726769395436019713
#3029339182
df_tl_quali<- try(rtweet::get_timeline("3029339182"))
df_tl_quali_res <- df_tl_quali %>%
                select(screen_name, status_id,mentions_screen_name, )
