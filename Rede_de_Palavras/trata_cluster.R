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

df_trabalho<- map_dfr(unique(df_trabalho$termo), function(termo_q){
  

  
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
    
    #influencers<- unlist(df_tl$mentions_screen_name)
    #tibble( nome=rep(user_t,length(influencers)), influencers= influencers)
    
    df_tl
    
    
  })
  
  df_influencers$termo <- rep(termo_q,NROW(df_influencers))
  df_influencers
})



#df_trabalho <- df_cluster


cont<<-0
df_influencers <- map_dfr(df_trabalho$status_id,function(status){
  
  cont<<-cont+1
  
  print(status)
  print((cont/NROW(df_trabalho))*100)
  
  
  influencers <- unlist((df_trabalho%>% filter(status_id == status) %>% select(mentions_screen_name))$mentions_screen_name)
  tibble( status=rep(status,length(influencers)), influencers= influencers)
  
})

glimpse(df_influencers)

names(df_influencers)[1] <- "status_id"

df_cluster<- df_influencers %>%
  inner_join(df_trabalho)

df_cluster_copy <- df_cluster



library(forcats)

df_factor_influencers<-
  
  df_cluster %>%
  filter(!is.na(influencers)) %>%
  group_by(influencers) %>%
  summarise(
    contagem = n()
    
  ) %>%
  arrange(desc(contagem))%>%
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
top_influencers<- df_cluster %>%
  filter(!is.na(influencers)) %>% 
  group_by(influencers) %>%
    summarise(
      contagem = n()
    ) %>%
    arrange(influencers) %>%
  top_n(20)

df_cluster %>%
  filter(influencers %in% top_influencers$influencers) %>%
  group_by(termo, influencers) %>%
  summarise(
    contagem = n()
  ) %>%
  arrange(influencers) %>%
   ggplot(aes(x= influencers, y=contagem, fill=termo)) +
  geom_col() +
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


df_cluster %>%
  filter(influencers %in% top_influencers$influencers) %>%
  mutate(tipo_mencao = ifelse(is.na(reply_to_screen_name),"Retuíte",ifelse(reply_to_screen_name==influencers,"Resposta","Adição"))) %>%
  #filter(tipo_mencao == "Retuíte")%>%
  group_by(termo, influencers, tipo_mencao) %>%
  ggplot(aes(x= influencers, fill= tipo_mencao)) +
  geom_bar() +
  facet_grid(termo ~ .) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

df_cluster %>%
  filter(influencers %in% top_influencers$influencers) %>%
  mutate(tipo_mencao = ifelse(is.na(reply_to_screen_name),"Retuíte",ifelse(reply_to_screen_name==influencers,"Resposta","Adição"))) %>%
  filter(tipo_mencao == "Retuíte")%>%
  group_by(termo, influencers, tipo_mencao) %>%
  ggplot(aes(x= influencers, fill= termo)) +
  geom_bar() +
  #facet_grid(termo ~ .) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  )



#1113875475905036291
#	726769395436019713
#3029339182
df_tl_quali<- try(rtweet::get_timeline("3029339182"))
df_tl_quali_res <- df_tl_quali %>%
                   filter(!is.na(mentions_screen_name))
  
  
                select(screen_name, status_id,mentions_screen_name, text)
