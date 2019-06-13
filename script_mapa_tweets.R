### Script usado para produzir animações e figuras para post de DataSCOUT sobre a #VazaJato
### Autor: Charles Novaes de Santana
###        
### Email: charlesn@ethz.ch
### Data: 30.09.2018


library(dplyr)
library(ggplot2)
library(lubridate)

#carregar os data.frames contendo os tweets
load("df_tweets_10000_vazajato.Rdat")
#load("ThousandMiles.RData")
#load("Thousandlies.RData")

p1 <- df_tweets %>% 
  mutate(data = lubridate::round_date(created, "minute")) %>% 
  count(data, term) %>% 
  ggplot(aes(data, n)) +
  geom_line() +
  xlab("Horário (H:M:SS)") + 
  ylab("Número de tuítes por minuto") + 
  ggtitle("Número de tuítes contendo hashtags relacionadas ao #VazaJato (10.06.19)")+
  facet_wrap(~term)

png("TweetsPerMinute_VazaJato.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


p2 <- df_tweets %>%
  group_by(term) %>%
  summarise(tot=sum(retweetCount)) %>%
  ggplot(aes(x=reorder(term,tot), tot, fill =term)) + 
  geom_text(aes(x=reorder(term,tot), y = tot, label = paste0(signif(tot/1000000,2)," Millions") ),vjust = -0.5, size=2.8)+
  geom_bar(stat="identity") + 
  xlab("Termos") + 
  ylab("Número de retuítes") + 
  ggtitle("Número de retuítes contendo hashtags relacionadas ao #VazaJato (10.06.19)")

png("totalRetweets_VazaJato.png",width=3200,height=1800,res=300)
print(p2)
dev.off()

df_vaza <- df_tweets %>% filter(term == "#VazaJato") %>% group_by(term, screenName) %>% summarise(tot = sum(retweetCount)) %>% arrange(tot) %>% tail(30)
df_moro <- df_tweets %>% filter(term == "Moro") %>% group_by(term, screenName) %>% summarise(tot = sum(retweetCount)) %>% arrange(tot) %>% tail(30)
df_dala <- df_tweets %>% filter(term == "Dallagnol") %>% group_by(term, screenName) %>% summarise(tot = sum(retweetCount)) %>% arrange(tot) %>% tail(30)
df_inter <- df_tweets %>% filter(term == "Intercept") %>% group_by(term, screenName) %>% summarise(tot = sum(retweetCount)) %>% arrange(tot) %>% tail(30)

df <- rbind(df_vaza, df_moro, df_dala, df_inter)
df <- df %>%
  mutate(screenName = factor(screenName, levels = rev(unique(df$screenName)))) %>%
  ungroup() %>%   # As a precaution / handle in a separate .grouped_df method
  arrange(term, tot) %>%   # arrange by facet variables and continuous values
  mutate(.r = row_number()) # Add a row number variable

p3 <- df %>% 
  ggplot(aes(x=.r, y=tot, fill = term)) +  # Use .r instead of x
  geom_col(show.legend=FALSE) +
  geom_text(aes(x = .r, y = tot, label = paste0(tot%/%1000,"k") ),vjust = 0, hjust = 0, size=2.8)+
  coord_flip() +
  scale_x_continuous(  # This handles replacement of .r for x
    breaks = df$.r,     # notice need to reuse data frame
    labels = df$screenName
  ) +
  theme(axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8))+
  xlab("Usuários") + ylab("Número de Retuítes") +
  facet_wrap(~ term, scales = "free")  # Should free scales at x-axis(though could be left to user)


png("influenciadores_VazaJato.png",width=3200,height=1800,res=300)
print(p3)
dev.off()

