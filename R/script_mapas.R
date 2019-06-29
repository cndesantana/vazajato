library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(brazilmaps)

df <- readRDS("tweets_Moro_Lula.rds")

top_hashtags <- df %>% 
  unnest(hashtags) %>% 
  filter(!is.na(hashtags)) %>% 
  group_by(hashtags) %>% 
  summarise(total = n()) %>% 
  arrange(total) %>%
  filter(rank(desc(total)) <= 30) %>%
  select(hashtags)

## network approach
hash_user_time <- df %>% 
  unnest(hashtags) %>% 
  filter(!is.na(hashtags)) %>%
  select(hashtags, screen_name, created_at)

## map of tweets and hashtags
place_hashtag_time <- df %>% 
  unnest(hashtags) %>% 
  filter(!is.na(hashtags)) %>%
  select(hashtags, place_name, place_type,place_full_name,country,location, created_at)
## separando os tuítes contendo hashtags pro-Moro

WorldData <- map_data('world') %>% 
  filter(region != "Antarctica") %>% 
  fortify
hash_antimoro <- toupper(c("TontosDoMBL",
                       "Lula",
                       "LulaLivre",
                       "VazaJato",
                       "BolsoNarcos",
                       "LulaLivreUrgente"))
hash_moro <- toupper(c("Dia30AceleraBrasil",
                       "DomingoNasRuas",
                       "Dia30VemPraRua",
                       "LulaNaCadeia",
                       "Dia30NasRuas",
                       "Moro"))
## separando os tuites contendo hashtags pró- e contra- Moro

p0 <- place_hashtag_time %>% 
  mutate(hashtags = toupper(hashtags)) %>% 
  group_by(hashtags) %>% 
  summarise(total = n()) %>% 
  arrange(total) %>% tail(13) %>% 
  filter(hashtags != "BRASIL") %>% 
  mutate(grupo = if_else(hashtags %in% toupper(hash_moro), "pró-Moro", "anti-Moro")) %>% 
  ggplot(aes(x = reorder(hashtags,total), y = total, fill = grupo)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  labs(fill = "Groupo da Hashtag",y = "Número de tuítes", x = "hashtags", title = "Hashtags mais citadas") + 
  geom_text(aes(x = reorder(hashtags,total), y = total, label = total), hjust=-0.01) + 
  theme(legend.position="bottom")
png("hashtags_mais_citadas.png",width=3200,height=1800,res=300)
print(p0)
dev.off()

df1 <- data.frame(region=names(table(place_hashtag_time %>% 
                                      filter(!is.na(country)) %>% 
                                      filter(toupper(hashtags) %in%  hash_lula) %>%
                                      select(country))), 
                 value=as.numeric(table(place_hashtag_time %>%
                                          filter(!is.na(country)) %>% 
                                          filter(toupper(hashtags) %in%  hash_lula) %>%
                                          select(country))),
                 stringsAsFactors=FALSE)

df2 <- data.frame(region=names(table(place_hashtag_time %>% 
                                       filter(!is.na(country)) %>% 
                                       filter(toupper(hashtags) %in%  hash_moro) %>%
                                       select(country))), 
                  value=as.numeric(table(place_hashtag_time %>%
                                           filter(!is.na(country)) %>% 
                                           filter(toupper(hashtags) %in%  hash_moro) %>%
                                           select(country))),
                  stringsAsFactors=FALSE)


p1 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = df1, map=WorldData,
           aes(fill=value, map_id=region),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
    scale_x_continuous(breaks=c()) +
  labs(fill="Tuítes", title="Países de onde vieram tuítes com hashtags contrárias a Moro", x="", y="") +
  theme_bw()
png("paises_com_tuites_antimoro.png",width=3200,height=1800, res = 300)
print(p1)
dev.off()

p2 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = df2, map=WorldData,
           aes(fill=value, map_id=region),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Tuítes", title="Países de onde vieram tuítes com hashtags favoráveis a Moro", x="", y="") +
  theme_bw()
png("paises_com_tuites_moro.png",width=3200,height=1800, res = 300)
print(p2)
dev.off()


##
library(brazilmaps)

tb_cities <- place_hashtag_time %>% filter(toupper(country) == "BRAZIL") %>% select(place_name) %>% table()
df3 <- data.frame(mun=toupper(names(tb_cities)), 
                  value=as.numeric(tb_cities),
                  stringsAsFactors=FALSE)
muni_map <- brazilmaps::get_brmap("City")
muni_map2 <- muni_map %>% 
  left_join(df3, c("nome" = "mun"))
muni_map3 <- muni_map2 %>%
  group_by(State) %>%
  summarise(value_state = sum(value, na.rm=TRUE)) %>%
  ungroup()

tb_cities_antimoro <- place_hashtag_time %>% filter(toupper(hashtags) %in%  hash_lula) %>% filter(toupper(country) == "BRAZIL") %>% select(place_name) %>% table()
df4 <- data.frame(mun=toupper(names(tb_cities_antimoro)), 
                  value=as.numeric(tb_cities_antimoro),
                  stringsAsFactors=FALSE)
muni_map4 <- muni_map %>% 
  left_join(df4, c("nome" = "mun"))
muni_map4 <- muni_map4 %>%
  group_by(State) %>%
  summarise(value_state = sum(value, na.rm=TRUE)) %>%
  ungroup()
state_map <- muni_map4 %>%
  ggplot() +
  geom_sf(aes(fill = value_state),
          colour = "black", 
          size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Número total de tuítes contendo hashtags críticas a Moro")
png("tuites_antimoro_por_estado.png",width=3200,height=1800,res=300)
print(state_map)
dev.off()

tb_cities_promoro <- place_hashtag_time %>% filter(toupper(hashtags) %in%  hash_moro) %>% filter(toupper(country) == "BRAZIL") %>% select(place_name) %>% table()
df5 <- data.frame(mun=toupper(names(tb_cities_promoro)), 
                  value=as.numeric(tb_cities_promoro),
                  stringsAsFactors=FALSE)
muni_map5 <- muni_map %>% 
  left_join(df5, c("nome" = "mun"))
muni_map5 <- muni_map5 %>%
  group_by(State) %>%
  summarise(value_state = sum(value, na.rm=TRUE)) %>%
  ungroup()
state_map <- muni_map5 %>%
  ggplot() +
  geom_sf(aes(fill = value_state),
          colour = "black", 
          size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Número total de tuítes contendo hashtags favoráveis a Moro")
png("tuites_promoro_por_estado.png",width=3200,height=1800,res=300)
print(state_map)
dev.off()


state_map <- muni_map3 %>%
  ggplot() +
  geom_sf(aes(fill = value_state),
          colour = "black", 
          size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Número total de tuítes contendo termos 'Moro' ou 'Lula'")
png("tuites_por_estado.png",width=3200,height=1800,res=300)
print(state_map)
dev.off()


df6 <- left_join(df5, df4, by = c("mun","mun"))
names(df6) <- c("mun","pro","contra")
muni_map6 <- muni_map %>% 
  left_join(df6, c("nome" = "mun"))
muni_map6 <- muni_map6 %>%
  group_by(State) %>%
  summarise(total_pro = sum(pro, na.rm=TRUE),
            total_contra = sum(contra, na.rm=TRUE),
            value_state = signif(100*total_contra/(total_contra + total_pro),2)) %>%
  ungroup()
state_map <- muni_map6 %>%
  ggplot() +
  geom_sf(aes(fill = value_state),
          colour = "black", 
          size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = 2) +
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Proporção de tuítes do Estado contendo hashtags críticas a Moro") +
  labs(fill="Proporção (%)")
png("proporcaotuites_antimoro_por_estado.png",width=3200,height=1800,res=300)
print(state_map)
dev.off()

################3
