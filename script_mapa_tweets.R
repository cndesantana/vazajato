### Script usado para produzir animações e figuras para post de DataSCOUT sobre a #VazaJato
### Autor: Charles Novaes de Santana
###        
### Email: charlesn@ethz.ch
### Data: 30.09.2018


library(maptools)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(OpenStreetMap)
library(lubridate)

#carregar os data.frames contendo os tweets
load("df_tweets_10000_vazajato.Rdat")

p1 <- df_tweets %>% 
  mutate(data = lubridate::round_date(created, "minute")) %>% 
  count(data, term) %>% 
  ggplot(aes(data, n)) +
  geom_line() +
  xlab("Horário (H:M:SS)") + 
  ylab("Tweets por minuto") + 
  ggtitle("Tweets contendo hashtags relacionadas ao #VazaJato (10.06.19)")+
  facet_wrap(~term)

png("TweetsPerSecond_VazaJato.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


p2 <- df_tweets %>% 
  group_by(term) %>%
  summarise(tot=sum(retweetCount)) %>%
  ggplot(aes(x=term, tot, fill =term)) + 
  geom_bar(stat="identity") + 
  xlab("Termos") + 
  ylab("Retweets") + 
  ggtitle("Número de retweets contendo hashtags relacionadas ao #VazaJato (10.06.19)")

png("totalRetweets_VazaJato.png",width=3200,height=1800,res=300)
print(p2)
dev.off()


df_vaza <- df_tweets %>% filter(term == "#VazaJato") %>% group_by(term, screenName) %>% summarise(tot = sum(retweetCount)) %>% arrange(tot) %>% tail(30)
df_moro <- df_tweets %>% filter(term == "Moro") %>% group_by(term, screenName) %>% summarise(tot = sum(retweetCount)) %>% arrange(tot) %>% tail(30)
df_dala <- df_tweets %>% filter(term == "Dallagnol") %>% group_by(term, screenName) %>% summarise(tot = sum(retweetCount)) %>% arrange(tot) %>% tail(30)
df_inter <- df_tweets %>% filter(term == "Intercept") %>% group_by(term, screenName) %>% summarise(tot = sum(retweetCount)) %>% arrange(tot) %>% tail(30)

df <- rbind(df_vaza, df_moro, df_dala, df_inter)

p3 <- df %>% 
	group_by(term, screenName, tot) %>% 
	ggplot(aes(x = reorder(screenName,tot), y = tot, fill = term)) + 
	geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() + facet_wrap(~term, ncol = 2, scales = "free") + 
	ylab("Usuários do Twitter") + xlab("Número de retweets")

p3 <- df %>%
  mutate(word = factor(screenName, levels = rev(unique(screenName)))) %>%
  group_by(term) %>% arrange(desc(tot)) %>%
  top_n(20) %>% ungroup() %>%
  ggplot(aes(x = reorder(word, tot), y = tot, fill = term)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x = reorder(word, tot), y = tot, label = paste0(tot%/%1000,"k") ),hjust = 0, size=2.8 )+
  facet_wrap(~term, ncol = 2, scales = "free") +
  coord_flip()+
  xlab("Usuários") + ylab("Retweets")

png("influenciadores_VazaJato.png",width=3200,height=1800,res=300)
print(p3)
dev.off()



#####3 
### 
badlocations <- c("","Brasil",
		  "Brazil",
		  "É o fim do caminho "," ",
		  "021","gaga follows | fan account", 
		  "Hogwarts","Hogwarts ",
		  "brazil","País  Das Maravilhas ",
		  "BRASIL",
		  "brasil",
		  "fundo do poço",
		  "Não tem no ",
		  "Juiz de Fora �� São Lourenço ",
		  "na sua")

df_allhashs2 <- df_tweets 
# uniformizando a escrita das localizações, para fazer o barplot
df_allhashs2$location <- str_replace_all(df_allhashs2$location,", Brasil","")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,", Brazil","")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Brasil","")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Brazil","")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"RJ","Rio de Janeiro")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Aracaju - Se","Aracajú")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Aracaju","Aracajú")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Aracajú ","Aracajú")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"São Paulo - ","São Paulo")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Paraná","Curitiba")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Bahia","Feira de Santana")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Itiúba-BA","Itiúba")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"São Paulo- ","São Paulo")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Sao Paulo- ","São Paulo")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Abc","São Bernardo")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"CE","Sobral")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Ceará","Sobral")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Santa Catarina","Florianópolis")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Goiânia, Goiás","Goiânia")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Uberlândia - MG","Uberlândia")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Manaus - AM","Manaus")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Taubaté-sp","Taubaté")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Rio Grande do Norte","Natal")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"recife","Recife")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Pernambuco","Recife")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"rj","Rio de Janeiro")
df_allhashs2$location <- str_replace_all(df_allhashs2$location,"Riachão do Dantas, Se - ","Riachão do Dantas")
df_allhashs2$location[which(df_allhashs2$location == "Rio")] <- "Rio de Janeiro"
df_allhashs2$location[which(df_allhashs2$location == "Rio de Janeirot.")] <- "Rio de Janeiro"
df_allhashs2$location[which(df_allhashs2$location == "rio de janeiro")] <- "Rio de Janeiro"
df_allhashs2$location[which(df_allhashs2$location == "Rio Grande")] <- "Porto Alegre"
df_allhashs2$location[which(df_allhashs2$location == "Porto Alegre -Rs -  ")] <- "Porto Alegre"
df_allhashs2$location[which(df_allhashs2$location == "Rio Grande do Sul")] <- "Porto Alegre"
df_allhashs2$location[which(df_allhashs2$location == "RS")] <- "Porto Alegre"
df_allhashs2$location[which(df_allhashs2$location == "Minas Gerais")] <- "Belo Horizonte"
df_allhashs2 %>%
	filter(!location %in% badlocations) %>% 
	mutate(data = ymd_hms(created)) %>% 
	filter(data >= min(ymd_hms(df_tweets_elenao$created))) %>%
	group_by(location, hashtag) %>%
	summarise(parcial = n()) %>% ungroup() %>%
	group_by(location) %>% mutate(total = sum(parcial)) %>%
	ungroup() %>%
	filter(total!=64) %>%
	filter(total > 3) %>%
	arrange(total,location) %>% tail(70) %>%
	ggplot(aes(x = reorder(location,total), y = parcial, fill=hashtag)) +
	geom_bar(stat="identity") + 
	xlab("Cidades") + ylab("Número de tweets") +
	ggtitle("15 minutos de maior volume em 29.09.2018") +
	geom_text(aes(x = reorder(location,total), y = total, label=total),hjust=0,cex=3) +
	coord_flip() +
	scale_color_manual(values=c("darkgreen", "purple"))

df_locations <- df_allhashs2 %>% filter(!location %in% badlocations) %>% 
	mutate(location = toupper(location)) %>% 
	group_by(location) %>%
	summarise(tot = n()) %>%
	filter(tot > 10) %>%
	arrange(location)

# retirando algumas localizações com símbolos estranhos
goodlocations <- as.character(df_locations$location[-c(1,2,3)])

df_allhashtags2_tomap <- df_allhashs2 %>%
	mutate(location = toupper(location)) %>%
	filter(location %in% goodlocations)

# consertando o nome das cidades, para obter o geocode corretamente
fixcitynames <- function(df_allhashtags2_tomap){
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SÃO PAULO")] <- "SÃO PAULO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SP")] <- "SÃO PAULO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SÃO PAULO ")] <- "SÃO PAULO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SÃO PAULO-SP")] <- "SÃO PAULO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SÃO PAULOSP")] <- "SÃO PAULO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SAO PAULO")] <- "SÃO PAULO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="BH")] <- "BELO HORIZONTE, MINAS GERAIS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="BELO HORIZONTE")] <- "BELO HORIZONTE, MINAS GERAIS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="BELO HORIZONTE, MINAS GERAIS")] <- "BELO HORIZONTE, MINAS GERAIS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="PALMAS")] <- "PALMAS, TOCANTINS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="MACAÉ")] <- "MACAÉ, RIO DE JANEIRO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SÃO GONÇALO")] <- "SÃO GONÇALO, RIO DE JANEIRO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="ESPÍRITO SANTO")] <- "VITÓRIA, ESPÍRITO SANTO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="VITÓRIA")] <- "VITÓRIA, ESPÍRITO SANTO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="RIO DE JANEIRO")] <- "RIO DE JANEIRO, RIO DE JANEIRO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="ACRE")] <- "RIO BRANCO, ACRE, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="BRASIL ")] <- "SÃO PAULO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SANTOS")] <- "SANTOS, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SANTO ANDRÉ")] <- "SANTO ANDRÉ, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="ARGENTINA")] <- "BUENOS AIRES, ARGENTINA"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SALVADOR")] <- "SALVADOR, BAHIA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="FEIRA DE SANTANA")] <- "FEIRA DE SANTANA, BAHIA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SOBRAL")] <- "SOBRAL, CEARÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="RECIFE")] <- "RECIFE, PERNAMBUCO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="FORTALEZA-SOBRAL")] <- "SOBRAL, CEARÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="FORTALEZA, SOBRAL")] <- "SOBRAL, CEARÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="TERESINA")] <- "TERESINA, PIAUÍ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="URSAL")] <- "TERESINA, PIAUÍ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="JOINVILLE")] <- "JOINVILLE, SANTA CATARINA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="BLUMENAU")] <- "BLUMENAU, SANTA CATARINA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SANTARÉM")] <- "SANTARÉM, PARÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="DISTRITO FEDERAL")] <- "BRASÍLIA, DISTRITO FEDERAL, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="IA")] <- "BRASÍLIA, DISTRITO FEDERAL, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="BELÉM")] <- "BELÉM, PARÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="MANAUS")] <- "MANAUS, AMAZONAS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="MACAPÁ")] <- "MACAPÁ, AMAPÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="FORTALEZA")] <- "FORTALEZA, CEARÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location==" - RIO DE JANEIRO")] <- "RIO DE JANEIRO, RIO DE JANEIRO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="RIO DE JANEIRO ")] <- "RIO DE JANEIRO, RIO DE JANEIRO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="RIO DE JANEIRO - ")] <- "RIO DE JANEIRO, RIO DE JANEIRO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="NITERÓI")] <- "NITERÓI, RIO DE JANEIRO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SÃO BERNARDO")] <- "SÃO BERNARDO DO CAMPO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="PORTO ALEGRE")] <- "PORTO ALEGRE, RIO GRANDE DO SUL, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="NATAL")] <- "NATAL, RIO GRANDE DO NORTE, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="ARACAJÚ")] <- "ARACAJÚ, SERGIPE, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="BRASÍLIA")] <- "BRASÍLIA, DISTRITO FEDERAL, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="DUQUE DE CAXIAS")] <- "DUQUE DE CAXIAS, RIO DE JANEIRO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="CURITIBA")] <- "CURITIBA, PARANÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="GOIÂNIA")] <- "GOIÂNIA, GOIÁS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="CASCAVEL")] <- "CASCAVEL, PARANÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="RIO BRANCO")] <- "RIO BRANCO, ACRE, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="JOÃO PESSOA")] <- "JOÃO PESSOA, PARAÍBA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="MACEIÓ")] <- "MACEIÓ, ALAGOAS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="OSASCO")] <- "OSASCO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="JUNDIAÍ")] <- "JUNDIAÍ, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="FLORIANÓPOLIS")] <- "FLORIANÓPOLIS, SANTA CATARINA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="RIBEIRÃO PRETO")] <- "RIBEIRÃO PRETO, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SÃO LUÍS")] <- "SÃO LUÍS, MARANHÃO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="VILA VELHA")] <- "VILA VELHA, ESPÍRITO SANTO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="CUIABÁ")] <- "CUIABÁ, MATO GROSSO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="CAMPO GRANDE")] <- "CAMPO GRANDE, MATO GROSSO DO SUL, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="CAMPINA GRANDE")] <- "CAMPINA GRANDE, PARAÍBA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="ITAJAÍ")] <- "ITAJAÍ, SANTA CATARINA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="PORTO VELHO")] <- "PORTO VELHO, RONDÔNIA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="UBERABA")] <- "UBERABA, MINAS GERAIS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="PARAÍBA")] <- "CAMPINA GRANDE, PARAÍBA, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="SETE LAGOAS")] <- "SETE LAGOAS, MINAS GERAIS, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="ARARAQUARA")] <- "ARARAQUARA, SÃO PAULO, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="MARINGÁ")] <- "MARINGÁ, PARANÁ, BRASIL"
	df_allhashtags2_tomap$location[which(df_allhashtags2_tomap$location=="VENEZUELA")] <- "CARACAS, VENEZUELA"

	return(df_allhashtags2_tomap)
}

df_allhashtags2_tomap <- fixcitynames(df_allhashtags2_tomap)

getMapLocations <- function(allcoordinates,alllocations){
	ncoordinates <- length(allcoordinates[1,])
	mycoords <- data.frame()
	for(i in 1:ncoordinates){
		coord_lat <- allcoordinates[1,][[i]]$geometry.lat
		coord_lon <- allcoordinates[1,][[i]]$geometry.lng
		coord_location <- alllocations[i]
		if( (length(coord_lat) > 0)&
		   (length(coord_lon) > 0)&
		   (length(coord_location) > 0)){
			if(i == 1){
				mycoords <- data.frame(coord_lat,coord_lon,coord_location);
			}else{
				mycoords <- rbind(mycoords,data.frame(coord_lat,coord_lon,coord_location))
			}
		}
	}
	names(mycoords) <- c("lat","lon","location")
	return(mycoords)
}

library(opencage)
# você vai precisar de uma chave do 'opencage', que eu guardo na variavel mykey
mykey <- "000000000xxxxx"#chave fake
alllocations <- unique(sort(df_allhashtags2_tomap$location))
allcoordinates <- sapply(alllocations, FUN=opencage_forward, key = mykey)
my_locations <- getMapLocations(allcoordinates, alllocations)

lat_lon3 <- my_locations %>% 
	group_by(location) %>% 
	summarise(mean_lat = mean(lat), mean_lon = mean(lon))

df_allhashtags2_tomap <- 
	df_allhashtags2_tomap %>% 
	mutate(Data = ymd_hms(created) - hours(3))

maxtime <- range(ymd_hms(df_allhashtags2_tomap$Data))[2]
mintime <- maxtime - minutes(15)
new_dat_with_location <- df_allhashtags2_tomap %>% filter(Data >= mintime)
nowtime <- mintime+seconds(10)
lasttime <- mintime
map = openmap(c(-39,-74), 
	      c(11,-34),
	      zoom=4,
	      type="osm")
while(nowtime < maxtime){
	subdat <- new_dat_with_location %>%
		filter(Data <= nowtime)
	locais <- subdat %>%
		group_by(location,hashtag) %>%
		summarise(total=n())
	inputtable <- inner_join(lat_lon3, locais, by="location")
	png(paste0("mapa_",nowtime,".png"),width=3200,height=1800,res=300)
	plotMapaBrasilTweets(inputtable,nowtime,map)
	dev.off()
	lasttime <- nowtime
	nowtime <- nowtime+seconds(10)
}

#garantindo que os pontos estão no Brasil
plotMapaBrasilTweets <- function(inputtable, mytitle, map){
	lat_lon3 <- 
		inputtable %>% 
		filter(mean_lon <= -34) %>%
		filter(mean_lon >= -74)

	sizes_elenao <- lat_lon3 %>% 
		filter(hashtag == "#EleNão")
	sizes_elenao <- sizes_elenao$total
	sizes_elesim <- lat_lon3 %>% 
		filter(hashtag == "#EleSim")
	sizes_elesim <- sizes_elesim$total
	c2 <- projectPoints(mean_lat, 
			    mean_lon, 
			    data=as.data.frame(lat_lon3 %>% filter(hashtag == "#EleNão")), 
			    to=posm() )
	c3 <- projectPoints(mean_lat, 
			    mean_lon, 
			    data=as.data.frame(lat_lon3 %>% filter(hashtag == "#EleSim")), 
			    to=posm() )
	c4 <- projectPoints(9, 
			    -52, 
			    to=posm() )
	plot(map)
	points(c2,col="purple",pch=19,cex=log2(sizes_elenao)+0.3)
	points(c3,col="green",pch=19,cex=log2(sizes_elesim)+0.3)
	#  text(c2,(lat_lon3 %>% filter(hashtag == "#EleNão"))$location, cex=0.5)
	#  text(c3,(lat_lon3 %>% filter(hashtag == "#EleSim"))$location, cex=0.5)
	legend("bottomright",legend=c("#EleSim","#EleNão"),col=c("green","purple"),pch=19,cex=0.8)
	legend("topright",legend=c(1,10,100,1000),pch=1,pt.cex=c(log2(1)+0.3,log2(10)+0.3,log2(100)+0.3,log2(1000)+0.3),horiz=FALSE)
	text(c4,labels=mytitle)
}


dat_with_location$Data <- ymd_hm(dat_with_location$Data) - hours(3)
dat_with_location$Data <- ymd_hms(dat_with_location$Data) %>% format("%Y-%m-%d %H:%M")
mintime <- range(ymd_hm(dat_with_location$Data))[1]
maxtime <- range(ymd_hm(dat_with_location$Data))[2]
new_dat_with_location <- dat_with_location %>% filter(Data > mintime)
nowtime <- mintime+minutes(2)
lasttime <- mintime
map = openmap(c(-39,-74), 
	      c(11,-34),
	      zoom=4,
	      type="osm")
while(nowtime < maxtime){
	subdat <- new_dat_with_location %>%
		filter(Data <= nowtime)
	newsubdat <- new_dat_with_location %>%
		filter(Data <= nowtime) %>%
		filter(Data > lasttime)
	locais <- subdat$Localização
	pos_locais <- which(lat_lon3$location %in% locais)
	newlocais <- newsubdat$Localização
	pos_newlocais <- which(lat_lon3$location %in% newlocais)
	if( (length(pos_locais) > 0) & (length(pos_newlocais) > 0) ){
		png(paste0("mapa_",nowtime,".png"),width=3200,height=1800,res=300)
		plotMapaBrasilTweets(lat_lon3[pos_locais,],lat_lon3[pos_newlocais,],nowtime,map)
		dev.off()
	}else{
		png(paste0("mapa_",nowtime,".png"),width=3200,height=1800,res=300)
		plotMapaBrasilTweets(lat_lon3[pos_locais,],NULL,nowtime,map)
		dev.off()
	}
	lasttime <- nowtime
	nowtime <- nowtime+minutes(2)
}
