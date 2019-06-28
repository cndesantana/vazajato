#install.packages("rtweet")

## load rtweet package
library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)

setwd("/home/ubuntu/raspador")
data <- system("date +\"%Y%m%d%H%M\"",intern=TRUE) 
termos <- c("#VazaJato","Moro","Senado")

token <- create_token(
   app = "my_twitter_research_app",
   consumer_key = "3I8JehozX8N4Bojg0qSdmDFLX",
   consumer_secret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg",
   access_token = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC",
   access_secret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"
)   

for(termo in termos){
   tweets <- search_tweets(termo, n = 10000, retryonratelimit = TRUE, type = "mixed")
   tweets$termo <- rep(termo, nrow(tweets));
   saveRDS(tweets, file = paste0("datavazajato/tweets_",termo,"_",data,".rds"))
}

cmd <- paste("sh gitcommit.sh",data)
system(cmd)
