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
   consumer_key = "XX",
   consumer_secret = "XXX",
   access_token = "XX-XX",
   access_secret = "XX"
)   

for(termo in termos){
   tweets <- search_tweets(termo, n = 10000, retryonratelimit = TRUE, type = "mixed")
   tweets$termo <- rep(termo, nrow(tweets));
   saveRDS(tweets, file = paste0("datavazajato/tweets_",termo,"_",data,".rds"))
}

cmd <- paste("sh gitcommit.sh",data)
system(cmd)
