#install.packages("rtweet")

## load rtweet package
library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)

data <- system("date +\"%Y%m%d%H%M\"") 
termos <- c("#VazaJato")
for(termo in termos){
   tmls <- search_tweets(termo, n = 20000)
   save(tmls, file = paste0("data/tweets_",data,".RData"))
}
