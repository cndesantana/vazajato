#install.packages("rtweet")

## load rtweet package
library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)


#test <- identical(token, get_token())
#
#if(!test){
#   exit(1)
#}
## get user IDs of accounts followed by CNN
tmls <- search_tweets("#ShowDoPavão", n = 20000)
#tmls <- get_timelines(c("TheInterceptBr", "SF_Moro", "deltanmd", "LulaOficial"), n = 3200)
#tmls %>% filter(as.Date(format(created_at,"%Y-%m-%d")) >= as.Date("2019-05-01")) %>% group_by(screen_name) %>% summarise(totRet = sum(retweet_count))
save(tmls, file = "data/tweets_hashtag_pavao.RData")

