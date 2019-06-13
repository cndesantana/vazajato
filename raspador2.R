#install.packages("rtweet")

## load rtweet package
library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)

#token <- create_token(
#   app = "my_twitter_research_app",
#   api_key = "3I8JehozX8N4Bojg0qSdmDFLX",
#   api_secret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg",
#   access_token = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC",
#   access_token_secret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx")
#
#test <- identical(token, get_token())
#
#if(!test){
#   exit(1)
#}
## get user IDs of accounts followed by CNN
tmls <- get_timelines(c("TheInterceptBr", "SF_Moro", "deltanmd", "STF_oficial"), n = 3200)
tmls %>% filter(as.Date(format(created_at,"%Y-%m-%d")) >= as.Date("2019-06-09")) %>% group_by(screen_name) %>% summarise(totRet = sum(retweet_count))
save(tmls, file = "tweets_dos_usuarios.RData")

