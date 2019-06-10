library(stringr)
library(quanteda)
library(readtext)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shiny)
library(shinyFiles)
library(devtools)
library(Rfacebook)
library(lubridate)
library(dplyr)
library(stylo)
library(tidytext)
library(tm)
library(wordcloud)
library(xlsx)
library(gdata)
library(readxl)
library(httr)
library(twitteR)

api_key = "XXXXXXXXXXXXXXX"
api_secret = "YYYYYYYYYYYYYYYYYYYYYYYYYY"
access_token = "XYXYXYXYXYXYXXYXYXYXYXYYXYX"
access_token_secret = "ZZZZZZZZZZZZZZZZZZZZZZZZ"
   api_key = "3I8JehozX8N4Bojg0qSdmDFLX"
   api_secret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg"
   access_token = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC"
   access_token_secret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"
 
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

args = commandArgs(trailingOnly=TRUE)
cat(length(args))
if(length(args) > 2)
{
   dateini <- args[1]
   datefim <- args[2]
   terms <- args[-c(1,2)]
}else{
   terms <-c("#VazaJato","Intercept","Moro","Dallagnol","#MoroCriminoso")
   dateini <-c("2019-06-09")
   datefim <-c("2019-06-10")
}

df_tweets <-data.frame()
for(i in 5:length(terms)){
  term <- terms[i]
  tweets <-searchTwitter(term, n=10000, lang="pt-br",dateini,datefim,resultType = "recent")
  df_tweets <- rbind(df_tweets,cbind(twListToDF(tweets),term))
}

save(df_tweets,file="df_tweets_10000_MoroCriminoso_vazajato.Rdat")

