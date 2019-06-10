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

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

args = commandArgs(trailingOnly=TRUE)
cat(length(args))
if(length(args) > 2)
{
   dateini <- args[1]
   datefim <- args[2]
   terms <- args[-c(1,2)]
}else{
   terms <-c("#VazaJato","Intercept","Moro","Dallagnol")
   dateini <-c("2019-06-09")
   datefim <-c("2019-06-10")
}

df_tweets <-data.frame()
for(i in 1:length(terms)){
  term <- terms[i]
  tweets <-searchTwitter(term, n=10000, lang="pt-br",dateini,datefim,resultType = "recent")
  df_tweets <- rbind(df_tweets,cbind(twListToDF(tweets),term))
}

save(df_tweets,file="df_tweets_10000_vazajato.Rdat")

