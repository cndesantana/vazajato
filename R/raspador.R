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

api_key = "XXX"
api_secret = "XX"
access_token = "XX-XX"
access_token_secret = "XXXXX"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

args = commandArgs(trailingOnly=TRUE)
cat(length(args))
if(length(args) > 2)
{
   dateini <- args[1]
   datefim <- args[2]
   terms <- args[-c(1,2)]
}else{
   terms <-c("cnpq","CNPQ", "Marcos Pontes","Future-se","Abraham Weintraub",
             "Weintraub", "@Astro_Pontes", "@AbrahamWeint")
   dateini <-c("2019-08-26")
   datefim <-c("2019-09-02")
}

df_tweets <-data.frame()
for(i in 1:7){
  term <- terms[i]
  tweets <-searchTwitter(term, n=10000, lang="pt-br",dateini,
                         datefim)
  df_tweets <- rbind(df_tweets,cbind(twListToDF(tweets),term))
}



save(df_tweets,file="df_pesquisa.Rdat")

