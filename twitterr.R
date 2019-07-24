
library(twitteR)
library(ROAuth)
library(tm)
library(RCurl)
library(plyr)
library(ggplot2)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

api_key <- "bCxUBIGAKEotklJaqHgZ1qISF"
api_secret <- "8WTObohcyYuu3j3KQ2CxPADSV01nuUe9nW2mqooQOIaLKwHqo9"
access_token <- "958495403225288704-sQXjNhqnE6j4ZNbxqwF3SCkmyNfvtTl"
access_token_secret <- "9wlEouNiU8AVuOPelUei9PkOALXWswnUCbsbD1QMdZ0et"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##hashtagden veri Ã§ekme
library(tidyverse)
availableTrendLocations() %>% filter(country == "Turkey")
h <- getTrends(2343732)
head(h,20)
tw <- searchTwitter("#yks2019", n=2000)

class(tw)
str(tw)
df_tw <- twListToDF(tw)
View(df_tw)

#Appending some more words to actual words
pos.words2 = c('güzel','makam', 'iyi','olumlu','baþarý')
neg.words2 = c( 'asla', 'olmaz','kötü', 'olumsuz')

df$text
#converting Into dataFrame
#calcuating result
result <- score.sentiment(df$text,pos.words2,neg.words2)
result
#summarlizing data
summary(result$score)

#Histogram
hist(result$score,col="yellow", main="Score of tweets",ylab=" Count of tweets")

#Count No of Tweets
count(result$score)

#ploting the tweets on qplot
qplot(result$score,xlab = "Score of tweets")

#score Sentiment function
#Used to remove all unwanted data 
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


plot(cars)


##profilden veri Ã§ekme
df_user <- userTimeline('melis_ceylann', n = 100)
df <- twListToDF(df_user)
View(df)

##Profil temel bilgilerine eriÅŸim
mls <- getUser("melis_ceylann")
attributes(mls)
str(mls)
mls$name
mls$statusesCount
mls$profileImageUrl
download.file("http://pbs.twimg.com/profile_images/1073926902463762432/laojx3mg_normal.jpg",destfile = "pl.jpg")
mls$getFavorites(n=10)
mls$getFriends(n=10)

cmylmz <- getUser("CMYLMZ")
cmylmz$friendsCount
cmylmz$location

mls$getFollowers(n=10)
mls$getFollowerIDs(n=100)

mls$lastStatus$favoriteCount
mls$lastStatus$statusSource
mls$lastStatus$text


##Profilin Enleri
 