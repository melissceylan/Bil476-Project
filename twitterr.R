library(twitteR)
library(ROAuth)
library(tm)
library(RCurl)
library(plyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

api_key <- "bCxUBIGAKEotklJaqHgZ1qISF"
api_secret <- "8WTObohcyYuu3j3KQ2CxPADSV01nuUe9nW2mqooQOIaLKwHqo9"
access_token <- "958495403225288704-sQXjNhqnE6j4ZNbxqwF3SCkmyNfvtTl"
access_token_secret <- "9wlEouNiU8AVuOPelUei9PkOALXWswnUCbsbD1QMdZ0et"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


##hashtagden veri çekme


availableTrendLocations() %>% filter(country == "Turkey")
h <- getTrends(2343732)
head(h,20)
view(h)

tw <- searchTwitter("#yks2019", n=2000)
tw_m <- searchTwitter("#yks2019", n=300)
df_tw <- twListToDF(tw)
tw_min <- twListToDF(tw_m)
View(tw_min)
View(df_tw)


#Appending some more words to actual words


pos.words2 = c('başarılar','yerleştim','güzel','Güzel','#buralarseninlegüzel','#BuralarSeninleGüzel','gelirse','iyi')
neg.words2 = c('usulsüzlük','kötü','zor','keşke','keske','stresi','stresidir','stresinin','stresin','stres')

#n
#Hashtag ile Duygu Analizi


bscore <- score.sentiment(df_tw$text,pos.words2,neg.words2,.progress="text")
mscore <- score.sentiment(tw_min$text,pos.words2,neg.words2,.progress="text")
View(bscore)
view(mscore)
hist(bscore$score,col="purple", 
     main="#yks2019 Hashtag",
     ylab=" Count of tweets",
     xlab="Sentiment")
hist(mscore$score,col="blue", 
     main="#yks2019 Hashtag_s", 
     ylab = "Count of Tweets", 
     xlab = "Sentiment")

#profilden duygu analizi


df_hl <- userTimeline('haluklevent', n = 1000)
hl <- userTimeline('haluklevent', n = 300)
df <- twListToDF(df_hl)
hl_s <- twListToDF(hl)
View(df)
view(hl_s)

pos.words3 = c('hayırsever','teşekkür','teşekkürü','teşekkürler','Teşekkür','Teşekkürler','harikasın','harikasınız','sağol','sağolun','sağolasın','sağolsun', 'Sağol','Sağolun','Sağolasın','Sağolsun')
neg.words3 = c('tepki','kahroldum','Kahroldum','donör','hastalık','Hastalık','hastası','hastanesinin','hastaneye','hastane','hastalığı','hasta','Hasta','acı','Acıların','acılarını','acıtan','acılı','malesef','Maalesef')

p_score <- score.sentiment(df$text,pos.words3,neg.words3,.progress = "text")
hscore <- score.sentiment(hl_s$text,pos.words3,neg.words3,.progress = "text")
View(p_score)

hist(p_score$score, 
     col="purple", 
     main="Haluk Levent Profile",
     ylab="Frequency",
     xlab="Sentiment")
hist(hscore$score, 
     col="blue", 
     main = "Haluk Levent Profile", 
     ylab = "Frequency",
     xlab="Sentiment")

#ploting the tweets on qplot

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



# Profil hakkında duygu analizi


b <- searchTwitter("@haberturk", n=1000)
b_sml <- searchTwitter("@haberturk", n=300)
db <- twListToDF(b)
b_smlL <- twListToDF(b_sml)

View(db)

pos.words4 = c('güzel','Güzel','iyi','iyiyim','iyiye','"sevgi"','sevgili','Sevgili','tebrik','Tebrik','Tebrikler','teşekkür','mutlu')
neg.words4 = c('işsizliğin','yalan','savaş','işsiz','olumsuz','terörist','Terörist','Teröristler','yolsuzluk','YOLSUZLARI','YOLSUZLUK','şehit', 'kötü','cinayete','krizi')

p1_score <- score.sentiment(db$text,pos.words4,neg.words4,.progress = "text")
bs_score <- score.sentiment(b_smlL$text,pos.words4,neg.words4,.progress = "text")
View(p1_score)
view(bs_score)

hist(p1_score$score, 
     col="purple", 
     main="Habertürk Profile",
     ylab="Frequency",
     xlab="Sentiment")
hist(bs_score$score, 
     col="blue", 
     main = "Habertürk Profile",
     ylab = "Frequency", 
     xlab = "Sentiment")


## Kullanim Saatleri Dagilimi


fp_tw <- userTimeline('fatihportakal', n = 2000)
fp <- twListToDF(df_user)

fptw_s <- userTimeline('fatihportakal', n = 300)
fptw <- twListToDF(fptw_s)

hist(hour(fp$created), col = "purple", 
     xlab = "Saat Araligi", 
     ylab = "Tweet Sayisi",
     xlim = c(0,25))

hist(hour(fptw$created), col = "blue",
     xlab = "Saat Aralığı",
     ylab = "Tweet Sayısı",
     xlim = c(0,25))

gunisim <- wday(fp$created, label = TRUE)
fptw_gun <- wday(fptw$created, label = TRUE)

ggplot(fp, aes(gunisim)) + geom_bar()
ggplot(fptw, aes(fptw_gun)) + geom_bar()