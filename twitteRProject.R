#0- Load my Credentials
#1- Connect to twitteR
#2- Search by screenName, string, geolocation
#3- Build a wordcloud of the text
#4- save the dataframe into excel
#5- Search By text or hashtag
#6- Search by geolocation
##End
#Step-0 set working directory to point to your files location
#Set it to your working folder
#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("xlsx")
library(twitteR)
library(ROAuth)
setwd("C:/Users/Brandon/Desktop/BUAN TwitteR")
getwd() #The working directory where R will read files and save files into
load(file="./docs/cred.Rdata") #Assumes that your credntials file is in the current folder
setup_twitter_oauth(cred$consumerKey, cred$consumerSecret, 
                    cred$oauthKey, cred$oauthSecret)

searchTwitter("$penn", n=1000)
tweets<- searchTwitter("$penn", n=1000)
t2 <- tweets[[2]]
str(t2)
tweetsDF <- twListToDF(tweets)
View(tweetsDF)
save(tweetsDF, file="./TweetsDF_May-11.Rdata")

gdgts<- sub(".*>(.*)</a>", "\\1", tweetsDF$statusSource)
gdgts<- tolower(gdgts)
table(gdgts)
windows()
par(mar=c(2,9,2,1))
barplot(table(gdgts), horiz =TRUE, las=2, cex.axis=0.5)
#install wordcloud package
install.packages("wordcloud")
#install colorBrewer Package
install.packages("RColorBrewer")
#instal tm package
install.packages("tm")
library(wordcloud)
library(RColorBrewer)
library(tm)
windows()
set.seed(1234)
text <- tolower(tweetsDF$text)
wordcloud(text, scale=c(3, 0.3), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE,
          min.freq=1, max.words=Inf)

#Search twitter for the "Penn" text
pennTweetsTL <- searchTwitter("$Penn", n=100) #1

#The Slots in the list are worth investigating
x1 <- pennTweetsTL[1]
View(x1)

x1[[1]]$text
x1[[1]]$screenName
x1[[1]]$urls
str(x1[[1]]$urls)
#Show the row id(s) of tweets that have more than one url
for(i in 1:length(pennTweetsTL)){
  if(nrow(pennTweetsTL[[i]]$urls) >= 1) 
    print(i)
}
for(i in 1:length(pennTweetsTL)){
  if(nrow(pennTweetsTL[[i]]$urls) >1) print(i)
}

pennSearchDF <- twListToDF(pennTweetsTL)
View(pennSearchDF)

allTweetsDF <- rbind(pennSearchDF, tweetsDF)
allTweetsDF <- unique(allTweetsDF)

allTweetsDF <- allTweetsDF[!duplicated(allTweetsDF$id),] #Removes duplicate ids.
View(allTweetsDF)

#
#################################  Session-2, Another Day
#
##Load the previously harvested allTweetsDF if they are already Saved.
##
load(file="C:/Users/Brandon/Desktop/BUAN TwitteR/data/TweetsDF_May-11.Rdata")
View(allTweetsDF)
##Harvest more Tweets and append it to the current list
#sign-in to a twitter session (Day 1)
library(twitteR)
library(ROAuth)
install.packages("dplyr")
setwd("C:/Users/Brandon/Desktop/BUAN TwitteR")
load(file="./cred.Rdata")
setup_twitter_oauth(cred$consumerKey, cred$consumerSecret, 
                    cred$oauthKey, cred$oauthSecret)
newList <- searchTwitter("$Penn OR #penn", n=500)
newListDF <- twListToDF(newList)
allTweetsDF <- rbind(allTweetsDF, newListDF)
allTweetsDF <- allTweetsDF[!duplicated(allTweetsDF$id),] #Removes duplicate ids.

#Save the Data for Future Use.
save(allTweetsDF, file="C:/Users/Brandon/Desktop/BUAN TwitteR/data/allTweetsDF_May_10(2).Rdata")

library("xlsx")
tweetFile <- read.xlsx("C:/Users/Brandon/Desktop/BUAN TwitteR/data/hH3000tdf_May-05-2021.xlsx", 1)
allTweetsDF <- rbind(allTweetsDF,tweetFile)

tweetFile2 <- read.xlsx("C:/Users/Brandon/Desktop/BUAN TwitteR/data/hH3000tdf_May-6-2021.xlsx", 1)
allTweetsDF <- rbind(allTweetsDF,tweetFile2)
View(allTweetsDF)

tweetFile3 <- read.xlsx("C:/Users/Brandon/Desktop/BUAN TwitteR/data/hH3000tdf_May-10-2021.xlsx", 1)
allTweetsDF <- rbind(allTweetsDF,tweetFile3)
View(allTweetsDF)

#Transform Text
atdf <- allTweetsDF
write.csv(atdf$text, file="../atdfText.csv")

### ###### 10.2 Transform Text
## Inspect the text:A lot of cleanup is needed: Data Cleaning, scrubbing, repurposing, etc.
## ###### Cleanup of the text for the purpose of text analytics
## convert all to lowercase, 
## remove none-ASCII chars, etc.
## remove urls, 
#Order of clean-up is important
# Trouble!!!!!
head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)
tolower(atdf$text)  

#remove retweets
atdf <- data.frame(dplyr::filter(atdf, grepl('FALSE', isRetweet)))
View(atdf)
#removeNonASCII() is a method that removes non-ASCII characters
removeNonASCII<- function(txt){
  return(iconv(txt, to="ASCII", sub=""))
}

removeNonASCII(atdf$text)
atdf$text <- removeNonASCII(atdf$text)
View(atdf)

head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)

#removeCntrl() a method to remove control characters in the text
removeCntrls<- function(x){
  x<- gsub("[[:cntrl:]]",  "",x)
  return(x)
}
removeCntrls(atdf$text)
atdf$text <- removeCntrls(atdf$text)
head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)

# tolower is a method from the R package, we do not have to write it ourself
atdf$text <- tolower(atdf$text)
head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)
write.csv(atdf$text, file="../atdfText_1.csv")

## removeYRLs() is a method: It performs a regular expression match and clean
grep("http(s?)://[[:alnum:]].\\S*", atdf$text, value=TRUE)
m<- regexpr("http(s?)://[[:alnum:]].\\S*", atdf$text)
regmatches(atdf$text, m)

removeURLs <- function(x){    #df$text <- removeURLs(df$text)
  x<- gsub("http(s?)://[[:alnum:]].\\S*", " ", x)
  x<- gsub("http(s?):/.*$", " ",x)
  return(x)
}

atdf$text <- removeURLs(atdf$text)

## Inspect the Data
grep("http(s?)://[[:alnum:]].\\S*", atdf$text, value=TRUE)
m<- regexpr("http(s?)://[[:alnum:]].\\S*", atdf$text)
regmatches(atdf$text, m)
head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)
write.csv(atdf$text, file="../tweetsText_1.csv")

#removeRT() removes RT space at the beginning of a retweeted tweet
grep("(^RT|^MRT) @", allTweetsDF$text, value=TRUE)
m<- regexpr("(^RT|^MRT) @", allTweetsDF$text)
regmatches(allTweetsDF$text, m)
removeRT<- function(x){    #df$text <- removeURLs(df$text)
  x<- gsub("(^RT|^MRT) @", "@",x, ignore.case=TRUE)
  return(x)
}

removeRT(atdf$text)
atdf$text <- removeRT(atdf$text)
## Inspect the Data
grep("(^RT|^MRT) @", atdf$text, value=TRUE)
m<- regexpr("(^RT|^MRT) @", atdf$text)
regmatches(atdf$text, m)
head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)

#removeQuotes() removes single and double quotes
grep("\'|\"", atdf$text, value=TRUE)
m<- regexpr("\'|\"", atdf$text)
regmatches(atdf$text, m)
removeQuotes<- function(x){
  return(gsub("\'|\"", " ", x))
}
removeQuotes(atdf$text)
atdf$text <- removeQuotes(atdf$text)
grep("\'|\"", atdf$text, value=TRUE)
m<- regexpr("\'|\"", atdf$text)
regmatches(atdf$text, m)

#removeNewLines() removes news lines
grep("[\r\n]", atdf$text, value=TRUE)
m<- regexpr("[\r\n]", atdf$text)
regmatches(atdf$text, m)
removeNewLines<- function(x){
  x<- gsub("[\r\n]",  " ",x)
  return(x)
}
atdf$text <- removeNewLines(atdf$text)


grep("[\r\n]", atdf$text, value=TRUE)
m<- regexpr("[\r\n]", atdf$text)
regmatches(atdf$text, m)
head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)
#removeColons() removes colons
removeColons<- function(x){
  x<- gsub(":",  " ",x)
  return(x)
}
atdf$text <- removeColons(atdf$text)

#removePeriods() removes periods
removePeriods<- function(x){
  x<- gsub("[.]",  " ",x)
  return(x)
}

atdf$text <- removePeriods(atdf$text)
head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)

grep("&.*;", atdf$text, value=TRUE)
m<- regexpr("&.*;", atdf$text)
regmatches(atdf$text, m)
removeMany<- function(x){
  x<- gsub("&.*;",  " ",x)
  x<- gsub("/",  " ",x)
  x<- gsub(",",  " ",x)
  x<- gsub(" http",  " ",x)
  x<- gsub("http ",  " ",x)
}
atdf$text <- removeMany(atdf$text)
grep("&.*;", atdf$text, value=TRUE)
m<- regexpr("&.*;", atdf$text)
regmatches(atdf$text, m)
head(atdf$text,25)
atdf[390:410,]$text
tail(atdf$text,25)



# Last thing to do is to remove, extra, leading and trailing spaces
# removeMultipleSpaces() removes replaces multiples spaces by just one space
removeExtraSpaces<- function(x){
  x<- gsub("[[:space:]]+",  " ",x)
  return(x)
}
atdf$text <- removeExtraSpaces(atdf$text)
grep("&.*;", atdf$text, value=TRUE)
m<- regexpr("&.*;", atdf$text)
regmatches(atdf$text, m)

# removeLeadTrailSpaces() removes leading and trailing spaces
grep("^[[:space:]]", atdf$text, value=TRUE)
m<- regexpr("^[[:space:]]", atdf$text)
regmatches(atdf$text, m)

removeLeadingTrailingSpaces<- function(x){
  x<- gsub("^[[:space:]]",  "",x)
  x<- gsub("[[:space:]]$",  "",x)
  return(x)
}
atdf$text <- removeLeadingTrailingSpaces(atdf$text)
grep("&.*;", atdf$text, value=TRUE)
m<- regexpr("&.*;", atdf$text)
regmatches(atdf$text, m)

grep("^[[:space:]]", atdf$text, value=TRUE)
m<- regexpr("^[[:space:]]", atdf$text)
regmatches(atdf$text, m)

write.csv(atdf$text, file="../atdfText_10.csv")
## Letus do a wordcloud
library(wordcloud)
windows(width=10, height=8)
wordcloud(atdf$text)

## Letus do a colorful wordcloud
library(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(atdf$text,scale=c(3, 0.6), random.order=T, colors=pal2)

library(tokenizers)
grams_3 <- tokenize_ngrams(atdf$text, n = 3, n_min = 3,lowercase=TRUE)
unlist(grams_3)
g3<- table(unlist(grams_3))

wordcloud(words=names(g3), freq=g3, scale=c(1.6, 0.4), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.22,
          min.freq=3, max.words=Inf)
View(atdf)
allTweetsDF <- atdf
save(allTweetsDF, file="../cleanTweetsDF.Rdata")
View(allTweetsDF)

##################-1 Inspect the twitterers
unique(allTweetsDF$screenName)
snTable <- data.frame(table(allTweetsDF$screenName))
snTable
View(snTable)
names(snTable)<- c("screenName", "Freq")
table(snTable$Freq)
windows()
barplot(table(snTable))

barplot(table(snTable[snTable$Freq>2,]), main= "Freq Count of participents Count")

minFreq = 2
sfr<- snTable[snTable$Freq> minFreq & snTable$Freq <max(snTable$Freq) ,]
sfr <- sfr[order(-sfr$Freq),]
View(sfr)
windows(height=10, width=8)
?par
par()$mar
par(mar=c(2, 5, 2, 1))
par(mai=c(0.4, 1.2, 0.4, 0.2))
barplot(sfr$Freq, names.arg=sfr$screenName, horiz=TRUE, las=2, cex.names=0.5)

wordcloud(words=sfr$screenName, freq=sfr$Freq, scale=c(3, 1), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=2, max.words=Inf)

#grab portnoy mentions
length(grep("@stoolpresidente", allTweetsDF$text))

#grab barstool  mentions
length(grep("@barstoolsports", allTweetsDF$text))

length(grep("portnoy", allTweetsDF$text))

###########-2 grab Check hashtags
grep("#[[:alnum:]]+", allTweetsDF$text, value=TRUE) 
# + means one or more, * means Zero or more

m<- regexpr("#[[:alnum:]]+", allTweetsDF$text)
hts <- data.frame(table(regmatches(allTweetsDF$text, m)))
hts <- hts[order(hts$Freq),]
View(hts)
hts1 <- hts[hts$Freq>1,]
barplot(hts1$Freq,names.arg=hts1$Var1, xlab="", ylab="", las=2, cex.names=0.7, horiz = TRUE)

wordcloud(words=hts1$Var1, freq=hts1$Freq, scale=c(3, 0.5), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=1, max.words=Inf)

############-3 Grab mentions
windows(height=10, width=8)
grep("@[[:alnum:]]+", allTweetsDF$text, value=TRUE)
m<- regexpr("@[[:alnum:]]+", allTweetsDF$text)
mntns <- data.frame(table(regmatches(allTweetsDF$text, m)))
mntns <- mntns[order(mntns$Freq),]
View(mntns)
mntns1 <- mntns[mntns$Freq>1,]

barplot(mntns1$Freq,names.arg=mntns1$Var1, xlab="", ylab="", las=2, cex.names=0.6, horiz = TRUE)
wordcloud(words=mntns$Var1, freq=mntns1$Freq, scale=c(4, 1), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=1, max.words=Inf)


##################-3 Grab application Used
appUsed<- sub(".*>(.*)</a>", "\\1", allTweetsDF$statusSource)
#1 ".*> Explained: . means any, * means zero or more, then >
#2 </a>" Explained: and ends with the following keyboard symbols </a>"
#3 (.*) Explained: grap whtever is between pattern 1 and 2 

appUsed<- tolower(appUsed)
apps <- data.frame(table(appUsed))
View(apps[order(-apps$Freq),])
apps<- apps[ apps$Freq> 1,]
apps <- apps[order(apps$Freq),]
windows()

barplot(apps$Freq,names.arg=apps$appUsed, xlab="", ylab="", las=2, cex.names=0.7, horiz = TRUE)
wordcloud(words=apps$appUsed, freq=apps$Freq, scale=c(4, 1), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=1, max.words=Inf)

###################-4 Grab URLS
grep("http(s?)://[[:alnum:]].\\S*", allTweetsDF$text, value=TRUE)
m<- regexpr("http(s?)://[[:alnum:]].\\S*", allTweetsDF$text)
urls <- data.frame(table(regmatches(allTweetsDF$text, m)))
urls
urls <- urls[order(-urls$Freq),]
urls
barplot(urls$Freq,names.arg=urls$Var1, xlab="", ylab="", las=2, cex.names=0.7, horiz = TRUE)
wordcloud(words=urls$Var1, freq=urls$Freq, scale=c(4, 1), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=1, max.words=Inf)


###################-5 Inspect the Text
head(allTweetsDF$text, 10)
allTweetsDF$text[1000:1009]
tail(allTweetsDF$text, 10)

wordcloud(allTweetsDF$text, scale=c(3, 1), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=3, max.words=Inf)

#Error in .tolower(txt) : invalid input '??? ?????????O??? ????????????' in 'utf8towcs'

text <- iconv(allTweetsDF$text, "latin1", "ASCII", sub="")
#displayTweetsText(allTweetsDF$text)
text
wordcloud(text, scale=c(2, 0.5), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=2, max.words=Inf)

windows(height=8, width=10)
grams_4 <- tokenize_ngrams(text, n = 4, n_min = 3,lowercase=TRUE)
unlist(grams_4)
g4<- table(unlist(grams_4))

wordcloud(words=names(g4), freq=g4, scale=c(1.8, 0.6), random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"), random.color=TRUE, rot.per=0.25,
          min.freq=3, max.words=Inf)

displayTweetsText<- function(txt){
  h<- head(txt, 10)
  m<- txt[(length(text)/2-5):(length(text)/2+5)]
  t<- tail(txt, 10)
  hmt <- c(h,m,t)
  hmt
}
