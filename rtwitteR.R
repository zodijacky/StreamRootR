require("twitteR")
require("RCurl")
library(plyr)
consumer_key <- 'ViPM7TU4Z7U0Pr3KSRaqpqMIR'
consumer_secret <- 'M1uQtgRAkXnNS37GNN6oBl4OGZycylNgYvmyo6meBb6WyJPulk'
access_token  <- '244692641-2XzicioVJs22iO44unOsy8ZkF1X26XXw5i9OukeE'
access_secret <- 'KoajRNPgvYf53B2rWu3SurlpnZiK1yxfXdXMBPwX8XjW6'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret )
Stream_root_tweets <- searchTwitter("streaming",n=500)
length.tweet <- length(Stream_root_tweets)
Streamtweets11.df <- ldply(Stream_root_tweets,function(t)t$toDataFrame())
write.csv(Streamtweets11.df,"tweetsStreams.csv")
#get the text
SR_text = sapply(Stream_root_tweets,function(x) x$getText() )
#cleaning 


# define "tolower error handling" function
clean.text <- function(SR_text)
{
  SR_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", SR_text)
  SR_text = gsub("@\\w+", "", SR_text)
  SR_text = gsub("[[:punct:]]", "", SR_text)
  SR_text = gsub("[[:digit:]]", "", SR_text)
  SR_text = gsub("http\\w+", "", SR_text)
  SR_text = gsub("[ \t]{2,}", "", SR_text)
  SR_text = gsub("^\\s+|\\s+$", "", SR_text)
  SR_text = gsub("amp", "", SR_text)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  SR_text = sapply(SR_text, try.tolower)
  SR_text = SR_text[SR_text != ""]
  names(SR_text) = NULL
  return(SR_text)
}
SR_corpuss <-Corpus(VectorSource(SR_text))
SR_text <- tm_map(SR_corpuss,removePunctuation)
SR_text <- tm_map(SR_text,content_transformer(tolower))
SR_text <- tm_map(SR_text,removeWords,stopwords("english"))
SR_text <- tm_map(SR_text,removeNumbers)
write.csv(SR_text,"tweetclean.csv")
#building wordCloud
require(wordcloud)
pal <-brewer.pal(8,"Dark2")
wordcloud(SR_text,random.order=F,max.word=40,color =pal)

#Sentiment analysis
library(syuzhet)
mysentiment <- get_nrc_sentiment(SR_text)
sentimentScore <- data.frame(colSums(mysentiment[,]))
names(sentimentScore) <- "score"
sentimentScore <- cbind("sentiment"= row.names(sentimentScore),sentimentScore)
rownames(sentimentScore) <- NULL
ggplot(data = sentimentScore,aes(x= sentiment ,y= score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiment")+ylab("Score")+ggtitle("total Sentiment Score based on tweets")


#WordCloud
wordcloud(SR_text,random.order=F,max.word=40,colors=rainbow(50))