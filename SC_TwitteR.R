
### -----------------------------------------------------
### --- Goran S. Milovanovic, Phd
### --- 21 Feb 2017
### --- SmartCat: https://www.smartcat.io/
### --- SmartCat Blog, February.
### --- Case Study: NASA 21 February 2017. w. {TwitteR}
### -----------------------------------------------------

### -----------------------------------------------------
### --- Part 1: Access Twitter Search API w. {TwitteR}
### --- Description: NASA has announced a press-conference
### --- on 22 Feb 2017 citing "...Discovery Beyond Our Solar System" 
### --- See:
### --- https://www.nasa.gov/press-release/nasa-to-host-news-conference-on-discovery-beyond-our-solar-system/
### --- Task 1: scrape all NASA related Twitter accounts
### --- List of NASA and NASA related Social Media:
### --- https://www.nasa.gov/socialmedia
### --- Task 2: scrape #asknasa tag; compare
### -----------------------------------------------------

### --- clear all
rm(list=ls())

### --- Set-up:
library(twitteR)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(tm.plugin.sentiment)
library(ggplot2)
library(wordcloud)
library(proxy)
library(igraph)

### --- NASA Twitter Accounts:
accountsFrame <- read.csv('NASATwitterAccounts.csv',
                          header = T,
                          row.names = 1,
                          stringsAsFactors = F)

### --- Twitter Credentials:
consumer_key <- '6TA88vkxJK5xqO5LUv4RIycpw'
consumer_secret <- '2f0svS4OA5OWViX7zTbhU1IHQjdD2yPPCI6tPxl1UqWyOzurep'
access_token <- '2552114780-BbNSM7DdCInaNWQ3HWz1GxMSoBYGmPgPVdnezvl'
access_secret <- 'xL0kkiEdkTWfa2OD21t2WednSSvCNXJWibwaCGQawEc5h'
setup_twitter_oauth(consumer_key, 
                    consumer_secret, 
                    access_token, 
                    access_secret)

### --- access Twitter Search API
### --- Accessed on: February 21, 2017, late CET hours
setwd(paste0(getwd(),"/source"))
accounts <- accountsFrame$Account
for (i in 1:length(accounts)) {
  # - access Twitter
  t1 <- Sys.time()
  uT <- userTimeline(accounts[i], n=3200,
                     includeRts = T,
                     excludeReplies = FALSE)
  # - format
  dT <- lapply(uT, function(x) {data.frame(text = x$text, 
                                             screeName = x$screenName, 
                                             date = x$created)})
  dT <- do.call(rbind, dT)
  # - save
  write.csv(dT, paste0(accounts[i],"_Tweets.csv"))
  t2 <- Sys.time()
  # - report
  print(paste0("Completed: ", accounts[i], 
               ": ", as.character(i), 
               "/", length(accounts), 
               "|Time: ", round(t2-t1, 2),
               "|Acquired: ", length(dT$text))
               )
}

### -----------------------------------------------------
### --- Part 2: Compute Sentiment Scores
### --- {tm.plugin.sentiment}
### -----------------------------------------------------

### --- combine data sets
files <- list.files()
for (i in 1:length(files)) {
  f <- read.csv(files[i],
                header = T,
                row.names = 1,
                stringsAsFactors = F)
  if (i == 1) {
    tweetsDF <- f 
  } else {
    tweetsDF <- rbind(tweetsDF, f)
  }
  # - report
  print(paste0("Merged file: ", i, "/", length(files)))
}
rm(f); gc()

### --- some wranglig takes place here:
tweetsDF$Date <- unname(sapply(tweetsDF$date, function(x) {
  strsplit(x, split = " ", fixed = T)[[1]][1]
}))
tweetsDF <- tweetsDF %>% separate(Date,
                                  into = c('Year', 'Month', 'Day'),
                                  remove = F)
tweetsDF$Time <- unname(sapply(tweetsDF$date, function(x) {
  strsplit(x, split = " ", fixed = T)[[1]][2]
}))
tweetsDF <- tweetsDF %>% separate(Time,
                                  into = c('Hour', 'Minute', 'Second'),
                                  remove = F)

# - sort
tweetsDF <- tweetsDF %>% arrange(Year, Month, Day, Hour, Minute, Second)

### --- Sentiment Analysis
# - as {tm} VCorpus
nasaCorpus <- VCorpus(VectorSource(tweetsDF$text))
# - {tm.plugin.sentiment} sentiment scores
# - Term-Document Matrix
nasaTDM <- TermDocumentMatrix(nasaCorpus,
                              control = list(tolower = TRUE,
                                             removePunctuation = TRUE,
                                             removeNumbers = TRUE,
                                             removeWords = list(stopwords("english")),
                                             stripWhitespace = TRUE,
                                             stemDocument = TRUE,
                                             minWordLength = 3,
                                             weighting = weightTf))
# - {tm.plugin.sentiment} polarity score
# - NOTE: that would be (n-p)/(n+p)
nasaPolarity <- polarity(nasaTDM)
sum(nasaPolarity != 0)
tweetsDF$Polarity <- nasaPolarity
rm(list = c('nasaPolarity', 'nasaCorpus')); gc()
# - save nasaSentimentTDM
setwd("../")
saveRDS(nasaTDM, "nasaSentimentTDM.Rds")
rm(nasaTDM); gc()
write.csv(tweetsDF, "tweetsDF.csv")

### -----------------------------------------------------
### --- Part 3: What is the optimal time for the Aliens
### --- to invade Earth?
### -----------------------------------------------------

emoHours <- tweetsDF %>% 
  group_by(Hour) %>%
  summarise(tweets = n(),
            positive = length(which(Polarity > 0)),
            neutral = length(which(Polarity == 0)),
            negative = length(which(Polarity < 0))
            )
emoHours$positive <- emoHours$positive/emoHours$tweets
emoHours$neutral <- emoHours$neutral/emoHours$tweets
emoHours$negative <- emoHours$negative/emoHours$tweets
emoHours$Hour <- as.numeric(emoHours$Hour)
emoHours$Volume <- emoHours$tweets/max(emoHours$tweets)
emoHours <- emoHours %>% 
  gather(key = Measure,
         value = Value,
         positive:Volume)
ggplot(emoHours, aes(x = Hour, y = Value, color = Measure)) +
  geom_path(size = .25) +
  geom_point(size = 1.5) +
  geom_point(size = 1, color = "White") +
  ggtitle("Optimal Time to Invade Earth") +
  scale_x_continuous(breaks = 0:23, labels = as.character(0:23)) +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 8, angle = 90))

### -----------------------------------------------------
### --- Part 4: The Semantics of #asknasa
### -----------------------------------------------------

### --- access Twitter: the #asknasa tag

### --- Twitter Credentials:
consumer_key <- '6TA88vkxJK5xqO5LUv4RIycpw'
consumer_secret <- '2f0svS4OA5OWViX7zTbhU1IHQjdD2yPPCI6tPxl1UqWyOzurep'
access_token <- '2552114780-BbNSM7DdCInaNWQ3HWz1GxMSoBYGmPgPVdnezvl'
access_secret <- 'xL0kkiEdkTWfa2OD21t2WednSSvCNXJWibwaCGQawEc5h'
setup_twitter_oauth(consumer_key, 
                    consumer_secret, 
                    access_token, 
                    access_secret)

# - Approx. 14:00:00 CET:
askNASA <- searchTwitter("#asknasa", 
                         n = 5000, 
                         lang = "en",
                         resultType = "recent", 
                         retryOnRateLimit=10)
# - format
dT <- lapply(askNASA, function(x) {data.frame(text = x$text, 
                                         screeName = x$screenName, 
                                         date = x$created)})
dT <- do.call(rbind, dT)
# - remove NASA and NASA related accounts from dt:
dT <- dT[-which(grepl("NASA", dT$screeName, fixed = T)), ]

# - extract all mentioned user accounts in #asknasa
askNasaMentioned <- str_extract_all(dT$text, "@[[:alnum:]|_]+")
# - unique NASA and NASA related accounts
nasaAcc <- paste0("@", accounts)
# - how many mentions of NASA and NASA related accounts in #asknasa
searchNasaMentioned <- paste(unlist(askNasaMentioned), collapse = " ")
nasaAccCount <- sapply(nasaAcc, function(x) {
  sum(str_count(searchNasaMentioned, fixed(x)))
})
nasaAccCount <- nasaAccCount[nasaAccCount > 0]
nasaAccCount

# - extract all hashtags #asknasa
askNasaHashtags <- unlist(str_extract_all(dT$text, "#[[:alnum:]]+"))
# - remove #asknasa
askNasaHashtags <- askNasaHashtags[-which(tolower(askNasaHashtags) == "#asknasa")]
askNasaHashtags <- sort(table(askNasaHashtags), decreasing = T)
askNasaHashtags <- data.frame(hashtag = names(askNasaHashtags),
                              frequency = as.numeric(askNasaHashtags))

par(bg = "black")
wordcloud(words = askNasaHashtags$hashtag,
          freq = askNasaHashtags$frequency,
          scale = c(4, .5),
          max.words = 200,
          min.freq = 1,
          random.order = T,
          colors = brewer.pal(9, name = "Blues")[2:9])

### --- Aliens?
askNasaHashtags$hashtag[
  which(grepl("alien", tolower(askNasaHashtags$hashtag)))]

### --- store objects
write.csv(dT, "AskNasaTweetsDF.csv")

### -----------------------------------------------------
### --- Part 5: The Social Map of NASA Twitter accounts
### -----------------------------------------------------

dT <- read.csv("AskNasaTweetsDF.csv", 
                     header = T, 
                     row.names = 1, 
                     stringsAsFactors = F)
tweetsDF <- read.csv("tweetsDF.csv",
                     header = T,
                     row.names = 1,
                     stringsAsFactors = F)

tweetTexts <- tweetsDF %>% 
  group_by(screeName) %>%
  summarise(text = paste(text, collapse = " "))
# - accNames is to be used later:
accNames <- tweetTexts$screeName
accNames <- append(accNames, "askNASA")
tweetTexts <- tweetTexts$text
askNasaText <- paste(dT$text, collapse = "")
tweetTexts <- append(tweetTexts, askNasaText)
tweetTexts <- enc2utf8(tweetTexts)
tweetTexts <- VCorpus(VectorSource(tweetTexts))

# - Term-Doc Matrix for this:
removePunctuationSpecial <- function(x) {
  x <- gsub("#", "HASHCHAR", x)
  x<- gsub("@", "MONKEYCHAR", x)
  x <- gsub("[[:punct:]]+", "", x)
  x <- gsub("HASHCHAR", "#", x)
  x <- gsub("MONKEYCHAR", "@", x)
  return(x)
}
tweetTexts <- tm_map(tweetTexts, 
                     content_transformer(removePunctuationSpecial),
                     lazy = TRUE)
tweetsTDM <- TermDocumentMatrix(tweetTexts,
                                control = list(tolower = FALSE,
                                               removePunctuation = FALSE,
                                               removeNumbers = TRUE,
                                               removeWords = list(stopwords("english")),
                                               stripWhitespace = TRUE,
                                               stemDocument = FALSE,
                                               minWordLength = 3,
                                               weighting = weightTf))
# - store TDM object:
saveRDS(tweetsTDM, "tweetsTDM.Rds")

# - extract only mention and hashtag features:
tweetsTDM <- t(as.matrix(tweetsTDM))
w <- which(grepl("^@|^#", colnames(tweetsTDM)))
tweetsTDM <- tweetsTDM[, -w]
# - keep only mention and hashtag features w. Freq > 50
wK <- which(colSums(tweetsTDM) > 10)
tweetsTDM <- tweetsTDM[, wK]
# - transform to binary for Jaccard distance
wPos <- which(tweetsTDM > 0, arr.ind = T)
tweetsTDM[wPos] <- 1
# - Jaccard distances for accounts and #asknasa:
simAccounts <- dist(tweetsTDM, method = "Jaccard", by_rows = T)
simAccounts <- as.matrix(simAccounts)
diag(simAccounts) <- 1
colnames(simAccounts) <- accNames
rownames(simAccounts) <- accNames
# - For each account + #asknasa, pick the least dissimilar one:
connected <- apply(simAccounts, 1, function(x) {
  accNames[which.min(x)]
})
# - keep 80 most closely related to #askNASA:
askNASAProx <- names(sort(simAccounts[, 'askNASA'], decreasing = F)[1:80])
askNASAProx <- append(askNASAProx, 'askNASA')
inx <- which(accNames %in% askNASAProx)

# - Visualize w. {igraph}
accountNet <- data.frame(ougoing = accNames[inx],
                         incoming = connected[inx],
                         stringsAsFactors = F)

accountNet <- graph.data.frame(accountNet, directed = T)

# - plot w. {igraph}
par(mai=c(rep(0,4)))
plot(accountNet,
     edge.width = .75,
     edge.color = "darkcyan",
     edge.arrow.size = 0.15,
     vertex.size = 2,
     vertex.color = "white",
     vertex.label.color = "black",
     vertex.label.font = 1,
     vertex.label.family = "sans",
     vertex.label.cex = .55,
     vertex.label.dist = .25,
     vertex.label.dist = .45,
     edge.curved = 0.5,
     margin = c(rep(0,4)))






