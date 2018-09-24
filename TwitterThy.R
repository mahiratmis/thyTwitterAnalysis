# install.packages(c("twitteR","ggplot2", "lubridate", "scales", "tm", "stringr", "wordcloud", "syuzhet", "reshape2", "dplyr", "RJSONIO", "leaflet", "rworldmap","topicmodels","LDAvis","servr","stringi"))
library(twitteR)
library(ggplot2)
library(lubridate)
library(scales)
library(tm)
library(stringr)
library(wordcloud)
library(syuzhet)
library(reshape2)
library(dplyr)

library(data.table)
library(RJSONIO)
library(leaflet)

library(sp)
library(rworldmap)


library(topicmodels)
library(LDAvis)
library(servr)
library(stringi)


number_of_tweers_to_search <- 1000
data_dir <- "~/Projects/R/data/"
filename <- paste0("thytwitter",number_of_tweers_to_search,".csv")


consumer_key <- '8kmONz7G**'
consumer_secret <- 'vORYByyR9i**'
access_token <- '1158143544-A4xVk7iCqdeD7eE**'
access_secret <- 'reBZWrtVA02awxpVjmgLM5qyyhfRV**'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


datalist =list()

listofNames <- c("@TurkishAirlines", "@UKTurkish", "@TK_US", "@TK_HelpDesk", "@TK_Netherlands", "@TK_INDIA", "@TK_Norway", "@TA_IRELAND", "@TK_CAN", "#WidenYourWorld")
print("Searching for Keywords")
isFirst <- T
i <- 1
for (searchKeyWord in listofNames){
  print(sprintf("Searching for %s", searchKeyWord))
  allthy <- searchTwitter(searchKeyWord, lang='en', n = number_of_tweers_to_search)
  if(length(allthy) == 0){
    print("Returned no results")
  }
  else{
    allthyDF <- twListToDF(allthy)
    allthyDF$text <- gsub("[[:punct:]]", " ", allthyDF$text) #remove punctuations
    print(sprintf("Returned %i results each having %i columns", nrow(allthyDF),ncol(allthyDF)))
    datalist[[i]] <- allthyDF
    i <- i+1
  }
  Sys.sleep(3) #sleep a while to prevent Twitter API rate limit error.
}


tweets = do.call(rbind, datalist)
write.table(tweets, file = paste0(data_dir,filename), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)

tweetsRead <- read.csv(paste0(data_dir,filename), strip.white=TRUE, sep="\t", stringsAsFactors=FALSE)

# get rid of empty columns 
purified <- tweets[!(is.na(tweets$text) | tweets$text=="" | tweets$text=="."), ]
# get rid of empty columns 
purified <- tweets[!(is.na(tweets$screenName) | tweets$screenName=="" | tweets$screenName=="." | substring(tweets$screenName, 1, 1)=="<"), ]

# remove special characters in the column called text and put the cleaned text in the new column called clean_text
purified$clean_text <- str_replace_all(purified$text, "@\\w+", "")
# remove special characters in the column called text and put the cleaned text in the new column called clean_text
purified$screenName<- str_replace_all(purified$screenName, "@\\w+", "")

# extract sentiments from tweets. The sentiment information is stored in the dataframe called Sentiment
Sentiment <- get_nrc_sentiment(purified$clean_text)
# Combine two dataframes (purified and Sentiment) into a new one called tweets
tweets<- cbind(purified, Sentiment)
print(" === Writing Purified Results to File. ===")
# select only positive and negative tweets
write.table(tweets, file = paste0(data_dir,"TweetsWithSentimentPurified.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)
tweetsPositive <- tweets[tweets$positive>tweets$negative, ]
tweetsNegative <- tweets[tweets$negative>tweets$positive, ]


####################### START TOPIC MODELING ####################################
# thanks to Wayne Weiai Xu https://rpubs.com/cosmopolitanvan/topicmodeling
# defaults parameters
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
# number of topics to find
k <- 5 #find 5 topics


# function to visualize results
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

# topic modeling for POSITIVE TWEETS
tweets_byusers_corpus <- iconv(tweetsPositive$clean_text)
corpus <- Corpus(VectorSource(tweets_byusers_corpus))
# purify corpus
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus,removeWords,stopwords("english")) 
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus,stripWhitespace) 
# create document term matrix
dtm <- DocumentTermMatrix(corpus) 

inspect(dtm[1:5, 1:5])  
dtm.mx <- as.matrix(dtm)
frequency <- colSums(dtm.mx)
frequency <- sort(frequency, decreasing=TRUE)
frequency[1:25] 

# run linear discriminant analysis
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
# write topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste0(data_dir,"topic_model",k,"DocsToTopicsPos.csv"))

# write terms for each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste0(data_dir,"topic_model",k,"TopicsToTermsPos.csv"))
ldaOut.terms[1:6,]

# serVis(topicmodels2LDAvis(ldaOut))

# topic modeling for NEGATIVE TWEETS
neg_tweets_byusers_corpus <- iconv(tweetsNegative$clean_text)
neg_corpus <- Corpus(VectorSource(neg_tweets_byusers_corpus))
# purify neg_corpus
neg_corpus <- tm_map(neg_corpus, content_transformer(tolower)) 
neg_corpus <- tm_map(neg_corpus, removePunctuation) 
neg_corpus <- tm_map(neg_corpus,removeWords,stopwords("english")) 
# neg_corpus <- tm_map(neg_corpus,removeWords,c("mindhunter", "david", "fincher","netflix")) 
neg_corpus <- tm_map(neg_corpus, removeNumbers) 
neg_corpus <- tm_map(neg_corpus,stripWhitespace) 
# create document term matrix
neg_dtm <- DocumentTermMatrix(neg_corpus) 


inspect(neg_dtm[1:5, 1:5])  
neg_dtm.mx <- as.matrix(neg_dtm)
frequency <- colSums(neg_dtm.mx)
frequency <- sort(frequency, decreasing=TRUE)
frequency[1:25] 

# run linear discriminant analysis
neg_ldaOut <-LDA(neg_dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
# write topics
neg_ldaOut.topics <- as.matrix(topics(neg_ldaOut))
write.csv(neg_ldaOut.topics,file=paste0(data_dir,"topic_model",k,"DocsToTopicsNeg.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)

# write terms for each topic
neg_ldaOut.terms <- as.matrix(terms(neg_ldaOut,6))
write.csv(neg_ldaOut.terms,file=paste0(data_dir,"topic_model",k,"TopicsToTermsNeg.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)
neg_ldaOut.terms[1:6,]

# serVis(topicmodels2LDAvis(neg_ldaOut))

####################### END OF TOPIC MODELING ####################################


####################### START GEOGRAPHIC ANALYSIS ################################ 
# create a function for getting coordinates from Google Map API.
# We use the code published by Lucas Puente (http://lucaspuente.github.io/notes/2016/04/05/Mapping-Twitter-Followers)
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyC2__f1WoY***")
}

# how many users per sentiment(positive or negative) do you want to analyze
# uncomment next line to use all users
# n_users_to_use <- nrow(tweetsPositive)
n_users_to_use <- 25

# Process Positive Tweets
print(" === Processing Positive Tweets. ===")
tweetsPositive$user_location_on_twitter_bio <- NA
tweetsPositive$profile_image <- NA

for (user in tweetsPositive$screenName[1:n_users_to_use]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(tweetsPositive[tweetsPositive$screenName==user,]$user_location_on_twitter_bio <- getUser(user)$location)
  try(tweetsPositive[tweetsPositive$screenName==user,]$profile_image <- getUser(user)$profileImageUrl)
}


tweetsPositive$lat <-NA
tweetsPositive$lng <-NA

tweets_withgeo_pos <- tweetsPositive[tweetsPositive$user_location_on_twitter_bio != "" & !is.na(tweetsPositive$user_location_on_twitter_bio),]

# get the coordinate data for all tweets via Google Map API.
for (name in tweets_withgeo_pos$user_location_on_twitter_bio){ 
  rowid<-which(tweets_withgeo_pos$user_location_on_twitter_bio == name)
  print(paste0("getting the coordinates for:",name,", rowid is:",rowid))
  Sys.sleep(3)
  try(geodata <- geocode_apply(name))
  
  if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:",geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lngis:", geodata$results[[1]]$geometry$location[[2]]))
    tweets_withgeo_pos[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    tweets_withgeo_pos[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

# create map for positive messages
# create a separate dataframe called tweets_withgeo_show. This dataframe contains only complete coordinates. 
tweets_withgeo_show_pos <- tweets_withgeo_pos[!is.na(tweets_withgeo_pos$lat),c("lat","lng", "user_location_on_twitter_bio", "profile_image", "text")]
print(" === Positive Results with Geo Info are saved to file. ===")
write.table(tweets_withgeo_show_pos, file = paste0(data_dir,"TweetsWithGeoShowPos.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)
map1 <- leaflet() %>% setView(lng = 32.86, lat = 39.92, zoom = 3)
map1 <- leaflet(data = tweets_withgeo_show_pos) %>% 
  addTiles() %>%
  setView(lng = 32.86, lat = 39.92, zoom = 4) %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio)) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    fillOpacity = 0.2, fillColor= 'green', color = 'green'
  ) 

# create map with user profile images for positive messages
usericon <- makeIcon(
  iconUrl = tweets_withgeo_show_pos$profile_image,
  iconWidth = 15, iconHeight = 15
)
map2 <- leaflet(data = tweets_withgeo_show_pos) %>% 
  addTiles() %>%
  setView(lng = 32.86, lat = 39.92, zoom = 4) %>% 
  addMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio),icon = usericon,data = tweets_withgeo_show_pos) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) 


# Process Negative Tweets
print(" === Processing Negative Tweets. ===")
tweetsNegative$user_location_on_twitter_bio <- NA
tweetsNegative$profile_image <- NA

for (user in tweetsNegative$screenName[1:n_users_to_use]){  
  print(c("finding the profile for:",user))
  Sys.sleep(3) #build in a sleeper to prevent Twitter API rate limit error. 
  try(tweetsNegative[tweetsNegative$screenName==user,]$user_location_on_twitter_bio <- getUser(user)$location)
  try(tweetsNegative[tweetsNegative$screenName==user,]$profile_image <- getUser(user)$profileImageUrl)
}


# create a function for getting coordinates from Google Map API.
# We use the code published by Lucas Puente (http://lucaspuente.github.io/notes/2016/04/05/Mapping-Twitter-Followers)
tweetsNegative$lat <-NA
tweetsNegative$lng <-NA

tweets_withgeo_neg <- tweetsNegative[tweetsNegative$user_location_on_twitter_bio != "" & !is.na(tweetsNegative$user_location_on_twitter_bio),]

# get the coordinate data for all tweets via Google Map API.
for (name in tweets_withgeo_neg$user_location_on_twitter_bio){ 
  rowid<-which(tweets_withgeo_neg$user_location_on_twitter_bio == name)
  print(paste0("getting the coordinates for:",name,", rowid is:",rowid))
  Sys.sleep(1)
  try(geodata <- geocode_apply(name))
  
  if (geodata$status=="OK" & length(geodata$results)=="1") {
    print(c("the lat is:",geodata$results[[1]]$geometry$location[[1]]))
    print(c("the lngis:", geodata$results[[1]]$geometry$location[[2]]))
    tweets_withgeo_neg[rowid,]$lat <- geodata$results[[1]]$geometry$location[[1]]
    tweets_withgeo_neg[rowid,]$lng <- geodata$results[[1]]$geometry$location[[2]]
  }else {
    print ("skipping")
  }
}

# create map for positive messages
# create a separate dataframe called tweets_withgeo_show. This dataframe contains only complete coordinates. 
tweets_withgeo_show_neg <- tweets_withgeo_neg[!is.na(tweets_withgeo_neg$lat),c("lat","lng", "user_location_on_twitter_bio", "profile_image", "text")]
print(" === Negative Results with Geo Info are saved to file. ===")
write.table(tweets_withgeo_show_neg, file = paste0(data_dir,"TweetsWithGeoShowNeg.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)

map3 <- leaflet() %>% setView(lng = 32.86, lat = 39.92, zoom = 3)
map3 <- leaflet(data = tweets_withgeo_show_neg) %>% 
  addTiles() %>%
  setView(lng = 32.86, lat = 39.92, zoom = 4) %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio)) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(
    fillOpacity = 0.2, fillColor= 'red', color = 'red'
  ) 

# create map with user profile images for negative messages
usericon <- makeIcon(
  iconUrl = tweets_withgeo_show_neg$profile_image,
  iconWidth = 15, iconHeight = 15
)
map4 <- leaflet(data = tweets_withgeo_show_neg) %>% 
  addTiles() %>%
  setView(lng = 32.86, lat = 39.92, zoom = 4) %>% 
  addMarkers(lng = ~lng, lat = ~lat, popup = ~ as.character(user_location_on_twitter_bio),icon = usericon,data = tweets_withgeo_show_neg) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) 
map4


# combine the two maps
map5 <- leaflet() %>% setView(lng = 32.86, lat = 39.92, zoom = 3)
map5 <- leaflet(data = rbind(tweets_withgeo_show_pos, tweets_withgeo_show_neg)) %>% 
  addTiles() %>%
  setView(lng = 32.86, lat = 39.92, zoom = 4) %>% 
  addCircleMarkers(data=tweets_withgeo_show_neg, lng = ~lng, lat = ~lat, fillOpacity = 0.2, fillColor= 'red', color = 'red', popup = ~ as.character(user_location_on_twitter_bio)) %>% 
  addCircleMarkers(data=tweets_withgeo_show_pos, lng = ~lng, lat = ~lat, fillOpacity = 0.2, fillColor= 'green', color = 'green', popup = ~ as.character(user_location_on_twitter_bio)) %>%     
  addProviderTiles(providers$Esri.NatGeoWorldMap)
####################### END OF GEOGRAPHIC ANALYSIS ############################

## GENERATE COUNTRY DISTRIBUTION OF SENTIMENTS

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  # countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  # indices$ISO3 # returns the ISO3 code 
  # indices$continent   # returns the continent (6 continent model)
  # indices$REGION   # returns the continent (7 continent model)
}

####################### GROUP BY COUNTRY NAME  ############################
print(" === Sentiments Per Country are being analyzed ===")
# negative countries
neg_points <- tweets_withgeo_show_neg[,c(2,1)]
neg_points$country <- coords2country(neg_points) 
tweets_withgeo_show_neg$country <- neg_points$country
print(" === Negative Results with Country Info are saved to file. ===")
write.table(tweets_withgeo_show_neg, file = paste0(data_dir,"TweetsWithCountryShowNeg.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)
# Group Negative Points by Country
my_stats_neg <- tweets_withgeo_show_neg %>%
  group_by(country) %>%
  summarise(count=n())
write.table(my_stats_neg, file = paste0(data_dir,"CountryNegCount.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)

# Save Negative Plot
# Give the chart file a name.
png(file = paste0(data_dir,"barchart_country_negcount.png"))

# Plot the bar chart.
barplot(my_stats_neg$count,names.arg = my_stats_neg$country,xlab = "Country",ylab = "Number of Tweets",col = "blue",
        main = "Negative Sentiment Chart",border = "red", las=2)

# Save the file.
dev.off()



# positive countries
pos_points <- tweets_withgeo_show_pos[,c(2,1)]
pos_points$country <- coords2country(pos_points) 
tweets_withgeo_show_pos$country <- pos_points$country
print(" === Positive Results with Country Info are saved to file. ===")
write.table(tweets_withgeo_show_pos, file = paste0(data_dir,"TweetsWithCountryShowPos.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)

# Group Positive Points by Country
my_stats_pos <- tweets_withgeo_show_pos %>%
  group_by(country) %>%
  summarise(count=n())

write.table(my_stats_pos, file = paste0(data_dir,"CountryPosCount.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)

# Save Positive Plot
# Give the chart file a name.
png(file =  paste0(data_dir,"barchart_country_poscount.png"))

# Plot the bar chart.
barplot(my_stats_pos$count,names.arg = my_stats_pos$country,xlab = "Country",ylab = "Number of Tweets",col = "blue",
        main = "Positive Sentiment Chart",border = "red", las=2)
# Save the file.
dev.off()
####################### END OF GROUP BY COUNTRY NAME  ####################

####################### GROUP BY SCREEN NAME  ############################
print(" === Sentiments Per Screen Name are being analyzed ===")
# negative screen names
# Group Negative Points by screen name
screenname_grouping_neg <- tweetsNegative %>%
  group_by(screenName) %>%
  summarise(count=n())
write.table(screenname_grouping_neg, file = paste0(data_dir,"ScreenNameNegCount.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)

# Save Negative Plot
# Give the chart file a name.
png(file = paste0(data_dir,"barchart_screenname_negcount.png"))

screenname_grouping_neg <- screenname_grouping_neg[order(screenname_grouping_neg$count, decreasing = TRUE), ]

# Plot the bar chart.
barplot(screenname_grouping_neg$count[1:10],names.arg = screenname_grouping_neg$screenName[1:10],xlab = "ScreenName",ylab = "Number of Tweets",col = "blue",
        main = "Negative Sentiment Chart",border = "red", las=2)

# Save the file.
dev.off()


# positive screen names
# Group Positive Points by screen name
screenname_grouping_pos <- tweetsPositive %>%
  group_by(screenName) %>%
  summarise(count=n())
write.table(screenname_grouping_pos, file = paste0(data_dir,"ScreenNamePosCount.csv"), row.names=FALSE, na="", col.names=T, sep="\t", quote =T)

# Save Negative Plot
# Give the chart file a name.
png(file = paste0(data_dir,"barchart_screenname_poscount.png"))


screenname_grouping_pos <- screenname_grouping_pos[order(screenname_grouping_pos$count, decreasing = TRUE), ]

# Plot the bar chart.
barplot(screenname_grouping_pos$count[1:10],names.arg = screenname_grouping_pos$screenName[1:10],xlab = "ScreenName",ylab = "Number of Tweets",col = "blue",
        main = "Positive Sentiment Chart",border = "red", las=2)

# Save the file.
dev.off()
####################### END OF GROUP BY SCREEN NAME  ######################


####################### GROUP BY DATE  ####################################
# group negatives by month
png(file = paste0(data_dir,"barchart_bydate_poscount.png"))
tweetsPositiveCopy <- tweetsPositive %>% 
  group_by(created=floor_date(created, "day")) %>%
  summarize(positive=sum(positive))
# Plot the bar chart.
barplot(tweetsPositiveCopy$positive, names.arg = tweetsPositiveCopy$created, xlab = "Date",ylab = "Number of Tweets",col = "blue",
        main = "Positive Sentiment Chart",border = "red", las=2)

# Save the file.
dev.off()

# group positives by day
png(file = paste0(data_dir,"barchart_bydate_negcount.png"))
tweetsNegativeCopy <- tweetsNegative %>% 
  group_by(created=floor_date(created, "day")) %>%
  summarize(negative=sum(negative))
# Plot the bar chart.
barplot(tweetsNegativeCopy$negative, names.arg = tweetsNegativeCopy$created, xlab = "Date",ylab = "Number of Tweets",col = "blue",
        main = "Negative Sentiment Chart",border = "red", las=2)

# Save the file.
dev.off()
####################### END OF GROUP BY DATE  ############################
