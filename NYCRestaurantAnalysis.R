# Analysis of Chinese Restaurants in NYC
#
# @author Kelly Xie


############### Clean NYC restaurant health inspection data ###############


library(dplyr)
require(dplyr)

# Clean up NYC Open Data's Chinese restaurant inspections from the last 5 years, 2013-2017
inspections = read.csv("~/Desktop/Chinese_Restaurants.csv")

# The dataset features multiple inspections for any given restaurant
# Calculate the average inspection score for each unique restaurant
avg.scores = inspections %>% group_by(DBA) %>% 
  summarise(AVG.SCORE = mean(SCORE, na.rm=TRUE))

# Merge average inspection scores into inspections dataframe
inspections = merge(x = inspections, y = avg.scores, by = "DBA", all = TRUE)

# Assign an average letter grade to each restaurant based on their average inspection scores
# How sanitation grades are awarded in NYC:
# A = 0-13 points
# B = 14-27 points
# C = 28+ points
inspections$AVG.GRADE = ifelse(inspections$AVG.SCORE < 14, "A", 
                               ifelse(inspections$AVG.SCORE < 28, "B", "C"))


############### Scrape and clean Yelp restaurant data #################


# Extract Yelp data.
library(rvest)
library(stringr)

# Collect the following restaurant data from pages 1-100 of "Chinese" section:
# name, location, price point, rating, number of reviews, URL
yelp.data=NULL
for (i in seq(1,990,10)) { # loop for collecting data across multiple pages
  
  # name
  name = read_html(paste("https://www.yelp.com/search?find_loc=New+York,+NY&start=", i, "&cflt=chinese", sep="")) %>%
    html_nodes("#super-container .js-analytics-click") %>%
    html_text()
  name = name[c(F,T)][-1] # exclude duplicate entries and paid ads
  
  # location
  location = read_html(paste("https://www.yelp.com/search?find_loc=New+York,+NY&start=", i, "&cflt=chinese", sep="")) %>%
    html_nodes(".neighborhood-str-list") %>%
    html_text()
  location = gsub("^\\s+|\\s+$", "", location, fixed = FALSE)[-1] # strip whitespace
  
  # price point
  price = read_html(paste("https://www.yelp.com/search?find_loc=New+York,+NY&start=", i, "&cflt=chinese", sep="")) %>%
    html_nodes(".price-range") %>%
    html_text()
  
  # rating
  rating = read_html(paste("https://www.yelp.com/search?find_loc=New+York,+NY&start=", i, "&cflt=chinese", sep="")) %>%
    html_nodes(".rating-large") %>%
    html_attr("title")
  rating = substring(rating, 1,3)[-1]
  
  # number of reviews
  numreviews = read_html(paste("https://www.yelp.com/search?find_loc=New+York,+NY&start=", i, "&cflt=chinese", sep="")) %>%
    html_nodes(".rating-qualifier") %>%
    html_text()
  numreviews = gsub("^\\s+|\\s+$", "", numreviews, fixed = FALSE) # strip whitespace
  numreviews = substring(numreviews, 1, nchar(numreviews)-8)[-1]
  
  # URL
  url = read_html(paste("https://www.yelp.com/search?find_loc=New+York,+NY&start=", i, "&cflt=chinese", sep="")) %>%
    html_nodes("#super-container .js-analytics-click") %>%
    html_attr("href")
  url = url[c(TRUE,FALSE)][-1] # exclude duplicates and paid ads
  
  # Combine data
  data=NULL
  data=cbind(name, location, rating, price, numreviews, url)

  # Store data
  yelp.data = rbind(yelp.data, data)
}
nrow(yelp.data) # get total number of restaurants
yelp.data = as.data.frame(yelp.data) # convert vector to data frame
yelp.data$name = toupper(yelp.data$name) # convert names to uppercase


# Collect and clean the textual reviews from each URL
review.text=NULL
for (i in 1:nrow(yelp.data)) { # loop for collecting review data in each URL
  
  # Get top 20 useful reviews for each restaurant
  text = read_html(paste("https://www.yelp.com", yelp.data$url[i], sep="")) %>%
    html_nodes(".review-content p") %>%
    html_text()
  text = gsub("^\\s+|\\s+$", "", text, fixed = FALSE) # strip whitespace
  text = gsub('"', '', text, fixed = FALSE) # replace quotations
  text = paste(text, collapse=" ") # compress text into one block
  
  # Compile reviews
  review.text = rbind(review.text, text)
}

# Store reviews in the data frame created from before
yelp.data = cbind(yelp.data, review.text)
length(review.text) # get total number of reviews
review.text[1] # peek at content of reviews


########################## Summary statistics ##########################


# Join 2 dataframes (yelp.data and inspections) using restaurant name as primary key:
# Dataframe of total unique inspections
all.data = merge(x=yelp.data, y=inspections, by.x = "name", by.y = "DBA")

# Dataframe of total unique restaurants
alldata =  merge(x= yelp.data, 
                 y= inspections[,c("DBA", "BORO", "BUILDING",
                                  "STREET", "ZIPCODE", "AVG.SCORE", "AVG.GRADE")], 
                 by.x="name", by.y="DBA")
alldata = unique(alldata)

# Plot ratings and grades for each restaurant
plot1 = ggplot(data=(alldata), aes(x=as.character(rating))) +
  geom_bar(aes(fill=as.character(AVG.GRADE)), position='stack') +
  labs(title='Rating and Grade by Restaurant', 
       x='Yelp Rating', 
       y='Restaurants') +
  theme_bw() +
  theme(legend.key=element_blank()) +
  scale_fill_manual(name="Grade", values = c("royalblue", "mediumseagreen", "orange")) +
  geom_text(stat='count', aes(label=..count..), vjust = -1)
plot1

# Plot ratings and grades at the inspection level
plot2 = ggplot(data=(all.data), aes(x=as.character(rating))) + 
  geom_bar(aes(fill=as.character(AVG.GRADE)), position='stack') +
  labs(title='Rating and Grade by Unique Inspections', 
       x='Yelp Rating', 
       y='Inspections') +
  theme_bw() +
  theme(legend.key=element_blank()) +
  scale_fill_manual(name="Grade", values = c("royalblue", "mediumseagreen", "orange")) +
  geom_text(stat='count', aes(label=..count..), vjust = -1)
plot2

# Plot inspection scores over time (time based analysis)
all.data$newINSPECTION.DATE = strptime(all.data$INSPECTION.DATE,"%m/%d/%Y")
plot3 = ggplot() + 
  geom_point(data=all.data[which(all.data$AVG.GRADE=='A'),], 
             aes(x=as.Date(newINSPECTION.DATE), y=AVG.SCORE, color='A'), position='jitter') +
  geom_point(data=all.data[which(all.data$AVG.GRADE=='B'),], 
             aes(x=as.Date(newINSPECTION.DATE), y=AVG.SCORE, color='B'), position='jitter') +
  geom_point(data=all.data[which(all.data$AVG.GRADE=='C'),], 
             aes(x=as.Date(newINSPECTION.DATE), y=AVG.SCORE, color='C'), position='jitter') +
  xlab("Inspection Date") +
  ylab("Inspection Scores") +
  ggtitle("Inspection Scores over time for NYC Chinese Restaurants") +
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(date_breaks="1 month", date_labels = "%b %Y") +
  scale_colour_manual(name="Grades", values=c("royalblue", "mediumseagreen", "orange"))
plot3

# Get total number of inspections for each restaurant and append to dataframe
numinspections = all.data %>% count(name)
alldata2 = merge(x=alldata, y=numinspections, by='name')
alldata2 = unique(alldata2)


############### Scatterplot maps of Chinese restaurants by grade + rating ###############


library(ggplot2)
library(ggmap)
library(RJSONIO)

# Call GoogleMaps Geocode API to get coordinates for all addresses
geocode <- function(address) {
  require(RJSONIO)
  url <- "https://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, 
                         "&sensor=false&key=AIzaSyBlRlJJunYGhTrxlVCLxui9ZB4yCWYmPLA", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

# Call function on first 1000 restaurants in dataset
coordinates=NULL
for (i in 1:1000) {
  latlon = geocode(paste(alldata2[i,]$BUILDING, alldata2[i,]$STREET, "New York, NY", alldata2[i,]$ZIPCODE))
  coordinates = rbind(coordinates, cbind(name = as.character(alldata2[i,]$name), lon=latlon[1], lat=latlon[2]))
}

alldata3 = merge(x=coordinates, y=alldata2[1:130,], by='name') # Merge the dataframes
alldata3 = unique(alldata3)

# Build map that visualizes how the A/B grade restaurants are graded accordingly
NYCmap = ggmap(get_googlemap(center='New York City', scale=2, zoom=12), extent="normal")

mapAgrade = NYCmap +
  geom_point(data=alldata3[which(alldata3$AVG.GRADE == 'A' & (alldata3$rating == '5.0' | alldata3$rating == '4.5')),], 
             aes(x=as.numeric(as.character(lon)), y=as.numeric(as.character(lat)), col="*****", 
             alpha=0.4, size=5)) +
  geom_point(data=alldata3[which(alldata3$AVG.GRADE == 'A' & (alldata3$rating == '4.0' | alldata3$rating == '3.5')),], 
             aes(x=as.numeric(as.character(lon)), y=as.numeric(as.character(lat)), col="****", 
                 alpha=0.4, size=5)) +
  geom_point(data=alldata3[which(alldata3$AVG.GRADE == 'A' & alldata3$rating == '3.0'),], 
             aes(x=as.numeric(as.character(lon)), y=as.numeric(as.character(lat)), col="***", 
                 alpha=0.4, size=5)) +
  scale_colour_manual(name="Stars", values=c('*****' = "red3", '****' = "orange3", '***' = "yellow3"))
mapAgrade

mapBgrade = NYCmap +
  geom_point(data=alldata3[which(alldata3$AVG.GRADE == 'B' & (alldata3$rating == '5.0' | alldata3$rating == '4.5')),], 
             aes(x=as.numeric(as.character(lon)), y=as.numeric(as.character(lat)), col="*****", 
                 alpha=0.4, size=5)) +
  geom_point(data=alldata3[which(alldata3$AVG.GRADE == 'B' & (alldata3$rating == '4.0' | alldata3$rating == '3.5')),], 
             aes(x=as.numeric(as.character(lon)), y=as.numeric(as.character(lat)), col="****", 
                 alpha=0.4, size=5)) +
  geom_point(data=alldata3[which(alldata3$AVG.GRADE == 'B' & alldata3$rating == '3.0'),], 
             aes(x=as.numeric(as.character(lon)), y=as.numeric(as.character(lat)), col="***", 
                 alpha=0.4, size=5)) +
  scale_colour_manual(name="Stars", values=c('*****' = "purple3", '****' = "blue3", '***' = "green3"))
mapBgrade

  
########################## Wordclouds by rating and grade ##########################


library("tm")
library("wordcloud")

# Clean and reprocess the text to corpus.
corp.original = VCorpus(VectorSource(all.data$review.text))
corp = tm_map(corp.original, removePunctuation)
corp = tm_map(corp, removeNumbers)
corp = tm_map(corp, content_transformer(removeWords), stopwords("SMART"),lazy=TRUE)
corp = tm_map(corp, content_transformer(tolower),lazy=TRUE)
corp = tm_map(corp, content_transformer(stemDocument),lazy=TRUE)
corp = tm_map(corp, stripWhitespace)

# Generate document-term matrix.
dtm = DocumentTermMatrix(corp)
dtms = removeSparseTerms(dtm, .990)
dtm_matrix = as.matrix(dtms)

# Calculate correlation matrices between Yelp review text and grades.
corrA = cor(as.numeric(all.data$AVG.GRADE =="A"), dtm_matrix)
corrB = cor(as.numeric(all.data$AVG.GRADE =="B"), dtm_matrix)
corrC = cor(as.numeric(all.data$AVG.GRADE =="C"), dtm_matrix)

# Generate wordclouds with 50 terms with highest correlation magnitudes
# with each grade A, B, C
top200A = order(corrA, decreasing=TRUE)[1:200]
top200wordsA = colnames(corrA)[top200A]
A.df = as.data.frame(cbind(term = top200wordsA, corr = corrA[top200A]))
wordcloud(words = as.character(A.df$term)[1:50], 
          freq = as.numeric(as.character(A.df$corr))[1:50],
          scale = c(2.5, 0.5), 
          colors = brewer.pal(9,"Blues"), random.order = FALSE)

top200B = order(corrB, decreasing=TRUE)[1:200]
top200wordsB = colnames(corrB)[top200B]
B.df = as.data.frame(cbind(term = top200wordsB, corr = corrB[top200B]))
wordcloud(words = as.character(B.df$term)[1:50], 
          freq = as.numeric(as.character(B.df$corr))[1:50],
          scale = c(2.5, 0.5), 
          colors = brewer.pal(9,"Greens"), random.order = FALSE)

top200C = order(corrC, decreasing=TRUE)[1:200]
top200wordsC = colnames(corrC)[top200C]
C.df = as.data.frame(cbind(term = top200wordsC, corr = corrC[top200C]))
wordcloud(words = as.character(C.df$term)[1:50], 
          freq = as.numeric(as.character(C.df$corr))[1:50],
          scale = c(2.5, 0.5), 
          colors = brewer.pal(9,"Oranges"), random.order = FALSE)


################### Running regressions #######################


# Classify restaurants by grade in separate columns
all.data$AVG.GRADE.A = (all.data$AVG.GRADE == 'A')
all.data$AVG.GRADE.B = (all.data$AVG.GRADE == 'B')
all.data$AVG.GRADE.C = (all.data$AVG.GRADE == 'C')

# Fit logistic regression models to see which attributes are significant
# predictors of inspection grades
fitA = glm(AVG.GRADE.A ~ price + rating +
            as.numeric(as.character(numreviews)) + 
            BORO + ZIPCODE, data=all.data, family=binomial)
summary(fitA)

fitB = glm(AVG.GRADE.B ~ price + rating +
             as.numeric(as.character(numreviews)) + 
             BORO + ZIPCODE, data=all.data, family=binomial)
summary(fitB)

fitC = glm(AVG.GRADE.C ~ price + rating +
             as.numeric(as.character(numreviews)) + 
             BORO + ZIPCODE, data=all.data, family=binomial)
summary(fitC)


################### Predictive modeling and machine learning #######################


# Get 200 terms with most predictive power
corr = cor(as.numeric(all.data$AVG.SCORE), dtm_matrix)
absCorr = abs(corr)

top200 = order(absCorr, decreasing=TRUE)[1:200]
top200words = colnames(absCorr)[top200]
new_dtm_matrix = as.data.frame(cbind(Score = as.numeric(all.data$AVG.SCORE), 
                                dtm_matrix[,top200words]))

# Partition data in training and test sets
traindata = all.data[1:(.8*nrow(all.data)),]
testdata = all.data[-(1:(.8*nrow(all.data))),]
train.data = new_dtm_matrix[1:(.8*nrow(new_dtm_matrix)),]
test.data = new_dtm_matrix[-(1:(.8*nrow(new_dtm_matrix))),]

# Run a model to try and predict what score (and corresponding grade) 
# a restaurant will receive based on its Yelp reviews
model = lm(Score ~ ., data = train.data)
summary(model)

# Use the coef command to access top positive and negative words from the model
coef = coef(model)[-1]
pos.terms = coef[coef>0]
top.pos = sort(pos.terms,decreasing=T)[1:15]
top.pos # Top words correlated with a higher violation score

neg.terms = coef[coef<0]
top.neg = sort(neg.terms)[1:15]
top.neg # Top words correlated with a lower violation score

# Predict health inspection scores and get accuracy for training data
train.data$predict_score = predict(model, type="response", data=train.data)
train.accuracy = as.data.frame(cbind(traindata$AVG.SCORE, train.data$predict_score, abs(train.data$predict_score-traindata$AVG.SCORE)))
colnames(train.accuracy)= c("actual", "predict", "difference")
mean(train.accuracy$difference) # we get 1.552124 (very little deviance from actual scores)

# Predict scores and get accuracy for test data
test.data$predict_score = predict(model, type="response", newdata=test.data)
test.accuracy = as.data.frame(cbind(testdata$AVG.SCORE, test.data$predict_score, abs(test.data$predict_score-testdata$AVG.SCORE)))
colnames(test.accuracy)= c("actual", "predict", "difference")
mean(test.accuracy$difference) # we get 14.03286 (much more deviance from actual scores)

