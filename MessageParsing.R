library(jsonlite)
library(plyr)
library(dplyr)
library(lubridate)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(dygraphs)
library(reshape2)
library(xts)
library(RColorBrewer)
library(scales)

# read in messages.json file as a list
messages<-fromJSON("/opt/FB-Message-Parser/messages.json")

# retrieve the list of message threads (as defined by the people who are in them) 
convos<-messages[[1]]$people


# This function will parse the participants of a message thread and return the index
# of the thread in the list if it includes a matching name
include <- function (theList, toMatch){
  matches <- unique(grep(paste(toMatch,collapse="|"), 
                         theList, value=FALSE))
  return(matches)
}

# Create vector of desired message threads
include(convos,"Dave Perry")
convo_idx<-c(70, 95, 99, 100, 241, 243, 251, 252, 253, seq(254,283), 301)

# Subset original messages list
messages<-messages[[1]]$messages[convo_idx]

# Collapse list of dataframes into single dataframe
messages<-ldply(messages, data.frame)


# Extract only the date & time (i.e. removing Weekday)
messages$date_time<-str_split_fixed(messages$date_time, pattern = ', ', n=2)[,2]

# Convert date & time to POSIXct date-time object
messages$date_time<-parse_date_time(messages$date_time, order="B d! Y! R*")

# Create various time variables for filtering
messages$month<-month(messages$date_time, label = TRUE)
messages$day<-wday(messages$date_time, label=TRUE)
messages$year<-year(messages$date_time)
messages$hour<-hour(messages$date_time)
messages$am<-am(messages$date_time)

messages$time_of_day<-cut(messages$hour, breaks=c(0, 3, 6, 10, 14, 18, 23),
                          labels=c("Late Night", "Early AM", "AM", "Mid-Day", "Afternoon", "Evening"))

# Round down each date to nearest week (will allow for counting message totals by week)
messages$week_year<-floor_date(as.POSIXlt(messages$date_time),"week")

# messages<-messages[!messages$sender %in% c("28405570@facebook.com", "Imam Asad"),]
# messages<-messages[-10901,]

write.csv(messages, "/opt/FB-Message-Parser/messages.csv", row.names=FALSE)

####################
messages<-read.csv("/opt/FB-Message-Parser/messages.csv")



messages$week_year<-as.character(messages$week_year)
mess_chart<-group_by(messages,week_year,sender) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

mess_chart<-dcast(mess_chart, week_year~sender, value.var="freq")
mess_chart$week_year<-ymd(mess_chart$week_year)

mess_chart<-xts(mess_chart, order.by=mess_chart$week_year)


dygraph(mess_chart) %>%
  dyRangeSelector(height = 20) %>%
  dyAxis("x", label = "Week") %>%
  dyHighlight(highlightCircleSize = 5,
               hideOnMouseOut = FALSE,
              highlightSeriesOpts = list(strokeWidth =3)) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(1, "Set1"),
            fillGraph=TRUE)
  

############ Word Cloud work
zerg<-filter(messages, sender=="Chris Nash"|sender=="Malcolm Charles")
protoss<-group_by(messages, sender) %>%
  summarise(SC2 = paste(text, collapse = " "))

text<-as.character(filter(protoss, sender=="Malcolm Charles") %>% select(SC2))

docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
# docs <- tm_map(docs, stripWhitespace)
# docs <- tm_map(docs, stemDocument)

dtm<-TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

wordcloud(words=d$word, freq=d$freq, max.words=30)
d3TextCloud(test,label="dude")
