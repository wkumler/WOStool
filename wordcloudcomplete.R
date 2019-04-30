#Cloudmaking
#A script that queries WOS with an author's name and outputs a word cloud of
#  their paper titles

# Startup things ----

library(httr)
library(XML)
library(xml2)
library(tm)
library(wordcloud)

person <- "Van Mooy"
saveworthy <- F

query <- paste0("AU=", person)
#query <- "TS=bacteria"



# Request SID from Web of Science ----
endpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate"

body <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"\n    xmlns:auth="http://auth.cxf.wokmws.thomsonreuters.com">\n    <soapenv:Header/>\n    <soapenv:Body>\n        <auth:authenticate/>\n    </soapenv:Body>\n    </soapenv:Envelope>'
if(!exists("SID")){
  response <- POST(endpoint, body = body)
  raw_SID = response$headers$`set-cookie`
  SID = gsub("SID=", "", raw_SID)
} else if(length(SID)==0) {
  response <- POST(endpoint, body = body)
  raw_SID = response$headers$`set-cookie`
  SID = gsub("SID=", "", raw_SID)
} else {
  print("Reusing old SID")
}

if(response$status_code==500){
  stop("Internal server error!")
}



# Perform a search in WOS using the SID and a the query provided at top ----
endpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite"
bod1 <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"\n    xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">\n    <soapenv:Header/>\n    <soapenv:Body>\n    <woksearchlite:search>\n    <queryParameters>\n    <databaseId>WOS</databaseId>\n    <userQuery>'
bod2 <- '</userQuery>\n    <timeSpan>\n    <begin>1900-01-01</begin>\n    <end>2050-12-31</end>\n    </timeSpan>\n    <queryLanguage>en</queryLanguage>\n    </queryParameters>\n    <retrieveParameters>\n    <firstRecord>1</firstRecord>\n    <count>100</count>\n    </retrieveParameters>\n    </woksearchlite:search>\n    </soapenv:Body>\n    </soapenv:Envelope>'

body <- paste0(bod1, query, bod2)

response <- POST(endpoint, body = body, add_headers(cookie=paste0("SID=", SID)))
print(response$status_code)

doc <- read_html(response)
query_id <- xml_text(xml_find_all(doc, xpath = "//queryid"))
rec_cnt <- xml_text(xml_find_all(doc, xpath = "//recordsfound"))

print(query_id)
print(rec_cnt)
if(rec_cnt=="0") {
  stop("No records found")
}

raw_titles <- xml_text(xml_find_all(doc, xpath = "//title"))
titles <- gsub("^Title", "", raw_titles)



# And create a word cloud with it ----
wordiness <- sapply(X = titles, FUN = strsplit, " ")
names(wordiness) <- NULL
wordiness <- unlist(wordiness)
docs <- VCorpus(VectorSource(wordiness))

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
#docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#d <- d[-1,] #use if searching topics
#d <- d[-c(1,2),] #use if searching topics

if(saveworthy) {
  png(filename = paste0("Images/", gsub(" ", "_", person), "_cloud.png"), 
      width = 850, height = 850, units = "px")
}
suppressWarnings(
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          random.order = F, rot.per = 0, max.words = 50,
          colors = brewer.pal(9,"BuGn"), scale = c(4, .2))
)
if(saveworthy) {dev.off()}
