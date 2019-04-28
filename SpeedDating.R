#SpeedDating
# Takes WOS output and does timeline stuff with it, I guess

# Startup things ----

library(httr)
library(XML)
library(xml2)
library(ggplot2)
library(plotly)

person <- "Koehl MAR"
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



# Use SID to access information ----

endpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite"
bod1 <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"\n    xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">\n    <soapenv:Header/>\n    <soapenv:Body>\n    <woksearchlite:search>\n    <queryParameters>\n    <databaseId>WOS</databaseId>\n    <userQuery>'
bod2 <- '</userQuery>\n    <timeSpan>\n    <begin>1900-01-01</begin>\n    <end>2050-12-31</end>\n    </timeSpan>\n    <queryLanguage>en</queryLanguage>\n    </queryParameters>\n    <retrieveParameters>\n    <firstRecord>1</firstRecord>\n    <count>100</count>\n    </retrieveParameters>\n    </woksearchlite:search>\n    </soapenv:Body>\n    </soapenv:Envelope>'

body <- paste0(bod1, query, bod2)

response <- POST(endpoint, body = body, add_headers(cookie=paste0("SID=", SID)))
print(response$status_code)



# Extract years and titles ----
doc <- read_html(response)
raw_xml <- as.character(doc)
splitdoc <- unlist(strsplit(raw_xml, "<records>"))[-1]
yearGrab <- function(textchunk){
  yearstart <- regexpr("<label>Published.BiblioYear</label>", textchunk)+42
  yearend <- yearstart+3
  substr(textchunk, yearstart, yearend)
}
years <- sapply(splitdoc, yearGrab, USE.NAMES = F)
yeartable <- table(years)

raw_titles <- xml_text(xml_find_all(doc, xpath = "//title"))
titles <- gsub("^Title", "", raw_titles)

#Set the y coordinates
uniqueyears <- as.numeric(names(yeartable))
ycoords <- rep(uniqueyears, as.numeric(yeartable))

#Set the x coordinates
spreadX <- function(nspots){
#  spacing <- 1/(nspots+1)
#  ylocs <- seq(0, 1, by=spacing)
#  lastone <- length(ylocs)
#  return(ylocs[-c(1,lastone)])
  return(1:nspots)
}
xcoords <- unlist(lapply(as.numeric(yeartable), spreadX))

#Clean up the titles
titleClean <- function(title){
  outlen <- 40
  len <- nchar(title)
  if(len<=outlen){
    return(title)
  }
  spaces <- as.numeric(gregexpr(" ", title)[[1]])
  nlines <- ceiling(len/outlen)
  
  newtitle <- substr(title, 1, ceiling(len/nlines))
  for(i in 2:nlines){ #skip the first one to avoid putting a newline after the first word
    chunk <- substr(title, ceiling(len/nlines)*(i-1)+1, ceiling(len/nlines)*i)
    nchunk <- sub(" ", "\n ", chunk)
    newtitle <- paste0(newtitle, nchunk)
    #Divide the title into long chunks and replace the first space of each with a newline
  }
  #works, but match only finds the one location
  newtitle
}
clean_titles <- sapply(titles, titleClean,USE.NAMES = F)

#Collect the data
cleandf <- data.frame("Title"=clean_titles, "ycoord"=ycoords, "xcoord"=xcoords)






# And plot ----

gp <- ggplot() +
  geom_point(data = cleandf, aes(x=xcoord, y=ycoord, label=Title), 
             shape = 21, colour = "#69b3a2", fill = "#69b3a2aa", size = 5, stroke = 2) +
  theme_minimal() +
  theme(axis.line.x =element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

ggplotly(gp)
