#SpeedDating
# Takes WOS output and does timeline stuff with it, I guess

# Startup things ----

library(httr)
library(XML)
library(xml2)
library(ggplot2)
library(dplyr)
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


#Collect publication years
raw_years <- unlist(strsplit(raw_xml, "<records>"))[-1]

yearGrab <- function(textchunk){
  yearstart <- regexpr("<label>Published.BiblioYear</label>", textchunk)+42
  yearend <- yearstart+3
  substr(textchunk, yearstart, yearend)
}
years <- as.numeric(sapply(raw_years, yearGrab, USE.NAMES = F))


#Collect publication titles
raw_titles <- xml_text(xml_find_all(doc, xpath = "//title"))
titles <- gsub("^Title", "", raw_titles)

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
clean_df <- data.frame("Title"=clean_titles, "Year"=years)
sorted_df <- clean_df[order(clean_df$Year, clean_df$Title),]
num_within_year <- as.numeric(table(sorted_df$Year))
year_id <- character(0)
for(i in num_within_year){
  if(i!=0){
    year_id <- c(year_id, i:1)
  }
}

final_df <- cbind(sorted_df, year_id)


# And plot ----

gp <- ggplot(final_df) +
  geom_bar(aes(x=Year, fill=year_id, label=Title, group=interaction(Year, year_id)), 
           position = position_stack(reverse = T)) +
  coord_flip() +
  scale_fill_viridis_d() +
  scale_y_continuous("Papers published", labels = as.character(1:max(year_id)), breaks = 1:max(year_id)) +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(gp, tooltip = c("label", "x"))
