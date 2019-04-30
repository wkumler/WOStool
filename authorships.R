#Localauthors
#A script that queries WOS with an author's name and outputs a network of
#  the authors they frequently collaborate with

# Startup things ----

library(httr)
library(XML)
library(xml2)
library(corrplot)
library(ggraph)
library(igraph)

person <- "Ng Ren"
saveworthy <- F

query <- paste0("AU=", person)
#query <- "TS=bacteria"



# Request SID from Web of Science ----
endpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate"

body <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"\n    xmlns:auth="http://auth.cxf.wokmws.thomsonreuters.com">\n    <soapenv:Header/>\n    <soapenv:Body>\n        <auth:authenticate/>\n    </soapenv:Body>\n    </soapenv:Envelope>'
if(!exists("SID")){
  response <- POST(endpoint, body = body)
  print(response$status_code)
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



# Extract and clean author names ----

raw_authors <- xml_find_all(doc, xpath = "//authors")
authorlist <- as_list(raw_authors)
authorvec <- unlist(authorlist)
authorvec <- authorvec[!(names(authorvec)=="label")]
names(authorvec) <- NULL

extract_name <- function(fullname) {
  commaspace <- regexpr(", ", fullname)[1]
  substr(fullname, 1, commaspace+2)
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

simpleauthors <- sapply(authorvec, extract_name, USE.NAMES = F)
simpleauthors <- simpleauthors[!sapply(X = simpleauthors, nchar)==1]
capitalauthors <- sapply(simpleauthors, simpleCap, USE.NAMES = F)

otherauthors <- capitalauthors#[-grep(gsub(" ", ", ", substr(person, 1, regexpr(" ", person)[1]+1)), capitalauthors)]



# Create association matrix (adjacency) ----

dims <- length(unique(otherauthors))
matnames <- sort(unique(otherauthors))

authormat <- matrix(0, nrow=dims, ncol=dims)
rownames(authormat) <- matnames
colnames(authormat) <- matnames

for(paper in 1:length(authorlist)){
  coauthors <- as.character(unlist(authorlist[[paper]][-1]))
  coauthors <- sapply(coauthors, extract_name, USE.NAMES = F)
  coauthors <- sapply(coauthors, simpleCap, USE.NAMES = F)
  for(i in coauthors){
    xloc <- grep(i, colnames(authormat))
    for(j in coauthors){
      yloc <- grep(j, rownames(authormat))
      authormat[xloc, yloc] <- authormat[xloc, yloc]+1
    }
  }
}
diag(authormat) <- 0



# Plot correlation matrix ----
if(saveworthy) {
png(filename = paste0("Images/", gsub(" ", "_", person), "_corrplot.png"), 
    width = 2500, height = 2500, units = "px")
}
corrplot(authormat/max(authormat), type = "upper", 
         tl.col = "black", tl.srt = 60, tl.cex = 1,
         order = "hclust", 
         method = "square",
         cl.lim = c(0,1),
         addgrid.col = "grey90"
         )
if(saveworthy) {dev.off()}



# Plot network diagrams ----
network=graph_from_adjacency_matrix(authormat, mode='undirected', diag=F, weighted = T)

waits <- E(network)$weight/3

if(saveworthy) {
png(filename = paste0("Images/", gsub(" ", "_", person), "_net_ann.png"), 
    width = 1800, height = 1800, units = "px")
}
ggraph(network, layout = "auto") + 
  geom_edge_link( aes(edge_alpha=waits/max(waits)), edge_width=1, edge_colour="black") +
  scale_edge_alpha(name="Collaboration\n frequency") +
  geom_node_point( color="#69b3a2", size=5) +
  geom_node_text( aes(label=name), repel = TRUE, size=8, color="#69b3a2") +
  theme_void() +
  theme(
    plot.margin=unit(rep(1,4), "cm")
  )
if(saveworthy) {dev.off()}

if(saveworthy) {
png(filename = paste0("Images/", gsub(" ", "_", person), "_net.png"), 
    width = 1000, height = 1000, units = "px")
}
ggraph(network) + 
  geom_edge_link( aes(edge_alpha=waits/max(waits)), edge_width=1, edge_colour="black") +
  scale_edge_alpha(name="Collaboration\n frequency") +
  geom_node_point( color="#69b3a2", size=2) +
  theme_void() +
  theme(
    plot.margin=unit(rep(1,4), "cm")
  )
if(saveworthy) {dev.off()}

