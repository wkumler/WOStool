#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(XML)
library(xml2)
library(ggplot2)
library(plotly)
library(tm)
library(wordcloud)
library(corrplot)
# library(ggraph)
# library(igraph)

# Define all functions ----

getSID <- function(){
  endpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WOKMWSAuthenticate"
  body <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"\n    xmlns:auth="http://auth.cxf.wokmws.thomsonreuters.com">\n    <soapenv:Header/>\n    <soapenv:Body>\n        <auth:authenticate/>\n    </soapenv:Body>\n    </soapenv:Envelope>'
  response <- POST(endpoint, body = body)
  raw_SID = response$headers$`set-cookie`
  SID = gsub("SID=", "", raw_SID)
}

getResponse <- function(SID, query) {
  endpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite"
  bod1 <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"\n    xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">\n    <soapenv:Header/>\n    <soapenv:Body>\n    <woksearchlite:search>\n    <queryParameters>\n    <databaseId>WOS</databaseId>\n    <userQuery>'
  bod2 <- '</userQuery>\n    <timeSpan>\n    <begin>1900-01-01</begin>\n    <end>2050-12-31</end>\n    </timeSpan>\n    <queryLanguage>en</queryLanguage>\n    </queryParameters>\n    <retrieveParameters>\n    <firstRecord>1</firstRecord>\n    <count>100</count>\n    </retrieveParameters>\n    </woksearchlite:search>\n    </soapenv:Body>\n    </soapenv:Envelope>'
  
  body <- paste0(bod1, query, bod2)
  
  response <- POST(endpoint, body = body, add_headers(cookie=paste0("SID=", SID)))
  print(response$status_code)
  return(response)
}

renderWordcloud <- function(response) {
  doc <- read_html(response)
  raw_titles <- xml_text(xml_find_all(doc, xpath = "//title"))
  titles <- gsub("^Title", "", raw_titles)
  
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
  
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            random.order = F, rot.per = 0, max.words = 50,
            colors = brewer.pal(9,"BuGn"), scale = c(4, .2))
}

renderTimeline <- function(response) {
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
  
  #Create a unique annotation within each year for the plot
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
  labelin <- 1:max(as.numeric(year_id))
  
  gp <- ggplot(final_df) +
    geom_bar(aes(x=Year, fill=year_id, label=Title, group=interaction(Year, year_id)), 
             position = position_stack(reverse = T)) +
    coord_flip() +
    scale_fill_viridis_d() +
    scale_y_continuous("Papers published", labels = as.character(labelin), breaks = labelin) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplotly(gp, tooltip = c("label", "x"))
}

renderCorrplot <- function(response) {
  # Extract and clean author names ----
  doc <- read_html(response)
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
  
  
  
  # Plot correlation matrix ----
  corrplot(authormat/max(authormat), type = "upper", 
           tl.col = "black", tl.srt = 60, tl.cex = 1,
           order = "hclust", 
           method = "square",
           cl.lim = c(0,1),
           addgrid.col = "grey90",
           diag = F
  )
  
}

# Define UI ----
ui <- fluidPage(
   titlePanel("Web of Science Interface"),
   
   sidebarLayout(
      sidebarPanel(
         textInput(inputId = "person", 
                   label = "Who are you looking for?", 
                   value = "Ingalls Anitra",
                   placeholder = "Lastname (Firstname optional)"),
         submitButton(),
         checkboxInput("corrplot", "Render a correlation plot?")
         #checkboxInput("network", "Render an authorship network?")
         #checkboxInput("annetwork", "Annotate authorship network?")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h1(textOutput("person")),
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), 
                      plotOutput("wordcloud"), 
                      plotlyOutput("timeline"))
        ),
        plotOutput("corrplot", width = "1200px", height = "1200px"),
        plotOutput("network"),
        plotOutput("networklab")
      )
   )
)



# Define server ----
server <- function(input, output) {
  SID <- getSID()
  output$person <- renderText({
    input$person
  })
  
  response <- reactiveVal()
  observeEvent(input$person, {
    query <- paste0("AU=", input$person)
    newresponse <- getResponse(SID, query)
    response(newresponse)
  })
  
  output$wordcloud <- renderPlot({
    renderWordcloud(response())
  })
  
  output$timeline <- renderPlotly({
    renderTimeline(response())
  })
  
  output$corrplot <- renderPlot({
    if(input$corrplot) {
      renderCorrplot(response())
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

