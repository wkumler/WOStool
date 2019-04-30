#Let's see if we can get access to the full record information in WOS

# Startup things ----

library(httr)
library(XML)
library(xml2)



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



# Perform a search for using the SID obtained ----
endpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite"
bod1 <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"\n    xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">\n    <soapenv:Header/>\n    <soapenv:Body>\n    <woksearchlite:search>\n    <queryParameters>\n    <databaseId>WOS</databaseId>\n    <userQuery>'
bod2 <- '</userQuery>\n    <timeSpan>\n    <begin>1900-01-01</begin>\n    <end>2050-12-31</end>\n    </timeSpan>\n    <queryLanguage>en</queryLanguage>\n    </queryParameters>\n    <retrieveParameters>\n    <firstRecord>1</firstRecord>\n    <count>100</count>\n    </retrieveParameters>\n    </woksearchlite:search>\n    </soapenv:Body>\n    </soapenv:Envelope>'
query <- "AU=Koehl Mimi"

body <- paste0(bod1, query, bod2)

response <- POST(endpoint, body = body, add_headers(cookie=paste0("SID=", SID)))
print(response$status_code)

doc <- read_html(response)
query_id <- xml_text(xml_find_all(doc, xpath = "//queryid"))
rec_cnt <- xml_text(xml_find_all(doc, xpath = "//recordsfound"))

print(query_id)
print(rec_cnt)



newendpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearch"
bod1 <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"\n    xmlns:woksearch="http://woksearch.v3.wokmws.thomsonreuters.com">\n    <soapenv:Header/>\n    <soapenv:Body>\n    <woksearch:search>\n    <queryParameters>\n    <databaseId>WOS</databaseId>\n    <userQuery>'
bod2 <- '</userQuery>\n    <timeSpan>\n    <begin>1900-01-01</begin>\n    <end>2050-12-31</end>\n    </timeSpan>\n    <queryLanguage>en</queryLanguage>\n    </queryParameters>\n    <retrieveParameters>\n    <firstRecord>1</firstRecord>\n    <count>100</count>\n    <option>\n    <key>RecordIDs</key>\n    <value>On</value>\n    </option>\n    <option>\n    <key>targetNamespace</key>\n    <value>http://scientific.thomsonreuters.com/schema/wok5.4/public/FullRecord</value>\n    </option>    </retrieveParameters>\n    </woksearch:search>\n    </soapenv:Body>\n    </soapenv:Envelope>'
query <- "AU=Koehl Mimi"

body <- paste0(bod1, query, bod2)

response <- POST(endpoint, body = body, add_headers(cookie=paste0("SID=", SID)))
print(response$status_code)




body1 <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" 
   xmlns:woksearch="http://woksearch.v3.wokmws.thomsonreuters.com">
<soapenv:Header/>
<soapenv:Body>
<woksearch:search>
<queryParameters>
<databaseId>WOS</databaseId>   
<userQuery>TS=(cadmium OR lead)</userQuery>
<editions>
<collection>WOS</collection>
<edition>SCI</edition>
</editions>           
<timeSpan>
<begin>2000-01-01</begin>
<end>2011-12-31</end>
</timeSpan>        
<queryLanguage>en</queryLanguage>
</queryParameters>
<retrieveParameters>
<firstRecord>1</firstRecord>
<count>5</count>
<option>
<key>RecordIDs</key>
<value>On</value>
</option>
<option>            
<key>targetNamespace</key>
<value>http://scientific.thomsonreuters.com/schema/wok5.4/public/FullRecord</value>
</option>            
</retrieveParameters>
</woksearch:search>
</soapenv:Body>
</soapenv:Envelope>'
newendpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearch"

response <- POST(newendpoint, body = body1, add_headers(cookie=paste0("SID=", SID)))
response$status_code




body2 <- '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" 
   xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">
<soapenv:Header/>
<soapenv:Body>
<woksearchlite:search>
<queryParameters>
<databaseId>WOS</databaseId>   
<userQuery>TS=(cadmium OR lead)</userQuery>
<editions>
<collection>WOS</collection>
<edition>SCI</edition>
</editions>           
<timeSpan>
<begin>2000-01-01</begin>
<end>2011-12-31</end>
</timeSpan>        
<queryLanguage>en</queryLanguage>
</queryParameters>
<retrieveParameters>
<firstRecord>1</firstRecord>
<count>5</count>
</retrieveParameters>
</woksearchlite:search>
</soapenv:Body>
</soapenv:Envelope>'
endpoint <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite"

response <- POST(endpoint, body = body2, add_headers(cookie=paste0("SID=", SID)))
response$status_code
