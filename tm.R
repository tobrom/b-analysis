
text <- c("Brödig, fruktig smak med inslag av citrus, aprikos, kryddor och banan. Jästfällning. Serveras vid 6-8°C som sällskapsdryck eller till rätter av fisk och ljust kött, gärna med heta och asiatiska inslag", 
          "Fruktig smak med inslag av banan, aprikos, vetebröd, kryddor och citrus. Jästfällning. Serveras vid 10-11°C som sällskapsdryck eller till rätter av fisk och ljust kött, gärna med heta och asiatiska inslag.",
          "aprikos, vetebröd, kryddor och citrus. Jästfällning. Serveras")

bagWords <- Corpus(VectorSource(text)) %>%
  #tm_map(content_transformer(tolower)) %>%                            
  #tm_map(removePunctuation) %>%                                         
  #tm_map(removeNumbers) %>%
  #tm_map(removeWords, stopwords("sv")) %>%
  TermDocumentMatrix() %>%
  as.matrix()
bagWords  
  
#1
as.matrix(TermDocumentMatrix(bagWords, 
                             control = list(minWordLength = 1)))
#2
as.matrix(TermDocumentMatrix(bagWords, 
                             control = list(weighting = weightTfIdf)))


removeSparseTerms(1-1/nrow(ipoData)) 


###extract temperture


tempList <- str_locate_all(text, "[:digit:]")

tempIndex <- data.frame(start = map_dbl(tempList, function(x) {if (!nrow(x)) {NA} else { min(x) } }),
                          end = map_dbl(tempList, function(x) {if (!nrow(x)) {NA} else { max(x) } }))



start = map(tempList, function(x) {if (!nrow(x)) {NA} else { min(x) } })

temps <- map2(text[1:2], tempIndex[1:2,], ~str_sub(.x, .y$start, .y$end))





