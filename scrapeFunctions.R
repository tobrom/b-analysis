
library(rvest)
library(stringr)

scrapeProduct <- function(url) {

rawText <- read_html(url)

webName <- html_nodes(rawText, "head") %>% 
           html_text()

webDescription <- html_nodes(rawText, "p.description") %>% 
                  html_text()

tasteClock <- html_nodes(rawText, "span.cmp-screen-reader-text") %>% 
              html_text() %>% 
              data.frame() %>%
              slice(2:4) %>%
              t() %>%
              data.frame()

df <- data.frame(name = as.character(webName),
           description = as.character(webDescription),
           tasteBeska = as.numeric(str_trim(str_sub(tasteClock[,1], -2, -1))),
           tasteFyllighet = as.numeric(str_trim(str_sub(tasteClock[,2], -2, -1))),
           tasteSotma = as.numeric(str_trim(str_sub(tasteClock[,3], -2, -1))))

return(df)

}