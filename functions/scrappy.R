library(WikipediR)
library(rvest)

## Remove HTNL tags from text
parseToText <- function(url, pageTitle){
  contentHTML <- read_html(paste0(url,pageTitle))
  
  pList <- contentHTML %>% 
    html_nodes("p") %>%
    html_text() 
  
  contentText <- paste( unlist(pList), collapse='')
  contentText
  
}


createDatasetFromWikipedia <- function(femaleCategory, maleCategory, language){
  femaleBios <- retrieveBiographiesOfCategory(femaleCategory, language, "female")
  maleBios <- retrieveBiographiesOfCategory(maleCategory, language, "male")
  
  numOfFemaleBios <- dim(femaleBios)[1]  
  numOfMaleBios <- dim(maleBios)[1]
  
  # To create a banlanced dataset
  if (numOfFemaleBios > numOfMaleBios){
    femaleBios <- femaleBios[1:numOfMaleBios, ]
  } else if (numOfMaleBios > numOfFemaleBios){
    maleBios <- maleBios[1:numOfFemaleBios, ]
    
  }
  
  data <- rbind(femaleBios, maleBios)  %>% arrange(id)
  
  data
}


retrieveBiographiesOfCategory <- function(category, language, gender){
  
  if (language == "en"){
    url <- "https://en.wikipedia.org/wiki/"
  } else {
    url <- "https://es.wikipedia.org/wiki/"
  }
  pagesByCategory <- pages_in_category(
    language, 
    "wikipedia", 
    categories = category, 
    properties = c("title"), 
    type = c("page"),
    limit = 5000)
  
  pagesByCategory.query <- pagesByCategory$query
  
  authorNameList <- lapply(pagesByCategory.query$categorymembers, function(x) x$title)

  authorBiographyList <- lapply(authorNameList, function(x){
    parseToText(url, str_replace_all(x, " ", "_"))})
  
  numberOfBio <- length(authorBiographyList)
  
  if (gender == "female"){
    idSeq <- seq(2, 2*numberOfBio, 2)
    cat = 0
  } else {
    idSeq <- seq(1, 2*numberOfBio, 2)
    cat = 1
  }
  
  biography <- data.frame(
    "biography" = matrix(unlist(authorBiographyList), nrow=numberOfBio, byrow=T), 
    "id"= idSeq,
    "category" = cat, 
    "gender" = gender,
    stringsAsFactors = FALSE)

  biography
}