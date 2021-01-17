createGlobalTermDocumentMatrix <- function(data, stem, stopw, lang){
  
  contentByGender <- data %>% 
    group_by(category) %>% 
    summarise(text=paste(
      chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", str_replace_all(biography, "[^[:alnum:]]", " ")), 
      collapse=" "))
  
  corpus <- VCorpus(VectorSource(contentByGender$text),  
                    readerControl = list(language = lang))
  
  tdm <- TermDocumentMatrix(
    corpus, 
    list(toLower=TRUE, 
         removePunctuation = TRUE, 
         stopwords = stopw, 
         stemming = stem, 
         removeNumbers = TRUE, 
         weighting=tm::weightTf)) %>%
    as.matrix()
  
  colnames(tdm) <- c("female","male")
  
  
  
  return (tdm) 
}

obtainMeanOfTermsByBiography <- function(data, stem, stopw, lang, gend){
  
  contentByGender <- data %>% 
    filter(gender == gend) 
  
  biographiesNumber <- dim(contentByGender)[1]
  
  contentByGender <- contentByGender %>%
    group_by(gender) %>% 
    summarise(text=paste(
      chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", str_replace_all(biography, "[^[:alnum:]]", " ")),
      collapse=" "))
  
  corpus <- VCorpus(VectorSource(contentByGender$text),  
                    readerControl = list(language = lang))
  
  tdm <- TermDocumentMatrix(
    corpus, 
    list(toLower=TRUE, 
         removePunctuation = TRUE, 
         stopwords = stopw, 
         stemming = stem, 
         removeNumbers = TRUE, 
         weighting=tm::weightTf)) 

  termsNumber <- tdm$nrow
  average <- round(termsNumber / biographiesNumber, 2)
  
  statistics <- data.frame(biographiesNumber, termsNumber, average)
  
  return (statistics) 
}
