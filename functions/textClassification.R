createSampleList <- function(data, n){
  sampleList <- lapply(seq_len(n), function(x){
    data <- data[sample(nrow(data), nrow(data)),]
  })
  
  sampleList
}

createTermDocumentMatrixByGender  <- function(data, stem, stopw, lang, gend){
  
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
  tdm 
  
}

createGobalTermDocumentMatrix <- function(data, stem, stopw, lang){
  
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

createDataTest <- function(sampleList, stopw, stem, weight, language){
  
  dtmList <- list()
  containerList <- list()
  size <- length(sampleList)
  
  for (i in 1:size){
    
    m <- sampleList[[i]] 
    
    corpus <- VCorpus(VectorSource(m[,1]), readerControl = list(language = language))
    
    matrix <- DocumentTermMatrix(
      corpus, 
      list(toLower=TRUE, 
           removePunctuation = TRUE, 
           stopwords = stopw, 
           stemming = stem, 
           removeNumbers = TRUE, 
           weighting=weight))
    
    dtmList[[i]] <- matrix
    
    total_size <- dim(m)[1]
    train_size <- round(total_size) * 0.7
    
    container <- create_container(
      matrix, 
      m[,3],
      trainSize = 1:train_size, 
      testSize = (train_size + 1):total_size,
      virgin = FALSE)
    
    containerList[[i]] <- container
  }
  

  list("dtmList" = dtmList, "containerList" = containerList)
}

textClassification <- function(method, containerList){
  classifDF <- c()

  size <- length(containerList)
  for (i in 1:size){
    container <- containerList[[i]]
    if (method == "SVM"){
      train <- train_model(container,
                           method,
                           kernel = "linear",
                           method = "C-classification", 
                           cost = 10^2, 
                           cross = 0)
    } else {
      train <- train_model(container, method)
    }
    
    
    classify <- classify_model(container, train)
    analytics <- create_analytics(container, classify)
    
    alg_summary <- analytics@algorithm_summary
    
    
    aux <- data.frame(
      "precision" = alg_summary[,1],
      "recall" = alg_summary[,2],
      "fscore" = alg_summary[,3],
      "method" = method, 
      "gender" = c("female", "male"), 
      "sample" = i)
    
    classifDF <- rbind(classifDF, aux)
    
  }
  
  
  return (classifDF)
}