# For running this script it is necessary:
# - R version 3.6.x
# - Rtools version 3.5 (https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe)
# - RStem version 0.4-1 (https://cran.r-project.org/src/contrib/Archive/Rstem/)
# - maxent version 1.3.3 (https://cran.r-project.org/src/contrib/Archive/maxent/)
# - RTextTools version 1.4.2 (https://cran.r-project.org/src/contrib/Archive/RTextTools/)

library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(RTextTools)
library(SnowballC)
library(stringr)
library(tm)
library(wordcloud)

# Downloaded from https://figshare.com/articles/dataset/Biographies_of_literature_writers/13551467
load("datasets/biographies_EN.RData")

# Downloaded from  https://figshare.com/articles/dataset/biographies_RData/13551437
load("datasets/biographies_ES.RData")

source("functions/textClassification.R", encoding = "UTF-8")
source("functions/exploratoryAnalysis.R", encoding = "UTF-8")
source("functions/plots.R")

# To retrieve the biographies from wikipedia, uncomment the following trhee lines
# The category "Escritores_de_España_del_siglo_XX" contains both male and female biographies so 
# the female biographies must be manually removed

#source("functions/scrappy.R")
#biographies_EN <- createDatasetFromWikipedia("19th-century_women_writers", "19th-century_male_writers", "en")
#biographies_ES <- createDatasetFromWikipedia("Escritoras_de_España", "Escritores_de_España_del_siglo_XX", "es")


## Exploratory Analysis

## Average number of distinct terms per biography of female writers (Language: English)
termsFemale_EN <- obtainMeanOfTermsByBiography(biographies_EN, FALSE, TRUE, "english", "female")
termsFemale_EN

## Average number of distinct terms per biography of female writers appying stemming (Language: English)
termsFemaleStem_EN <- obtainMeanOfTermsByBiography(biographies_EN, TRUE, TRUE, "english", "female")
termsFemaleStem_EN

## Average number of distinct terms per biography of male writers (Language: English)
termsMale_EN <- obtainMeanOfTermsByBiography(biographies_EN, FALSE, TRUE, "english", "male")
termsMale_EN

## Average number of distinct terms per biography of male writers appying stemming (Language: English)
termsFemaleStem_EN <- obtainMeanOfTermsByBiography(biographies_EN, TRUE, TRUE, "english", "male")
termsFemaleStem_EN

## Average number of distinct terms per biography of female writers (Language: Spanish)
termsFemale_ES <- obtainMeanOfTermsByBiography(biographies_ES, FALSE, TRUE, "spanish", "female")
termsFemale_ES

## Average number of distinct terms per biography of female writers appying stemming (Language: Spanish)
termsFemaleStem_ES <- obtainMeanOfTermsByBiography(biographies_ES, TRUE, TRUE, "spanish", "female")
termsFemaleStem_ES

## Average number of distinct terms per biography of male writers (Language: Spanish)
termsMale_ES <- obtainMeanOfTermsByBiography(biographies_ES, FALSE, TRUE, "spanish", "male")
termsMale_ES

## Average number of distinct terms per biography of male writers appying stemming (Language: Spanish)
termsFemaleStem_ES <- obtainMeanOfTermsByBiography(biographies_ES, TRUE, TRUE, "spanish", "male")
termsFemaleStem_ES


## Creation of comparison and commonality wordclouds for the biographies written in English
tdm <- createGlobalTermDocumentMatrix(biographies_EN, FALSE, TRUE, "english")

comparison.cloud(tdm, random.order=FALSE, colors = c("indianred3","lightsteelblue3"),
                 title.size=2.5, max.words=300)

commonality.cloud(tdm, random.order = FALSE, colors = brewer.pal(4, "Dark2"), max.words = 70)


## Classification Experiments

## Experiment 1
## Stemming: FALSE
## Stopwords: TRUE
## Weighting: TF-IDF
## Language: English
sampleList <- createSampleList(biographies_EN, 10) 

ldt <- createDataTest(sampleList, stopw = TRUE, stem = FALSE, weight = tm::weightTfIdf,language = "english")

containerList <- ldt$containerList

svmClassify <- textClassification("SVM", containerList)

maxentClassify <- textClassification("MAXENT", containerList)

boostClassify <- textClassification("BOOSTING", containerList)

rfClassify <- textClassification("RF", containerList)

totalClassify_EN_1 <- rbind(svmClassify, maxentClassify, boostClassify, rfClassify) %>%
  mutate(stopwords = TRUE, stemming = FALSE, language = "English", weighting = "tf-idf")

## Experiment 2
## Stemming: TRUE
## Stopwords: TRUE
## Weighting: TF-IDF
## Language: English
ldt <- createDataTest(sampleList, stopw = TRUE, stem = TRUE, weight = tm::weightTfIdf,language = "english")

containerList <- ldt$containerList

svmClassify <- textClassification("SVM", containerList)

maxentClassify <- textClassification("MAXENT", containerList)

boostClassify <- textClassification("BOOSTING", containerList)

rfClassify <- textClassification("RF", containerList)

totalClassify_EN_2 <- rbind(svmClassify, maxentClassify, boostClassify, rfClassify) %>%
  mutate(stopwords = TRUE, stemming = TRUE, language = "English", weighting = "tf-idf")


## Experiment 3
## Stemming: FALSE
## Stopwords: TRUE
## Weighting: TF-IDF
## Language: Spanish
sampleList <- createSampleList(biographies_ES, 10) 

ldt <- createDataTest(sampleList, stopw = TRUE, stem = FALSE, weight = tm::weightTfIdf,language = "spanish")

containerList <- ldt$containerList

svmClassify <- textClassification("SVM", containerList)

maxentClassify <- textClassification("MAXENT", containerList)

boostClassify <- textClassification("BOOSTING", containerList)

rfClassify <- textClassification("RF", containerList)

totalClassify_ES_1 <- rbind(svmClassify, maxentClassify, boostClassify, rfClassify) %>%
  mutate(stopwords = TRUE, stemming = FALSE, language = "Spanish", weighting = "tf-idf")

## Experiment 4
## Stemming: TRUE
## Stopwords: TRUE
## Weighting: TF-IDF
## Language: Spanish
ldt <- createDataTest(sampleList, stopw = TRUE, stem = TRUE, weight = tm::weightTfIdf,language = "spanish")

containerList <- ldt$containerList

svmClassify <- textClassification("SVM", containerList)

maxentClassify <- textClassification("MAXENT", containerList)

boostClassify <- textClassification("BOOSTING", containerList)

rfClassify <- textClassification("RF", containerList)

totalClassify_ES_2 <- rbind(svmClassify, maxentClassify, boostClassify, rfClassify) %>%
  mutate(stopwords = TRUE, stemming = TRUE, language = "Spanish", weighting = "tf-idf")

totalClassify <- rbind(totalClassify_EN_1, totalClassify_EN_2, totalClassify_ES_1, totalClassify_ES_2)


## F1 Score comparison plot
plot <- f1ScoreComparisonPlot(totalClassify)
plot
