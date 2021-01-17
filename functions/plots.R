f1ScoreComparisonPlot <- function(classificationResults){
  
  classificationResults <- classificationResults %>% 
    mutate(label = ifelse(stemming,   paste(language, "(stemming)") , paste(language, "(no stemming)")))
  
  row.labs = c("Female", "Male")
  names(row.labs) = c("female", "male")
  
  globalBoxplot <- ggplot(classificationResults, aes(x=method, y=fscore, fill=label)) +
    geom_boxplot(width = 0.8) +
    scale_x_discrete(labels=c("SVM", "MaxEnt", "Boosting", "RF")) +
    scale_fill_manual(values=c("#56B4E9", "#226F99", "#E6FB6C", "#2F810A")) +
    ylab(expression(F[1]~score)) +
    xlab(" ") +
    facet_grid(~gender, labeller = labeller(gender = row.labs)) +
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          strip.text.x = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 11)) +
    theme_hc()
  
  globalBoxplot 
  
}
