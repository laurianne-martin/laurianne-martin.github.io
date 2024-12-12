### Cette démonstration est une adaptation/traduction/mise à jour de
### Hoving, Wesley. (2019). Quantéda. https://www.youtube.com/watch?v=O-2xpszBfdA

install.packages("quanteda")
install.packages("quanteda.textmodels")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
install.packages("stopwords")

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(stopwords)
library(ggplot2)

#Montrer le résumé de chaque document dans le corpus
summary(data_corpus_inaugural)

#Créer un objet avec le résumé de chaque document du corpus
tokeninfo <-summary(data_corpus_inaugural)

#Déterminer le discours avec le plus de phrases
tokeninfo[which.max(tokeninfo$Sentences), ]

#Déterminer le discours avec le moins de phrases
tokeninfo[which.min(tokeninfo$Sentences), ]

#Créer un graphique avec le nombre de phrases, selon l'année du discours
  ggplot(data = tokeninfo, aes(x = Year, y = Sentences, group = 1)) + geom_line() + geom_point() +
  scale_x_continuous(labels = c(seq(1789, 2021, 12)), breaks = seq(1789, 2021, 12)) +
  theme_bw()
 
#Créer un graphique avec le nombre de phrases, selon l'année du discours
  ggplot(data = tokeninfo, aes(x = Year, y = Sentences, group = 1)) + 
    geom_line() + 
    geom_point() + 
    geom_smooth(method = "lm") +
  scale_x_continuous(labels = c(seq(1789, 2021, 12)), breaks = seq(1789, 2021, 12)) +
  theme_bw() 

#Créer un graphique de dispersion lexicale pour le mot "america"
  data_corpus_inaugural_subset <-
    corpus_subset(data_corpus_inaugural, Year > 2000)
  tokens_inaugural <- tokens(data_corpus_inaugural_subset)
  kwic_results <- kwic(tokens_inaugural, pattern = "america") 
  textplot_xray(kwic_results)

#Créer des diagrammes de dispersion lexicale pour comparer entre le mot "america" et "american"  
  textplot_xray(
    kwic(tokens_inaugural, pattern = "america"),
    kwic(tokens_inaugural, pattern = "american")
  )

#Créer des diagrammes de dispersion lexicale à échelle absolue pour les mots "america" et "freedom"
  textplot_xray(
    kwic(tokens_inaugural, pattern = "america"),
    kwic(tokens_inaugural, pattern = "freedom"),
    scale = "absolute"
  )

#Créer un graphique de probabilité d'apparition des mots comparant le discours de Trump à celui de Biden    
  corpus_2017 <- corpus_subset(data_corpus_inaugural, Year == 2017)
  corpus_2021 <- corpus_subset(data_corpus_inaugural, Year == 2021) 
  
  # Combiner les deux sous-ensembles en un seul corpus
  corpus_combined <- c(corpus_2017, corpus_2021)
  
  #Diviser le corpus en tokens (en mots), puis retirer les mots inutiles
  tokens_corpus_combined <- tokens(corpus_combined,
                                   remove_punct = TRUE, 
                                   remove_symbols = TRUE,
                                   remove_numbers = TRUE)
  
  tokens_corpus_combined_no_stopwords <- tokens_remove(tokens_corpus_combined, stopwords("en"))
 
   # Créer un DFM (document frame matrice) à partir des tokens
  dfm_combined <- dfm(tokens_corpus_combined_no_stopwords)
  
  #Afficher le DFM
  print(dfm_combined)
  
  #Effectuer le calcul
  keyness_results <- textstat_keyness(dfm_combined)

  #Faire une visualisation des résultats
  textplot_keyness(keyness_results)  
  