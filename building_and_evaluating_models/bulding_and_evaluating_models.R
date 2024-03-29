#### Przed rozpoczęciem ####
# instalacja pakietów
devtools::install_github("selva86/InformationValue")

# załadowanie wszystkich paczek
list_of_packages <- c("readr"
                      ,"dplyr"
                      ,"rpart"
                      ,"rpart.plot"
                      ,"randomForest"
                      ,"ROCR"
                      ,"MASS"
                      ,"ggplot2"
                      ,"data.table"
                      ,"caret"
                      ,"readxl"
                      ,"InformationValue"
                      ,"caTools"
                      ,"car"
                      ,"jtools"
                      ,"broom.mixed"
                      ,"reshape2"
)

not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)
lapply(list_of_packages, library, character = TRUE)

#### Wczytanie danych ####
load("/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/dane po transformacji.Rda")

dane$salary = as.factor(dane$salary)

#### Podział zbioru ####
set.seed(107950)
# podział na zbiór testowy i treningowy (dostosować train.ratio)
train.ratio <- 0.70
# Indeks dla danych treningowych
index.train <- runif(nrow(dane),0,1) <= train.ratio
# Podzbiór danych treningowych
dane.train <- dane[index.train,]
# Podzbiór danych testowych
dane.test <- dane[!index.train,]


#### Drzewa decyzyjne ####

#### Drzewo decyzyjne "tree" ####
tree <- rpart(salary ~ age + workclass + education + education.num + marital.status + occupation + relationship + race + sex+ hours.per.week + country,
              data = dane.train,
              method = "class")

tree$variable.importance

rpart.plot(tree)

#### Drzewo decyzyjne "tree_weights" ####
tree_weights <- rpart(salary ~ age + workclass + education + education.num + marital.status + occupation + relationship + race + sex+ hours.per.week + country,
                data = dane.train,
                weights = fnlwgt,
                method = "class")

tree_weights$variable.importance
rpart.plot(tree_weights)

#### Drzewo decyzyjne "tree_przyciete" ####
tree_przyciete <- rpart(salary ~ age + workclass + education + education.num + marital.status + occupation + relationship + race + sex+ hours.per.week + country,
                        data = dane.train,
                        method = "class",
                        control = rpart.control(minbucket = 500))

rpart.plot(tree_przyciete)

#### Drzewo decyzyjne "tree_lim_depth" ####
tree_lim_depth <- rpart(salary ~ age + workclass + education + education.num + marital.status + occupation + relationship + race + sex+ hours.per.week + country,
                        data = dane.train,
                        method = "class",
                        control = rpart.control(maxdepth = 2))
#### Modele regresji ####
rpart.plot(tree_lim_depth)

#### Tabela podsumowująca regresję ####
tabela <- function(model_regresji){
  
  df <- summary(model_regresji)$coefficients
  
  pvalue <- as.data.frame(round(df[,4], 2))
  
  df <- cbind(df, pvalue)
  
  nazwy <- as.data.frame(rownames(df))
  
  df <- cbind(nazwy, df[1], df[5])
  
  colnames(df) <- c("Zmienna", "Współczynnik", "p-value")
  
  rownames(df) <- NULL
  
  return(df)
}

#### Model regresja_full ####
regresja_full <- glm(salary ~ . -education.num - fnlwgt,
                data = dane.train,
                family = binomial)

summary(regresja_full)
summ(regresja_full)
tabela(regresja_full)

#### Model regresja_step ####                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
regresja_step <- regresja_full %>%
  stepAIC()

exp(coef(regresja_full))
exp(coef(regresja_step))

tabela(regresja_step)

#### Lasy losowe ####

#### Wybór właściwej kombinacji ####
lasy <-  list()

for (i in 1:5){
  lasy[[i]] <- randomForest(salary ~. - fnlwgt,
                      data = dane.train,
                      ntree=300, mtry=i)
  abc <- as.vector(predict(lasy[[i]], newdata = dane.test, type = "prob")[, 2])
  AUC <-(performance(prediction(abc, dane.test$salary), "auc")@y.values[[1]])
  cat("AUC dla lasu ", i,": ", AUC,"\n", sep="")
}

#### Ostatecznie wybrany las ####
las_losowy <- randomForest(salary ~. - fnlwgt,
                           data = dane.train,
                           ntree=300, mtry=1)
abc <- as.vector(predict(las_losowy, newdata = dane.test, type = "prob")[, 2])
AUC <-(performance(prediction(abc, dane.test$salary), "auc")@y.values[[1]])
cat("AUC dla lasu ", 1,": ", AUC,"\n", sep="")

plot(las_losowy)

varImpPlot(las_losowy)

#### Ocena modeli ####

# Lista modeli do oceny
CM <- list()
CM[["regresja_full"]] <- table(ifelse(predict(regresja_full, new = dane.test, type = "response") > 0.5, 1, 0), dane.test$salary)
CM[["regresja_step"]] <- table(ifelse(predict(regresja_step, new = dane.test, type = "response") > 0.5, 1, 0), dane.test$salary)
CM[["tree"]] <- table(predict(tree, new = dane.test, type = "class"), dane.test$salary)
CM[["tree_weights"]] <- table(predict(tree_weights, new = dane.test, type = "class"), dane.test$salary)
CM[["tree_przyciete"]] <- table(predict(tree_przyciete, new = dane.test, type = "class"), dane.test$salary)
CM[["tree_lim_depth"]] <- table(predict(tree_lim_depth, new = dane.test, type = "class"), dane.test$salary)
CM[["las_losowy"]] <- table(predict(las_losowy, new = dane.test, type = "class"), dane.test$salary)

#### Funkcja oceny modeli #####
EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy # Misclassification Error Rate
  # inaczej: MER < - (false_positive + false_negative) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

sapply(CM, EvaluateModel)

#### Krzywe ROC ####
preds <- list()
preds[["regresja_full"]] <- as.vector(predict(regresja_full, newdata = dane.test, type = "response"))
preds[["regresja_step"]] <- as.vector(predict(regresja_step, newdata = dane.test, type = "response"))
preds[["tree"]] <- as.vector(predict(tree, newdata = dane.test)[, 2])
preds[["tree_weights"]] <- as.vector(predict(tree_weights, newdata = dane.test)[, 2])
preds[["tree_przyciete"]] <- as.vector(predict(tree_przyciete, newdata = dane.test)[, 2])
preds[["tree_lim_depth"]] <- as.vector(predict(tree_lim_depth, newdata = dane.test)[, 2])
preds[["las_losowy"]] <- as.vector(predict(las_losowy, newdata = dane.test, type = "prob")[, 2])

plot(performance(prediction(preds[["tree"]], dane.test$salary), "tpr", "fpr")) 
plot(performance(prediction(preds[["tree_weights"]], dane.test$salary), "tpr", "fpr")) 
plot(performance(prediction(preds[["tree_przyciete"]], dane.test$salary), "tpr", "fpr"))
plot(performance(prediction(preds[["tree_lim_depth"]], dane.test$salary), "tpr", "fpr"))
plot(performance(prediction(preds[["regresja_full"]], dane.test$salary), "tpr", "fpr"))
plot(performance(prediction(preds[["regresja_step"]], dane.test$salary), "tpr", "fpr"))
plot(performance(prediction(preds[["las_losowy"]], dane.test$salary), "tpr", "fpr"))

#### Funkcja: krzywe rok na jednym wykresie ####
RocCurves <- function(models_vector,predicted_variable_column){
  
  # Create an empty data frame with the required column names to store the results
  przewidywania_tabela_appended <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(przewidywania_tabela_appended) <- c("fp", "tp", "tn", "fn", "tpr", "fpr", "Model")
  
  # Loop through each model in the vector
  for(i in 1:length(models_vector)){
    # Get the predicted values for the current model
    
    przewidywania <- prediction(preds[[models_vector[i]]], predicted_variable_column)
    
    # Create a data frame with the true positive, false positive, true negative, and false negative rates
    przewidywania_tabela <- data.frame(
      "fp" = przewidywania@fp[[1]],
      "tp" = przewidywania@tp[[1]],
      "tn" = przewidywania@tn[[1]],
      "fn" = przewidywania@fn[[1]])
    
    # Calculate the true positive rate and false positive rate
    przewidywania_tabela$tpr <- przewidywania_tabela$tp/(przewidywania_tabela$tp+przewidywania_tabela$fn)
    przewidywania_tabela$fpr <- przewidywania_tabela$fp/(przewidywania_tabela$fp+przewidywania_tabela$tn)
    
    # Add the model name to the data frame
    przewidywania_tabela$Model <- rep(models_vector[i],nrow(przewidywania_tabela))
    
    # Append the current model's results to the overall results data frame
    przewidywania_tabela_appended <- rbind(przewidywania_tabela_appended, przewidywania_tabela)
  }
  
  # Create a ROC curve plot using ggplot2
  plot <- 
    ggplot(przewidywania_tabela_appended, aes(x = fpr, y = tpr, col = Model)) +
    geom_line() +
    labs(x = "False positive rate", y = "True positive rate") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, color = "black"),
          legend.text = element_text(size = 12, color = "black"),
          legend.title = element_text(size = 12, color = "black"),
          legend.position = c(0.95, 0.05),
          legend.justification = c(1, 0),
          legend.box.background = element_rect(color = "black", fill = NA),
          plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm") ) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    geom_vline(xintercept = seq(0, 1, by = 0.2), color = "lightgray", linetype = "dotted", size = 0.3) +
    geom_hline(yintercept = seq(0, 1, by = 0.2), color = "lightgray", linetype = "dotted", size = 0.3)
  
  # Return the plot 
  return(plot)
}

# Stworzenie wektora modeli do oceny
wektor_modeli_do_oceny <- c("tree", "tree_lim_depth", "regresja_full", "regresja_step", "las_losowy")

# Wywołanie funkcji
RocCurves(wektor_modeli_do_oceny, dane.test$salary)


#### Area Under Curve ####
(performance(prediction(preds[["tree"]], dane.test$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_weights"]], dane.test$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_przyciete"]], dane.test$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_lim_depth"]], dane.test$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["regresja_full"]], dane.test$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["regresja_step"]], dane.test$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["las_losowy"]], dane.test$salary), "auc")@y.values[[1]])

#### Krzywe Lift ####
performance <- list()

performance[["tree"]] <- performance(prediction(preds[["tree"]], dane.test$salary), "lift", "rpp")
performance[["tree_lim_depth"]] <- performance(prediction(preds[["tree_lim_depth"]], dane.test$salary), "lift", "rpp")
performance[["regresja_full"]] <- performance(prediction(preds[["regresja_full"]], dane.test$salary), "lift", "rpp")
performance[["regresja_step"]] <- performance(prediction(preds[["regresja_step"]], dane.test$salary), "lift", "rpp")
performance[["las_losowy"]] <- performance(prediction(preds[["las_losowy"]], dane.test$salary), "lift", "rpp")

plot(0, 0, type="n", xlim=c(0, 1), ylim=c(1, 4), xlab="Odsetek predykcji pozytywnych", ylab="Lift", xaxs="i", yaxs="i")

abline(v=seq(0, 1, by=0.1), lty=3, col="lightgray")
abline(h=seq(1, 4, by=0.5), lty=3, col="lightgray")

lines(as.numeric(performance[["tree"]]@x.values[[1]]), as.numeric(performance[["tree"]]@y.values[[1]]), col = "#00B0F6")
lines(as.numeric(performance[["tree_lim_depth"]]@x.values[[1]]), as.numeric(performance[["tree_lim_depth"]]@y.values[[1]]), col = "#E76BF3")
lines(as.numeric(performance[["regresja_full"]]@x.values[[1]]), as.numeric(performance[["regresja_full"]]@y.values[[1]]), col = "#A3A500")
lines(as.numeric(performance[["regresja_step"]]@x.values[[1]]), as.numeric(performance[["regresja_step"]]@y.values[[1]]), col = "#00BF7D")
lines(as.numeric(performance[["las_losowy"]]@x.values[[1]]), as.numeric(performance[["las_losowy"]]@y.values[[1]]), col = "#F8766D")

legend("topright", 
       legend=c("tree", "tree_lim_depth", "regresja_full", "regresja_step", "las_losowy"), 
       col=c("#00B0F6", "#E76BF3", "#A3A500", "#00BF7D", "#F8766D"), lty=c(1), lwd=c(2))

#### Sprawdzenie na danych treningowych #### 
preds <- list()

preds[["regresja_full"]] <- as.vector(predict(regresja_full, newdata = dane.train, type = "response"))
preds[["regresja_step"]] <- as.vector(predict(regresja_step, newdata = dane.train, type = "response"))
### Drzewa
preds[["tree"]] <- as.vector(predict(tree, newdata = dane.train)[, 2])
preds[["tree_weights"]] <- as.vector(predict(tree_weights, newdata = dane.train)[, 2])
preds[["tree_przyciete"]] <- as.vector(predict(tree_przyciete, newdata = dane.train)[, 2])
preds[["tree_lim_depth"]] <- as.vector(predict(tree_lim_depth, newdata = dane.train)[, 2])
### Las
preds[["las_losowy"]] <- as.vector(predict(las_losowy, newdata = dane.train, type = "prob")[, 2])

(performance(prediction(preds[["tree"]], dane.train$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_weights"]], dane.train$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_przyciete"]], dane.train$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_lim_depth"]], dane.train$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["regresja_full"]], dane.train$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["regresja_step"]], dane.train$salary), "auc")@y.values[[1]])
(performance(prediction(preds[["las_losowy"]], dane.train$salary), "auc")@y.values[[1]])



