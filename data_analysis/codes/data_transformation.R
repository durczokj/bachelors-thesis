#### wczytanie pakietów ####
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(readxl)
library(randomForest)
library(MASS)
install.packages("smbinning")
library(smbinning)
install.packages("xlsx")
library(xlsx)
install.packages("xlsx")
library(openxlsx)

##### wczytanie danych ####
rm(list = ls())
dane <- read.csv("/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/dane_oryginalne.csv", header=TRUE, sep=",")
head(dane)
dim(dane)
sum(apply(dane,1,anyNA))
apply(dane,2,anyNA)
summary(dane)
table(factor(dane$salary))

# zamiana zmiennej objaśnianej na 0-1
dane$salary <- ifelse(dane$salary == " >50K", as.integer(1), as.integer(0))

#### Usunięcie niepotrzebnych spacji ####
dane <- as.data.frame(lapply(dane, trimws))

#### Zmiana typu danych na numeryczne ####
zamiana_na_numeryczne <- function(df, zmienna) {
  if(!zmienna %in% colnames(df)) {
    stop(paste0("Column '", zmienna, "' does not exist in the dataframe."))
  }
  if(any(is.na(as.numeric(df[[zmienna]]))) == F){
    df[[zmienna]] <- as.numeric(df[[zmienna]])
  }
  df
}

for(i in 1:ncol(dane)){
  zmienna <- colnames(dane)[i]
  dane <- zamiana_na_numeryczne(dane,zmienna)
}

#### Transformacja zmiennej age ####
result <- smbinning(dane, "salary", "age", 0.05)
result$bands
result$ctree

age_bands <- c(result$bands)

dane$age <- cut(dane$age, breaks = age_bands, include.lowest= T)

#### Transformacja zmiennej country ####
dane$country <- ifelse(dane$country == "United-States", "US", "Non-US")

#### Transformacja zmiennej education ####
# <=12th
dane$education <- 
ifelse(
(dane$education == "Preschool" | 
dane$education == "1st-4th" |
dane$education == "5th-6th" |
dane$education == "7th-8th" |
dane$education == "9th" |
dane$education == "10th" |
dane$education == "11th" |
dane$education == "12th"),
"<= 12th", dane$education)

# High School
dane$education <- ifelse(dane$education == "HS-grad", "High School", dane$education)

# Associate
dane$education <- ifelse(
    dane$education == "Assoc-acdm" | 
    dane$education == "Assoc-voc", 
    "Associate", dane$education)

# Above Masters
dane$education <- ifelse(
  dane$education == "Prof-school" | 
    dane$education == "Doctorate", 
  "Above Masters", dane$education)

#### Transformacja zmiennej education.num ####
#stworzenie kolumny pomocniczej educationnum
dane$educationnum <- dane$education.num
#tworzenie zakresów podziału
result_educationnum <- smbinning(dane, "salary", "educationnum", 0.05)
result_educationnum$bands
result_educationnum$ctree
#tworzenie wektora podziału
educationnum_bands <- c(result_educationnum$bands)
#tworzenie podziału
dane$education.num <- cut(dane$educationnum, breaks = educationnum_bands, include.lowest=T)
#usuwanie zmiennej pomocniczej (której kolumna ma numer 16)
dane <- dane[-16]

#### Transformacja zmiennej hours.per.week (smbinning - niewykorzystywany)####
## stworzenie kolumny pomocniczej hoursperweek
## dane$hoursperweek <- dane$hours.per.week
#tworzenie zakresów podziału
## result_hoursperweek <- smbinning(dane, "salary", "hoursperweek", 0.05)
## result_hoursperweek$bands
## result_hoursperweek$ctree
#tworzenie wektora podziału
## hoursperweek_bands <- c(result_hoursperweek$bands)
#tworzenie podziału
## dane$hours.per.week <- cut(dane$hoursperweek, breaks = hoursperweek_bands,include.lowest=T)
#usuwanie zmiennej pomocniczej (której kolumna ma numer 16)
## dane <- dane[-16]

#### Transformacja zmiennej hours.per.week (manualna)####
dane$hours.per.week <- 
  ifelse(dane$hours.per.week < 40,
    "<40",
    ifelse(dane$hours.per.week > 40,
      ">40","40")
    )
dane$hours.per.week <- factor(dane$hours.per.week, levels = c("<40","40",">40"))

#### Transformacja zmiennej marital.status ####
dane$marital.status <- ifelse(
  dane$marital.status == "Married-AF-spouse" | 
    dane$marital.status == "Married-civ-spouse", 
  "With Spouse", "Spouseless")

#### Transformacja zmiennej race ####
dane$race <- ifelse(
  dane$race == "Other" | 
  dane$race == "Amer-Indian-Eskimo"|
  dane$race == "Black", 
  "Other", "White or Asian-Pac-Islander")

#### Transformacja zmiennej relationship ####

# Child or other relative
dane$relationship <- ifelse(
  dane$relationship == "Own-child" | 
    dane$relationship == "Other-relative",
  "Child or other relative", dane$relationship)

# Wife or husband
dane$relationship <- ifelse(
  dane$relationship == "Husband" | 
    dane$relationship == "Wife",
  "Wife or husband", dane$relationship)

#### Transformacja zmiennej workclass ####
dane$workclass <- ifelse(
  dane$workclass == "Never-worked" | 
    dane$workclass == "Without-pay"| 
    dane$workclass == "?",
  "Other", dane$workclass)

#### Transformacja zmiennej occupation ####

## service
dane$occupation <- ifelse(
  dane$occupation == "Priv-house-serv" | 
    dane$occupation == "Other-service"| 
    dane$occupation == "Handlers-cleaners",
  "Service", dane$occupation)

## other
dane$occupation <- ifelse(
  dane$occupation == "?" | 
  dane$occupation == "Armed-Forces",
  "Other", dane$occupation)

## Agriculture
dane$occupation <- ifelse(
  dane$occupation == "Farming-fishing",
  "Agriculture", dane$occupation)

## Administration
dane$occupation <- ifelse(
  dane$occupation == "Adm-clerical",
  "Administration", dane$occupation)

## Tech and Security
dane$occupation <- ifelse(
  dane$occupation == "Tech-support"|
  dane$occupation == "Protective-serv",
  "Tech and Secutity", dane$occupation)

#### Usunięcie zmiennej capital.gain ####
dane <- dane %>% dplyr::select(-capital.gain)

#### Usunięcie zmiennej capital.loss ####
dane <- dane %>% dplyr::select(-capital.loss)

#### Zapisanie danych ####

path <- "/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/dane po transformacji.xlsx"

write.xlsx(dane, path, sheetName = "Sheet1")
save(dane,file="/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/dane po transformacji.Rda")


