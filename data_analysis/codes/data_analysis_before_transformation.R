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
devtools::install_github("selva86/InformationValue")
library(InformationValue)
library(randomForest)
library(MASS)
install.packages("xlsx")
library(xlsx)
install.packages("xlsx")
library(openxlsx)


#### Wczytanie i uporządkowanie danych ####
dane <- read.csv("/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/dane_oryginalne.csv", header=TRUE, sep=",")
head(dane)
dim(dane)
sum(apply(dane,1,anyNA))
apply(dane,2,anyNA)
summary(dane)
table(factor(dane$salary))

# zamiana zmiennej objaśnianej na 0-1
dane$salary <- ifelse(dane$salary == " >50K", 1, 0)

##### BRAKI DANYCH #####

# liczba braków danych
sum(dane == " ?")

# liczba rekordów zawierających braki 
rekordy_z_brakami_danych <- function(matrix)
{
  braki_danych <- c()
  for (i in 1:nrow(matrix))
  {  
    braki_danych[i] <- (any(matrix[i,] == " ?"))
  }
  return(sum(braki_danych))
}
rekordy_z_brakami_danych(dane)


# liczba rekordów zawierających braki danych w poszczególnych kolumnach
colSums(dane == " ?")

braki <- data.frame(zmienna = colnames(dane))
braki$liczba_braków <- colSums(dane == " ?")
braki$odsetek_braków <- round(braki$liczba_braków/nrow(dane),4)

path2 <- "/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/Zmienne przed Transformacją/Braki danych.xlsx"
write.xlsx(braki, path2, row.names = F)

#### Funkcja analizy zmiennych #####
analiza_zmiennej <- function(zmienna, odsetek_rosnąco = T){

# stworzenie tabeli liczebności poziomów zmiennej i udziałów klasy pozytywnej

## grupowanie po wartości zmiennej i ustalenie liczebności
liczebności <- dane %>%
  group_by(!!sym(zmienna))  %>% 
  summarize(Liczba = n())

## grupowanie po wartości zmiennej i ustalenie liczebności klasy 1
liczebności_klasy_1 <- dane %>% 
  filter(salary == 1) %>%
  group_by(!!sym(zmienna)) %>%
  summarize(liczba_klasy1 = n())

## grupowanie po wartości zmiennej i ustalenie liczebności klasy 0
liczebności_klasy_0 <- dane %>%
  filter(salary == 0) %>%
  group_by(!!sym(zmienna)) %>%
  summarize(liczba_klasy0 = n())

## połączenie tabel liczebności klas
liczebności_klas <- full_join(liczebności_klasy_1,liczebności_klasy_0)

## połączenie tabel liczebności wartości zmiennej oraz klas
data <- inner_join(liczebności,liczebności_klas) %>% replace(is.na(.), 0)

## utworzenie tabeli liczebności wartości zmiennej oraz udziałów klas
liczebności_i_udziały <- data %>%
  mutate(
    udzial_grupy = Liczba/nrow(dane),
    odsetek_1 = liczba_klasy1/Liczba,
    odsetek_0 = liczba_klasy0/Liczba)


if(odsetek_rosnąco == T){
  liczebności_i_udziały <- arrange(liczebności_i_udziały, odsetek_1)
}

## uporządkowanie poziomów zmiennej wg udziału klasy 1
liczebności_i_udziały[[zmienna]] <- factor(liczebności_i_udziały[[zmienna]], levels = liczebności_i_udziały[[zmienna]])

dir_path <- paste("/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/Zmienne przed Transformacją/",zmienna, sep = "")

if (!file.exists(dir_path)) {
  dir.create(dir_path)
}

if(odsetek_rosnąco == T){
  path <- paste("/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/Zmienne przed Transformacją/",zmienna,"/",zmienna," ","liczebności i udziały.xlsx", sep = "")
} else{
  path <- paste("/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/Zmienne przed Transformacją/",zmienna,"/",zmienna," ","liczebności i udziały nieuporządkowane.xlsx", sep = "")
}

## eksport do .xlsx
write.xlsx(liczebności_i_udziały, path, sheetName = "Sheet1")

# stworzenie wykresu przedstawiającego liczebnosći wartości zmiennej oraz udziały klasy pozytywnej

## stworzenie współczynnika pozwalającego na prawidłowe utworzenie linii na secondary axis
współczynnik <- max(liczebności$Liczba)/100

## stworzenie wykresu
wykres <- 
  ggplot(liczebności_i_udziały) +
  geom_bar(aes(x=!!sym(zmienna), y = Liczba), stat = "identity", fill = "grey") +
  geom_line(aes(x=(1:nlevels(!!sym(zmienna))), y = odsetek_1*max(liczebności$Liczba)), col = "#007788", size= 1) + #jak zmienić prawy axis?
  scale_y_continuous(sec.axis=sec_axis(~.*1/współczynnik,name="Odsetek klasy pozytywnej (%)")) +
  ylab("Liczba obserwacji") +
  theme_bw(base_size = 12)

## zapisanie wykresu

if(odsetek_rosnąco == T){
  nazwa <- paste(zmienna,"wykres.png")
} else{
  nazwa <- paste(zmienna,"wykres nieuporządkowany.png")
}

unikalne <- as.character(unique(dane[[zmienna]]))
długości_unikalnych <- sapply(unikalne, nchar)
suma_długości <- sum(długości_unikalnych)

if(suma_długości > 146){
  szerokość <- min(13.333/146*suma_długości,49)
} else{
  szerokość <- 13.333
}

ggsave(
  nazwa,
  plot = wykres,
  device = "png",
  path = dir_path,
  scale = 1,
  width = szerokość,
  height = 7.5,
)
}

#### Analiza poszczegónych zmiennych ####

## wszystkie zmienne
zmienne <- colnames(dane)
lapply(zmienne, analiza_zmiennej)

## zmienne nieuporządkowane wg odsetka
analiza_zmiennej("age", F)
analiza_zmiennej("education.num", F)
analiza_zmiennej("hours.per.week", F)
analiza_zmiennej("capital.gain", F)
analiza_zmiennej("capital.loss", F)





