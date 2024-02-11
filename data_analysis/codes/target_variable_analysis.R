# wczytanie danych
dane<- read.csv("/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/dane_oryginalne.csv", header=TRUE, sep=",")
head(dane)
dim(dane)
sum(apply(dane,1,anyNA))
apply(dane,2,anyNA)
summary(dane)
table(factor(dane$salary))

# zamiana zmiennej objaśnianej na 0-1
dane$salary <- ifelse(dane$salary == " >50K", 1, 0)

# faktoryzacja zmiennej objaśnianej
dane$salary <- as.factor(dane$salary)

# stworzenie tabeli liczebności poziomów zmiennej i udziałów klasy pozytywnej

## grupowanie po wartości zmiennej i ustalenie liczebności
liczebności <- dane %>%
  group_by(salary)  %>% 
  summarize(Liczba = n())

## stworzenie wykresu
wykres_salary <- 
  ggplot(liczebności) +
  geom_bar(aes(x=salary, y = Liczba), stat = "identity", fill = "grey") +
  theme_bw(base_size = 12)

## wyświetlenie wykresu
wykres_salary

## zapisanie wykresu
ggsave(
  "salary wykres.png",
  plot = wykres_salary,
  device = "png",
  path = "/Users/Kuba/Library/CloudStorage/OneDrive-SGH/SGH/Praca Licencjacka/Analiza Danych/Zmienne przed Transformacją/salary",
  scale = 1,
  width = 13.33,
  height = 7.5,
)