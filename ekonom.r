######################## wstep ########################
# czyszczenie zmiennych
rm(list=ls()) 

# instalacja i wczytanie bibliotek
if (!("readxl" %in% rownames(installed.packages()))) install.packages("readxl") 
#if (!("ggpubr" %in% rownames(installed.packages()))) install.packages("ggpubr")
library(readxl)
#library(ggpubr)

# sciezka projektu
setwd("B:/Projekty/Studia/VI_semestr/Ekonometria")

# jezyk konsoli
Sys.setenv(LANG = "en")

######################## wczytanie danych ########################
dane <- read_excel("dane.xlsx", sheet=1)
dane <- as.data.frame(dane)

# konwersja na numeric
for (i in 1:length(dane)) { 
  dane[,i] <- as.numeric(dane[,i])
}

# podzial danych na dane wejsciowe i wyjsciowe
dane_x <- dane[-1]
dane_y <- dane[,1]

######################## wstepna selekcja danych ########################
# obliczenie wspolczynnikiem zmiennosci oraz wspolczynnikiem korelacji miedzy zmienna a wartoscia Y
fs_table <- data.frame(c(), c())
for (i in 1:ncol(dane_x)) {
  fs_table[1,i] <- sd(dane_x[,i])/mean(dane_x[,i])
  fs_table[2,i] <- cor(dane_x[,i], dane_y,  method = "pearson")
}
row.names(fs_table) <- c("wsp_zmienn", "pearson_korr")
colnames(fs_table) <- colnames(dane_x)

# wybranie odpowiednich zmiennych
dane_w <- data.frame(c(1:17))
licznik <- 1
for (i in 1:ncol(fs_table)) {
  # jesli wspolczynnikiem zmiennosci jest wiekszy niz 10% oraz korelacja zmiennej z Y wieksza niz 70%
  if (abs(fs_table[1,i]) > 0.10 && abs(fs_table[2,i]) > 0.70) {
    dane_w[,licznik] <- dane_x[,i]
    colnames(dane_w)[licznik] <- colnames(dane_x)[i]
    licznik <- licznik + 1
  } else {
    cat("odrzucam",  colnames(dane_x)[i], "\n")
  }
}
remove(dane_x, fs_table, dane, licznik) # usuwanie

######################## metoda Hellwiga ########################
func_hellwig <- function(dane) {
  N <- length(dane) # oblicza ilosc zmiennych objasniajacych
  M <- 2^N-1 # oblicza ilosc kombinacji 0-1
  
  zm_obj <- dane[,1:N] # macierz zmiennych objasniajacych
  r <- cor(zm_obj) # macierz korelacji miedzy zmiennymi
  r <- as.matrix(abs(r)) # wartosci bezwzgledne i macierz 
  R <- cor(zm_obj, dane_y) # wektor korelacji Y z kazda ze zmiennych
  R <- as.vector(R)
  
  tab <- as.matrix(expand.grid(rep(list(0:1), N)))[-1,] # macierz 0-1 kombinacji
  colnames(tab) <- colnames(dane_w)[1:N]
  wyniki <- matrix(0,M,N) # macierz 0 na wyniki czastkowe pojemnosci
  colnames(wyniki) <- colnames(dane_w)[1:N]
  
  for (i in 1:M) {
    for (j in 1:N) {
      if (tab[i,j] != 0) {
        wyniki[i,j] <- (R[j]^2)/(tab[i,]%*%(as.vector(r[,j])))
      }
    }
  }
  maks <- which.max(rowSums(wyniki))
  
  # podglad kilku najlepszych wynikow
  wynikiS <- cbind(wyniki,0)
  wynikiS[,(N+1)] <- rowSums(wyniki)
  nazwy <- colnames(as.data.frame(wyniki))
  colnames(wynikiS) <- c(nazwy, "hellwigP")
  
  ind <- order(wynikiS[,(N+1)], decreasing = TRUE)[1:15] # zwroci 15 najlepszych
  najlepsze15 <- wynikiS[ind,]
  
  return(as.data.frame(najlepsze15))
}

hellwig_wynik <- func_hellwig(dane_w) # 15 najepszych wynikow

check_hellwig <- function(col_no, hellwig) {
  hellwig$hellwigP <- 0
  
  # wybranie odpowiednich zmiennych po metodzie Hellwiga
  dane_h <- data.frame(c(1:17))
  licznik_h <- 1
  for (j in 1:ncol(hellwig)) {
    # wybieram te kolumny, ktore wyszly w metodzie hellwiga
    if (hellwig[col_no,j] > 0) {
      dane_h[,licznik_h] <- dane_w[,j]
      colnames(dane_h)[licznik_h] <- colnames(dane_w)[j]
      licznik_h <- licznik_h + 1
    } else {
      cat("odrzucam",  colnames(dane_w)[j], "\n")
    }
  }
  return(dane_h)
}

dane_h1 <- check_hellwig(1, hellwig_wynik)
dane_h2 <- check_hellwig(2, hellwig_wynik)
dane_h3 <- check_hellwig(3, hellwig_wynik)

######################## sprawdzanie modeli ########################
### Residuals:
# residuals maja byc male - to reszty miedzy modelem a wartosciami rzeczywistymi
### Coefficients:
# Estimate to wagi dla zmiennych
# t value to 
# PR(>|t|) to p-value (im mniejszetym lepsze) - np. p-value=0.65, tzn. 65% szans, ze jest bezuzyteczne
# R^2 ma byc bliskie 1 zeby model byl dopasowany dobrze
### Performance Measures:
# Residual standard error - odchylenie standardowe reszt - lepsze mniejsze
# Multiple R-squared - R^2 ma byc bliskie 1
# F-statistic - test czy choc jedna ze zmiennych jest znaczaca - ma byc mniejsze niz 0.05

# zmiana danych y na szereg czasowy
dane_y_ts <- ts(dane_y, frequency = 1, start = c(2005, 1), end = c(2021, 1))
plot(dane_y_ts, main = "Median Sales Price of Houses Sold for the United States", 
     ylab = "$", xlab = "Rok", lwd = 2)

# 3 - ten zestaw zmiennych sie nie nadaje
reg_h3 = lm(dane_y_ts ~ dane_h3[,1] + dane_h3[,2] + dane_h3[,3] + dane_h3[,4] + dane_h3[,5])
summary(reg_h3)

# 1
reg_h1 = lm(dane_y_ts ~ dane_h1[,1] + dane_h1[,2] + dane_h1[,3] + dane_h1[,4])
summary(reg_h1) # widac, ze w tym modelu x4 ma duza wartosc p - odrzucam ja

# 2
reg_h2 = lm(dane_y_ts ~ dane_h2[,1] + dane_h2[,2] + dane_h2[,3])
summary(reg_h2) # wszystkie zmienne maja niska wartosci p - ten model to drugi najlepszy zestaw zmiennych z metody hellwiga
model <- reg_h2 # przypisuje wybrany model do zmiennej model dla ulatwienia

######################## wybrany model ########################
barplot(model$residuals, col = "red", main = "Reszty") # reszty

plot(model$fitted.values, type = "l", col = "red", lwd = 3,
     main = "Median Sales Price of Houses Sold for the United States\nPorównanie modelu z wartościami oryginalnymi", 
     ylab = "$", xlab = "Rok", xaxt = "n") # rysowanie modelu
axis(1, at=seq(1, 17, by=1), labels = seq(2005, 2021, by=1))
lines(dane_y, col = "blue", lwd = 3) # oryginalne dane
legend("topleft", legend=c("model", "oryginalne Y"), col=c("red", "blue"), lwd = 2)

######################## test modelu ########################

test <- function(v_train, v_test) {
  # dziele dane na zbiory testowe i uczace
  dane_y_train <- dane_y[v_train]
  dane_y_test <- dane_y[v_test]
  
  dane_h2_test <- dane_h2[v_test,1]
  dane_h2_test <- as.data.frame(dane_h2_test)
  dane_h2_test[,2] <- dane_h2[v_test,2]
  dane_h2_test[,3] <- dane_h2[v_test,3]
  names <- c("Homeowner_Vacancy_Rate_in_the_United_States", "Total_Business_Sales",
             "Total_Construction_Spending_Total_Construction_in_the_United_States")
  colnames(dane_h2_test) <- names
  
  dane_h2_train <- dane_h2[v_train,1]
  dane_h2_train <- as.data.frame(dane_h2_train)
  dane_h2_train[,2] <- dane_h2[v_train,2]
  dane_h2_train[,3] <- dane_h2[v_train,3]
  dane_h2_train[,4] <- dane_y_train
  names[4] <- "Y"
  colnames(dane_h2_train) <- names
  
  # tworze taki sam model
  model_test = lm(Y ~ Homeowner_Vacancy_Rate_in_the_United_States + Total_Business_Sales + Total_Construction_Spending_Total_Construction_in_the_United_States, data = dane_h2_train)
  przediwdziane <- predict(model_test, dane_h2_test)
  roznica <- dane_y_test - przediwdziane
  
  # lacze dane
  dane_test_plot <- append(dane_y_train, przediwdziane)
  plot(dane_test_plot, type = "l", col = "red", lwd = 3,
       main = "Median Sales Price of Houses Sold for the United States\nPorównanie modelu z wartościami oryginalnymi", 
       ylab = "$", xlab = "Rok", xaxt = "n")
  axis(1, at=seq(1, 17, by=1), labels = seq(2005, 2021, by=1))
  lines(dane_y, col = "blue", lwd = 3)
  legend("topleft", legend=c("model", "oryginalne Y"), col=c("red", "blue"), lwd = 2)
  
  barplot(roznica, col = "red", main = "Reszty") # reszty
}

test(c(1:12), c(13:17))
test(c(1:10), c(11:17))
test(c(1:7), c(8:17))
