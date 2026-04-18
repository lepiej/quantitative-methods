# =============================================================================
# Cel badawczy:
# Analiza relacji między wybranymi zmiennymi makroekonomicznymi w Niemczech:
# inwestycjami, dochodami i konsumpcją. Badanie wykorzystuje model VAR.
#
# Pytania badawcze:
# 1) Czy istnieją zależności przyczynowo-skutkowe między inwestycjami,
#    dochodami i konsumpcją w niemieckiej gospodarce?
# 2) Jak szok w jednej zmiennej wpływa na pozostałe zmienne w krótkim
#    i średnim okresie? (Analiza IRF)
# 3) Jaka część wariancji prognozy każdej zmiennej jest wyjaśniana
#    przez szoki w pozostałych zmiennych? (FEVD)
# 4) Jakie są prognozy dla inwestycji, dochodów i konsumpcji
#    w okresie 8 kwartałów?
# =============================================================================

# Zastosowanie modelu VAR do analizy danych makroekonomicznych
# Przykład opracowany w oparciu o podręcznik Lütkepohl, H. (2005).
# New introduction to multiple time series analysis. Springer Science & Business Media

# Konfiguracja środowiska
rm(list = ls())
graphics.off()
par(mar = c(1, 1, 1, 1))

if(!require(readxl)){install.packages("readxl")}
if(!require(vars)){install.packages("vars")}
if(!require(mFilter)){install.packages("mFilter")}
if(!require(urca)){install.packages("urca")}


library(readxl)
library(vars)
library(mFilter)
library(urca)

# Import danych
data <- read_excel("class3/data_DE.xlsx")
View(data)  

data <- data[1:76,]
invest <- ts(data$invest, start = c(1960, 1), frequency = 4)
income <- ts(data$income, start = c(1960, 1), frequency = 4)
cons <- ts(data$cons, start = c(1960, 1), frequency = 4)

# Wykresy szeregów czasowych
plot(invest)
plot(cbind(invest, income, cons))

# Analiza autokorelacji
invest.acf <- acf(invest, main = "investment")
invest.acf

income.acf <- acf(income, main = "income")
income.acf

cons.acf <- acf(cons, main = "consumption")
cons.acf

# biezace wartosci zaleza od tego co dzialo sie w przeszlosci, autokorelacja wystepuje. nie wystepowalaby gdyby ten wykres byl raz gora raz dol (nielinionwy)

# Testy pierwiastka jednostkowego: Rozszerzone testy Dickeya-Fullera (ADF) są przeprowadzane dla każdej zmiennej w celu sprawdzenia obecności pierwiastka jednostkowego, co wskazuje na niestacjonarność. Wybór opóźnień jest dokonywany na podstawie kryterium informacyjnego Akaike (AIC).
# Hipotezy testu ADF:
# Hipoteza zerowa (H0): Szereg czasowy ma pierwiastek jednostkowy (jest niestacjonarny).
# Hipoteza alternatywna (H1): Szereg czasowy nie ma pierwiastka jednostkowego (jest stacjonarny).


#logarytmiczne stopy zwrotu? na statyczna zmienna? 

adf.invest <- ur.df(invest, type = "trend", selectlags = "AIC")
summary(adf.invest)

adf.income <- ur.df(income, type = "trend", selectlags = "AIC")
summary(adf.income)

adf.cons <- ur.df(cons, type = "trend", selectlags = "AIC")
summary(adf.cons)

# Konwersja na obiekt szeregu czasowego
data <- ts(data, start = c(1960, 1), frequency = 4)
#jest blizej zera niz wartosci krytyczne wiec nie ma podstaw do odrzucenia hipotezy
#gdzie Value of test-statistic is: -1.0716 17.5785 10.1622 
#jest mniejsze niz wartosci w 
#Critical values for test statistics: 
#  1pct  5pct 10pct
#tau3 -4.04 -3.45 -3.15
#phi2  6.50  4.88  4.16
#phi3  8.73  6.49  5.47


# Pierwsze różnice logarytmów
diff_data <- diff(as.matrix(log(data)))

# Wykres
plot(diff_data)
#podejrzewamy ze zmienne sa stacjonarne (Stacjonarność zmiennych (zazwyczaj szeregów czasowych) oznacza, że ich właściwości statystyczne nie zmieniają się w czasie)
#przerprawdzimy test na pierwszych roznicach logarytmow zeby sprawdzic zaprzeczenie tego -> biestacjonarnosc zmiennych

#Ponownie badamy stacjonarność zmiennych po przekształceniu na pierwsze różnice lograrytmu
adf.invest1 <- ur.df(diff_data[,1], type = "trend", selectlags = "AIC")
summary(adf.invest1)

adf.income1 <- ur.df(diff_data[,2], type = "trend", selectlags = "AIC")
summary(adf.income1)

adf.cons1 <- ur.df(diff_data[,3], type = "trend", selectlags = "AIC")
summary(adf.cons1)

#szereg czasowy jest znaczacy na tych danych bo kontrhipoteza jest nieadekwatna bo vaule test-specific jest wieksza niz wartosci krytyczne wiec odrzucamy hipoteze o niezaleznosci czasowej. 


#Wybór rzędu opóźnienia modelu VAR - VARselect
#Kryterium Informacyjne Akaikego (AIC), Kryterium Informacyjne Hannana-Quinna (HQ), Kryterium Informacyjne Schwarza (SC) / Kryterium Informacyjne Bayesa (BIC), Final Prediction Error (FPE)
#wybralismy 12 okresow i model ze stala
info.bv <- VARselect(diff_data, lag.max = 12, type = "const")
info.bv$selection

#Estymacja modelu VAR(2) ze stałą:
# yt=v+∑i=12Aiyt−i+ut,
# gdzie ut∼N(0,Σ).


#opznienia p =2, model ze stala
model <- VAR(diff_data, p = 2, type = "const")
summary(model)
#kozde rownanie jest w zasadzie inedtyczne i nnasze inwestycej wyjasnniamy kolejno o poprzednie okresy ktore okreslilismy w poprzednim wybierajac opozninienie

# Diagnostyka modelu VAR:
# Test Portmanteau - serial.test
#Hipoteza zerowa (H0): W szeregu czasowym nie ma autokorelacji.
#Hipoteza alternatywna (H1): W szeregu czasowym występuje autokorelacja.

bv.serial <- serial.test(model, lags.pt = 12, type = "PT.asymptotic")
bv.serial

#calkiem spore p-value wiec mamy podstawe do odrzucenia hipotezy 

plot(bv.serial, names = "invest")

#Test heteroskedastyczności reszt - wielowymiarowy test efektu ARCH - arch.test
#Hipoteza zerowa (H0): Nie występuje efekt ARCH -- to odrzuclismy bo VAR nam wyszlo do odrzucenia wiec testyjemy hipoteze alternatywna
#Hipoteza alternatywna (H1): Występuje efekt ARCH

bv.arch <- arch.test(model, lags.multi = 5, multivariate.only = TRUE)
bv.arch 

#p-value duze wiec nie ma podstwy do odrzucenia hipotezy, nie wystepuje tutaj efekt arch czyli nie ma pojedynczych wariacji odbiegajacych od grupy

#Test normalności reszt - normality.test
#Hipoteza zerowa (H0): Reszty mają rozkład normalny.
#Hipoteza alternatywna (H1): Reszty nie mają rozkładu normalnego.

bv.norm <- normality.test(model, multivariate.only = TRUE)
bv.norm$jb.mul

#w przypadku tego podstwowego testu nie odrzucamy hipotezy alternatywnej bo p-value jest male i to oznacza, ze te reszty nie maja rozkladu normalnego 

#Test stabilności CUSUM - stability

bv.cusum <- stability(model, type = "OLS-CUSUM")
plot(bv.cusum)

#Test przyczynowości w sensie Grangera
#Hipoteza zerowa (H0): Zmienna X nie jest przyczyną Grangera zmiennej Y.
#Hipoteza alternatywna (H1): Zmienna X jest przyczyną Grangera zmiennej Y.

bv.cause.invest <- causality(model, cause = "invest")
bv.cause.invest
#zakladamy ze inwestycej nie sa przyczyna  
#p-value duze nie ma podstawy do odrzucenia 

bv.cause.income <- causality(model, cause = "income")
bv.cause.income
#zakladamy cons do not Granger-cause invest income
#p-value male jest podstawy do odrzucenia 
# jest podstwa do zbadania alternatywnej hipotezy

bv.cause.cons <- causality(model, cause = "cons")
bv.cause.cons
#zakladamy ze konsumpcja nie jest przyczyna w sensie Grangera 
#p-value duze nie ma podstawy do odrzucenia 

#Funkcje odpowiedzi na impuls (IRF)
#efekt szoku inwestycji na wydatki konsupcyjne
irf.cons1 <- irf(model, impulse = "invest", response = "cons",
               n.ahead = 8, boot = TRUE)
plot(irf.cons1, ylab = "wydatki konsumpcyjne", main = "Szok pochodzący z inwestycji")
#ten szok z inwestycji na pocatku powyzej roku juz nie ma dala nas znaczenia 


#efekt szoku inwestycji na dochód
irf.income <- irf(model, impulse = "invest", response = "income",
                 n.ahead = 8, boot = TRUE)
plot(irf.income, ylab = "dochód", main = "Szok pochodzący z inwestycji")


#efekt szoku konsumpcji na dochód
irf.income1 <- irf(model, impulse = "cons", response = "income",
                  n.ahead = 8, boot = TRUE)
plot(irf.income1, ylab = "dochód", main = "Szok pochodzący z wydatków konsumpcyjnych")

#efekt szoku konsumpcji na inwestycje
irf.invest <- irf(model, impulse = "cons", response = "invest",
                  n.ahead = 8, boot = TRUE)
plot(irf.income, ylab = "inwestycje", main = "Szok pochodzący z wydatków konsumpcyjnych")

#dekompozycja wariancji błędu prognozy - FEVD
#chcemy zbadaj jak nasza zmienna reaguje na swoja zmiennosc i zmiennosc pochodzaca od innych zmiennych. czy te zmiany zaleza od inwestycji czy to jednak inne zmienne. 

bv.vardec <- fevd(model, n.ahead = 12)
plot(bv.vardec)

#inwestycej maja wyplw na inwestyje, przychod na przychod, ale juz przy konsumpcji najwiekszy wplyw ma konsumpcja ale przychod tez juz ma wplyw

#prognozowanie na 8 okresów do przodu (przedział ufności 95%) - predict

predictions <- predict(model, n.ahead = 8, ci = 0.95)

#wykresy liniowe
plot(predictions, names = "invest")
#na czerwono przedzial ufnosci a na niebiesko predykcja 
#z tego 30 stron czesci empirycznej pracy dyplomowej 


plot(predictions, names = "income")

#wykresy wachlarzowe

fanchart(predictions, names = "cons")
#narodowy bank polski uwiebia NVAR wiec mozna tam juz aplikowac 