# ==============================================================================
# # Model GARCH w R, PROJET ZALICZENIOWY
# ==============================================================================

# ## 1. Wstęp, skład zespołu zaliczeniowego i pakiety
# 
#
# W przypadku finansowych szeregów czasowych często mamy do czynienia z 
# obserwacjami o wysokiej częstotliwości (dzienne, godzinowe, a nawet 
# milisekundowe). W ramach projektu zaliczeniowego ograniczymy się do obserwacji dziennych.
# 
# Wykorzystamy następujące pakiety:
# * `rugarch`: do jednowymiarowych modeli GARCH.
# * `rmgarch`: do wielowymiarowych modeli GARCH.
# * `quantmod`: do pobierania danych i podstawowej analizy technicznej.

rm(list=ls())
set.seed(12345)

if(!require(readxl)){install.packages("rugarch")}
if(!require(readxl)){install.packages("rmgarch")}
if(!require(readxl)){install.packages("quantmod")}

library(rugarch)
library(rmgarch)
library(quantmod)

library(readxl)
data <- read_excel("class3/data_DE.xlsx")
View(data) 

# ==============================================================================
# ## 2. Pobieranie danych
# 
# Użyjemy funkcji `getSymbols` z pakietu `quantmod` do pobrania danych z 
# Yahoo Finance. Pobierzemy dane dla indeksu NASDAQ Composite (`^IXIC)` oraz spółek 
# NVIDIA Corporation (`NVDA`), Microsoft Corporation  (`MSFT`) i Intel (`INTC`).
# 
# Interesuje nas okres od 2008-01-03 do 2026-04-18.
# ==============================================================================

startDate <- as.Date("2008-01-03")
endDate <- as.Date("2026-04-18")

# Pobieranie danych dla NASDAQ, NVDA, MSFT, INTC
getSymbols(c("^IXIC", "NVDA", "MSFT", "INTC"), from = startDate, to = endDate)

# Sprawdźmy, jak wyglądają dane dla NVIDIA Obiekt `xts` zawiera ceny otwarcia, 
# najwyższe, najniższe, zamknięcia, wolumen i ceny skorygowane.

head(NVDA)

# Możemy również wygenerować wykres cen akcji poszczególnych indeksów używając funkcji `chart_Series`.

chart_Series(NVDA)

# ==============================================================================
# ### Obliczanie stóp zwrotu
# 
# Do modelowania zmienności używamy stóp zwrotu, a nie poziomów cen. Funkcja 
# `dailyReturn` przekształca ceny w dzienne stopy zwrotu. Następnie tworzymy 
# ramkę danych `rX` zawierającą stopy zwortu dla wszystkich trzech spółek, co 
# przyda się w modelu wielowymiarowym.
# ==============================================================================

rNVDA <- dailyReturn(NVDA)
rMSFT <- dailyReturn(MSFT)
rINTC <- dailyReturn(INTC)

# Tworzymy zestaw danych dla modelu wielowymiarowego
rX <- merge(rNVDA, rMSFT, rINTC)
colnames(rX) <- c("rNVDA", "rMSFT", "rINTC")

# ==============================================================================
# ## 3. Jednowymiarowy model GARCH (Univariate)
# 
# Wykorzystamy pakiet `rugarch`. Pierwszym krokiem jest specyfikacja modelu za 
# pomocą funkcji `ugarchspec`.
# 
# Domyślna specyfikacja to zazwyczaj model GARCH(1,1) ze średnią modelowaną 
# jako ARFIMA(1,0,1).
# 
# My jednak zmienimy model średniej na AR(1) (czyli ARMA(1,0)), zachowując 
# standardowy GARCH(1,1) dla wariancji.
# ==============================================================================

# Specyfikacja modelu: AR(1) dla średniej, GARCH(1,1) dla wariancji

ug_spec <- ugarchspec(mean.model = list(armaOrder = c(1, 0)))

# ==============================================================================
# ### Estymacja modelu
# 
# Dopasowujemy model do danych (stóp zwrotu NVDA) używając funkcji `ugarchfit`. 
# Obiekt wynikowy zawiera oszacowane parametry (mu, ar1, omega, alpha1, beta1) 
# oraz testy diagnostyczne.
# ==============================================================================

ugfit <- ugarchfit(spec = ug_spec, data = rNVDA)

# ==============================================================================
# ### Analiza wyników
# 
# Poniżej wyodrębniamy zmienność i kwadraty reszt, aby je zwizualizować.
# ==============================================================================

# Wyciągnięcie współczynników, wariancji i reszt
ug_coef <- coef(ugfit)
ug_var <- sigma(ugfit)^2.          # sigma() to odchylenie standardowe, podnosimy do kwadratu
ug_res2 <- residuals(ugfit)^2

# Wykres kwadratów reszt (czarny) i oszacowanej wariancji (zielony)
# Ponieważ to obiekty xts, wykres automatycznie uwzględni daty na osi X
plot(ug_res2, main = "Kwadraty reszt i wariancja warunkowa NVDA", col = "black")
lines(ug_var, col = "green")

# ==============================================================================
# ### Prognozowanie
# 
# Do prognozowania zmienności na kolejne dni używamy funkcji `ugarchforecast`. 
# Wykonamy prognozę na 10 dni do przodu.
# 
# Zauważ, że `sigma` w prognozie to zmienność (odchylenie standardowe), czyli 
# pierwiastek z wariancji.
# ==============================================================================
ugfore <- ugarchforecast(ugfit, n.ahead = 10)

# Wyciągnięcie prognozy zmienności (sigma)
ug_f_sigma <- as.numeric(sigma(ugfore)) 
ug_f_var <- ug_f_sigma^2 # wariancja prognozowana

# Aby umieścić prognozę w kontekście, połączymy ostatnie 20 obserwacji 
# historycznych z 10-dniową prognozą na jednym wykresie.
ug_var_t <- c(tail(as.numeric(ug_var), 20), rep(NA, 10))
ug_res2_t <- c(tail(as.numeric(ug_res2), 20), rep(NA, 10))
ug_f_combined <- c(rep(NA, 20), ug_f_var) 

# Rysowanie prognozy
plot(ug_res2_t, type = "l", main = "Prognoza wariancji (pomarańczowy)")
lines(ug_f_combined, col = "orange", lwd = 2)
lines(ug_var_t, col = "green")

# ==============================================================================
# ## 4. Wielowymiarowy model GARCH (Multivariate)
# 
# Modelowanie zmienności dla wektora aktywów jest bardziej złożone. Wykorzystamy 
# pakiet `rmgarch` i model DCC (Dynamic Conditional Correlation).
# 
# Proces składa się z dwóch etapów:
# 1. Estymacja modeli GARCH dla każdego aktywa z osobna.
# 2. Estymacja dynamiki korelacji między standaryzowanymi resztami.
# ==============================================================================

# ==============================================================================
# ### Specyfikacja modelu
# 
# Zdefiniujemy ten sam model jednowymiarowy (AR(1)-GARCH(1,1)) dla wszystkich 
# trzech aktywów (NVIDIA, Microsoft, Intel) używając `replicate`. Następnie 
# dopasujemy te modele używając `multifit`.
# ==============================================================================

# Specyfikacja jednowymiarowa dla 3 aktywów
uspec.n <- multispec(replicate(3, ugarchspec(mean.model = list(armaOrder = c(1, 0)))))

# Dopasowanie modeli jednowymiarowych (Univariate fit)
multf <- multifit(uspec.n, rX)

# Teraz specyfikujemy model korelacji DCC. Użyjemy standardowego rzędu (1,1) i 
# wielowymiarowego rozkładu normalnego (`mvnorm`).

# Specyfikacja DCC
spec1 <- dccspec(uspec.n, dccOrder = c(1,1), distribution = "mvnorm")

# Estymacja modelu DCC
# fit = multf wskazuje, by użyć wcześniej obliczonych modeli jednowymiarowych
fit1 <- dccfit(spec1, data = rX, fit.control = list(eval.se = TRUE), fit = multf)

# ==============================================================================
# ### Analiza korelacji
# 
# Z modelu DCC możemy wyodrębnić macierze kowariancji (`rcov`) i korelacji 
# (`rcor`) zmienne w czasie.
# 
# Obiekt `cor1` jest tablicą trójwymiarową, gdzie trzeci wymiar to czas.
# ==============================================================================

cov1 <- rcov(fit1)  # Kowariancja
cor1 <- rcor(fit1) # Korelacja

# Wymiary macierzy korelacji
dim(cor1)

# Macierz korelacji dla ostatniego dnia
cor1[,,dim(cor1)[3]]

# ==============================================================================
# ### Wizualizacja korelacji
# 
# Wykreślmy zmienną w czasie korelację między Microsoft (aktywo nr 2) a Intel
# (aktywo nr 3). W macierzy to element `[2, 3, ]`. Przekonwertujemy go na 
# obiekt `xts` dla łatwiejszego rysowania.
# ==============================================================================
cor_BG <- as.xts(cor1[2,3,], order.by = index(rX))
plot(cor_BG, main = "Korelacja dynamiczna Intel i Microsoft")


# Poniżej przedstawiamy zestawienie korelacji dla wszystkich par aktywów.

par(mfrow=c(3,1))
plot(as.xts(cor1[1,2,], order.by = index(rX)), main="NVIDIA i Microsoft")
plot(as.xts(cor1[1,3,], order.by = index(rX)), main="NVIDIA i Intel")
plot(as.xts(cor1[2,3,], order.by = index(rX)), main="Microsoft i Intel")

# ==============================================================================
# ### Prognozowanie korelacji DCC
# 
# Używamy funkcji `dccforecast` do przewidzenia macierzy korelacji na 10 dni 
# do przodu.
# ==============================================================================

dccf1 <- dccforecast(fit1, n.ahead = 10)

# Użycie funkcji rcor() do wyciągnięcia prognoz korelacji z modelu DCC
Rf <- rcor(dccf1)

# Wyciągnięcie prognoz dla konkretnych par
corf_NM <- Rf[[1]][1,2,] 
corf_NI <- Rf[[1]][1,3,] 
corf_MI <- Rf[[1]][2,3,] 

# Na koniec wizualizujemy prognozy korelacji, dołączając je do ostatnich 20 
# obserwacji historycznych (kolor pomarańczowy oznacza prognozę).
par(mfrow = c(3, 1))

# NVIDIA i Microsoft
c_NM <- c(tail(cor1[1,2,], 20), rep(NA, 10))
cf_NM <- c(rep(NA, 20), corf_NM)
plot(c_NM, type="l", main="Korelacja NVIDIA i Microsoft")
lines(cf_NM, type="l", col = "orange", lwd = 2)

# NVIDIA i Intel
c_NI <- c(tail(cor1[1,3,], 20), rep(NA, 10))
cf_NI <- c(rep(NA, 20), corf_NI)
plot(c_NI, type="l", main="Korelacja NVIDIA i Intel")
lines(cf_NI, type="l", col = "orange", lwd = 2)

# Microsoft i Intel
c_MI <- c(tail(cor1[2,3,], 20), rep(NA, 10))
cf_MI <- c(rep(NA, 20), corf_MI)
plot(c_MI, type = "l", main="Korelacja Microsoft i Intel")
lines(cf_MI, type = "l", col = "orange", lwd = 2)

par(mfrow = c(1, 1)) # Reset układu wykresów

# Dynamiczne korelacje między NVIDIA, Microsoft i Intel są dodatnie i 
# zmienne w czasie, co wskazuje na wspólną reakcję na czynniki rynkowe. 
# Najwyższe wartości obserwujemy dla pary NVDA–INTC jako reprezentacja wspolnego
# sektora półprzewodników, choć pozostałe korelacje również wykazują wyraźny 
# trend wzrostowy. W szczególności NVDA–MSFT ma większą korelację ponieważ 
# inwestorzy kupują/sprzedają je razem co prowadzi do wysokiej korelacji. 
# W badanym okresie wszystkie trzy zależności są relatywnie wysokie, 
# co wskazuje na rosnącą synchronizację analizowanych spółek. 
# Prognozy sugerują stabilizację korelacji na zbliżonym poziomie dla NVDA 
# oraz ciekawą do dalszej obserwacji rosnącej korelacji między MSFT i INTC
# w krótkim horyzoncie.
