############################################################
# Metody badań w finansach - panele
# Plik: bank_panel_data.csv
# Zbiór danych na dzisiejsze zajęcia został pobrany z # https://www.kaggle.com/datasets/nepprkpandey/bank-dataset-for-panel-regression?resource=download
############################################################

# 0) Pakiety __________________________________________________
install.packages(c("readr","dplyr","plm","lmtest","sandwich","fixest","pROC","lme4"))

library(readr)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(fixest)
library(pROC)
library(lme4)

# 1) Wczytanie danych__________________________________________
# session -> set working directory -> choose directory
# Ustaw ścieżkę do pliku:
data <- bank_panel_data

# Szybki podgląd
print(head(data))
str(data)

# 1.1) Struktura danych panelowych (jednostka × czas)_________
# Sprawdzenie: liczba banków i lat
cat("\nLiczba banków:", n_distinct(data$Bank_ID), "\n")
cat("Zakres lat:", min(data$Year), "-", max(data$Year), "\n")

# Czy panel jest zbilansowany?
panel_check <- data %>%
  count(Bank_ID) %>%
  summarise(minT = min(n), maxT = max(n))
print(panel_check)
# potem zobaczymy to też w wydrukach z poszczególnych modeli

# Konwersja do pdata.frame (klucz: Bank_ID, Year)
# bardzo ważne
pdata <- pdata.frame(data, index = c("Bank_ID", "Year"))

# 1.2) Nowe zmienne____________________________________________
# ROA, dźwignia, loan to assets
pdata <- pdata %>%
  mutate(
    ROA = Profit / Assets,
    Equity = Assets - Liabilities,
    Leverage = Assets / Equity,
    Loan_to_Assets = Loans / Assets,
    Size = log(Assets)
  )

# 2) MODELE DLA DANYCH PANELOWYCH______________________________
# Zmienna zależna: ROA
# Regresory: Loans/Assets, Leverage, Interest_Rate, Size GDP_Growth

# 2.1) Pooled OLS________________________________________________
# Pooled OLS ignoruje heterogeniczność banków (efekty stałe banku)
m_pool <- plm(ROA ~ Loan_to_Assets + Leverage + Interest_Rate + Size + GDP_Growth,
              data = pdata,
              model = "pooling")
summary(m_pool)

ols <- lm(ROA ~ Loan_to_Assets + Leverage + Interest_Rate + Size + GDP_Growth, data = pdata)
summary(ols)
# Obserwacja: dokładnie takie same oszacowania, zwykła regresja lm działa tak samo jak plm 

# jeśli cechy banku stałe w czasie korelują z X, pooled OLS będzie obciążony (omitted variable bias).

# 2.2) Model efektów stałych (FE)_____________________________________
m_fe <- plm(ROA ~ Loan_to_Assets + Leverage + Interest_Rate + Size + GDP_Growth,
            data = pdata,
            model = "within") 
summary(m_fe)

# FE "within": jak zmiana X w tym samym banku w czasie wiąże się ze zmianą ROA.

# Przykład: Brak estymacji zmiennych stałych w czasie
# Dodajmy zmienną 0 - 1, 1 dla banków, które mają nr 2, 4, 6, 8 - załóżmy, że są to banki które maja wspólną stałą w czasie cechę
# np. działają w jednym kraju/ są obłożone dodatkowymi regulacjami, itp
pdata$Selected_Bank <- ifelse(data$Bank_ID %in% c(2, 4, 6, 8), 1, 0)

m_fe_fail <- plm(ROA ~ Loan_to_Assets + Selected_Bank, data = pdata, model = "within")
summary(m_fe_fail)

# 2.3) Model efektów losowych (RE)_____________________________________
# Założenie: brak korelacji efektu banku z regresorami
m_re <- plm(ROA ~ Loan_to_Assets + Leverage + Interest_Rate + Size + GDP_Growth,
            data = pdata,
            model = "random")
summary(m_re)

# 2.4) Wybór modelu_________________________________________
# Poolability test
pFtest(m_fe, m_pool)

# Test Hausmana
# H0: RE jest zgodny (brak korelacji efektów z X)
haus <- phtest(m_fe, m_re)
print(haus)

### MATERIAŁY DODATKOWE
### DODATKOWE MODELE_______________________________________
# SE odporne i klastrowane po banku:
coeftest(m_pool, vcov = vcovHC(m_pool, type = "HC1", cluster = "group"))

# SE klastrowane po banku (często w panelach):
coeftest(m_fe, vcov = vcovHC(m_fe, type = "HC1", cluster = "group"))

# FE z efektami czasowymi (Two-Way FE: bank + rok)
m_twfe <- plm(ROA ~ Loan_to_Assets + Leverage + Interest_Rate + Size + GDP_Growth,
              data = pdata,
              model = "within",
              effect = "twoways")
summary(m_twfe)
coeftest(m_twfe, vcov = vcovHC(m_twfe, type = "HC1", cluster = "group"))

# SE odporne/klastrowane:
coeftest(m_re, vcov = vcovHC(m_re, type = "HC1", cluster = "group"))

### PROBLEMY W MODELOWANIU__________________________________
# AUTOKORELACJA (panel)
# H0: brak autokorelacji
cat("\n--- TEST AUTOKORELACJI (pbgtest) ---\n")
print(pbgtest(m_fe))

# HETEROSKEDASTYCZNOŚĆ
# H0: homoskedastyczność
cat("\n--- TEST HETEROSKEDASTYCZNOŚCI (Breusch–Pagan) ---\n")
print(bptest(m_fe))

# ZALEŻNOŚĆ PRZEKROJOWA (cross-sectional dependence)
# H0: brak zależności przekrojowej
cat("\n--- TEST ZALEŻNOŚCI PRZEKROJOWEJ (Pesaran CD) ---\n")
print(pcdtest(m_fe, test = "cd"))