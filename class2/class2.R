#  Czy srednie w tych grupach sa w miare rowne? -> test ANOVA - analysis of variance : odpowiada binanie tak lub nie 
# Ale zeby wykonac ta analize nalezy sprawdzic czy spelnione sa warunki do wyboru tej metody. Warunki: 
# -normalnosc rozkladow
# -homogenicznosc wariancji 
# Jesli sa roznice, to nalezy wykonac test Tukey HSD. Porownujac srednie parami. 

# Jesli porownywac ANOVE parami to testy bylyby bardzo obciazone. alternatywa dla ANOVA jesli:
# nie ma normalnosci rozkladow -> Kruskal-Wallis. 
# jesli wszystkie warunki ANOVA spelnione -> test UDENTA (nie uszczegolawia ANOVA)

########################

# Kroki kalkulacji:
  
# 1.	Zainstaluj i załaduj bibliotekę nortest. Załaduj bibliotekę readxl

install.packages("nortest")
library("nortest")
library("readxl")

# 2.	Załaduj dane (szeregi czasowe stóp zwrotu) z arkusza 'Returns' skoroszytu 'Data1.xlsx' przesłanego wcześniej do pamięci sesji

library(readxl)
Data_Returns <- read_excel("class2/Data1.xlsx", 
                           sheet = "Returns")
View(Data_Returns)

# 3.	Zaprezentuj kilka wierszy z zaczytanej ramki danych

head(Data_Returns)

# 4.	Załaduj dane (szeregi czasowe stopy wolnej od ryzyka) z arkusza 'Risk_free' skoroszytu 'Data1.xlsx' przesłanego wcześniej do pamięci sesji

library(readxl)
Data_Risk_Free <- read_excel("class2/Data1.xlsx", 
                           sheet = "Risk_free")
View(Data_Risk_Free)

# 5.	Zaprezentuj kilka wierszy z zaczytanej ramki danych

tail(Data_Risk_Free)

# 6.	Oblicz wartości wskaźnika Sharpa dla wszystkich akcji

#tworzymy 3 puste zmienne ze wzoru na Sharpe'a. 1. Średnia z arkusza returns 2. Średnia stopa zwrotu wolna od ryzyka 3. Średnia z variancji
# pomijamy pierwsza kolumne z datami zeby uczyc funkcji przyjmujacej liczby =[,-1] lub =[,2:62], aby usunac NA dodajemy  na.rm = T

# Średnia z arkusza returns 
?colMeans
avg_Returns <- colMeans(Data_Returns[,-1], na.rm = T)
View(avg_Returns)

# Średnia stopa zwrotu wolna od ryzyka
avg_Risk_Free <- colMeans(Data_Risk_Free[,-1], na.rm = T)
View(avg_Risk_Free)

# Śrenia z wariancji - odchykenie standardowe = standard diviation 
?apply
sd_Returns <- apply(Data_Returns[,-1], 2, sd, na.rm = T)

# Budujemy Sharpe'a
sharpe <- (avg_Returns - avg_Risk_Free)/ sd_Returns
View(sharpe)

# 7.	Utwórz ramkę danych wskazującą do jakiego sektora przynależy dana akcja

sharpe_df <- data.frame(stock = names(sharpe), sharpe_value = sharpe)
View(sharpe_df)

# 8.	Do tabeli z wartościami wskaźnika Sharpa dodaj informację o sektorze

list_banks = list("ALR", "BHW", "BNP", "BOS", "GTN", "ING", "MBK", "MIL", "PEO", "PKO", "SAN", "SPL", "UCG")
list_energy = list("BDZ", "CEZ", "CLC", "ENA", "KGN", "MLS", "NVG", "OND", "PEN", "PEP", "PGE", "RAE", "TPE", "ZEP")
list_games = list("3RG", "11B", "ART", "BBT", "BCS", "BLO", "CDR", "CIG", "CRJ", "DGE", "GIF", "GOP", "HUG", "MOV", "PCF", "PLW", "RND", "SIM", "TEN", "ULG", "VVD")
list_media = list("AGO", "ATG", "CPL", "DIG", "GPP", "IMS", "KCI", "KPL", "IRQ", "MZA", "PGM", "PTW", "WPL")

# do tabeli nie mozna dodac listy wiec trzeba zamienic ja w wektor -> unlist()

banks_df <- data.frame(stock = unlist(list_banks), sector = "banks")
energy_df <- data.frame(stock = unlist(list_energy), sector = "energy")
games_df <- data.frame(stock = unlist(list_games), sector = "games")
media_df <- data.frame(stock = unlist(list_media), sector = "media")

#laczymy w jedna tabele informacje o sektorze 

sector_df <- rbind(banks_df, energy_df, games_df, media_df)

#polaczenie 2 tabel 

sector_sharpe <- merge(sharpe_df, sector_df, by = "stock")
View(sector_sharpe)

# 9.	Wykonaj test normalności
#chcemy zrobic test dla kazdej z grup, dla kazdego sektoru 
# autorzy Lilliefors (Kolmogorov-Smirnov)  zalozyli, ze hipoteza zerowa jest rozkladem normalnym 
# trzeba zawsze doczytac jaka w danym tescie jest hipoteza zerowa 

by(sector_sharpe$sharpe_value, sector_sharpe$sector, lillie.test)

# p-value > 0,05  <- nie ma podstaw do odrzucenia 

# 10.	Wykonaj test homogeniczności wariancji

bartlett.test(sector_sharpe$sharpe_value, sector_sharpe$sector)

# p-value > 0,05 czyli wariancje sa do siebie podobne <- nie ma podstaw do odrzucenia 

# 11.	Wykonaj testy ANOVA i TukeyHSD lub Kruskal-Wallis

anova <- aov(sector_sharpe$sharpe_value ~ sector_sharpe$sector)
View(anova)
summary(anova)

#p-value czyli Pr(>F)   jest < 0,05 wiec odrzucamy zalozenia zerowe
# czyli srednie nie sa rowne 

TukeyHSD(anova)
#analizujac wyniki p-value czyli p adj
#po danych widac, ze odrzucamy zalozenia gdzie p adj < 0,05 a zostawiamy jako srednie sa rowne dla media-banks i games-energy

install.packages("ggpplot2")
library(ggpplot2)

ggpplot(sector_sharpe, aes(x=sector, y=sharpe_value)) + geom_boxplot()
# wykres pudelkowy pokazuje mediany, pokazuje rozklad zmiennej

