install.packages("tidyverse")
library(ggplot2)

install.packages("ggpubr")
library(ggpubr)

install.packages("propagate")
library (propagate)

# TODO: 
#zmien sciezke xdd, 
# kodowanie polskich znakow, 
# czyszczenie danych?,
# przedzial ufnosci


wine <- read.csv("D:/studia/RPIS/projekt/winequality.csv", sep=";")


# brakujace dane
anyNA(wine)
# FALSE - no missing values


# wykres slupkowy przedstawiajacy licznosc poszczegolnych grup wina,
# ze wzgledu na jego jakosc - jest ona okreslona jako liczba calkowita
# z przedzialu [0,10], im wysza ocena, tym lepsze jakosciowo jest wino

counts <- table(wine$quality)
bp <- barplot(counts, main="Bar plot of wine quality", 
        xlab = "Quality",
        ylab = "Frequency",
        ylim=c(0,800),
        col = c("#6699FF"))
abline(h=0)
text(bp, counts+50,labels=round(counts,digits=2))

# TODO: moze ladniejszy wykres (np. ggplot)

# zauwazamy, ze najbardziej liczna jest grupa win o jakosci 5 (w skali 0-10)




# odchylenie standardowe - miara rozrzutu wokol sredniej
sd(wine$alcohol)

summary(wine)
# mozna zauwazyc 


# boxplot dla kazdej ze zmiennych
# rozklad danych oparty na: 
#   "min", kwartyl dolny(Q1), mediana(Q2), kwartyl górny(Q3), "max",
#   gdzie "min" = Q1 - 1.5*IQR, "max" = Q3 + 1.5*IQR, IQR = Q3 - Q1
#   (rozstep miedzykwartylowy)

# brzydki boxplot
boxplot(wine$quality,
        horizontal = TRUE,
        xlab = "Quality")


# ladny boxplot

# 1. QUALITY

# boxplot
ggplot(data = wine, mapping = aes(y = quality)) + 
    geom_boxplot(width = 1.2, fill = c("#CC99FF"), outlier.size = 2, 
                 alpha = 0.5, size = 0.6) +
    ylab("Quality") + coord_flip() + stat_boxplot(geom = 'errorbar') 



# 2. PH

# boxplot
ggplot(data = wine, mapping = aes(y = pH)) + 
  geom_boxplot(width = 1.2, fill = c("#CC99FF"), outlier.size = 2, 
               alpha = 0.5, size = 0.6) +
  ylab("pH") + coord_flip() + stat_boxplot(geom = 'errorbar')
# rozklad symetryczny?


# histogram z naniesiona gestoscia oraz gestoscia rozkladu normalnego
ggplot(data = wine, aes(pH)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1/50, color = "darkblue", fill = c("#99CCFF")) +
  geom_density(alpha = 0.2, size = 0.9, color = c("#000033")) + 
  stat_function(fun = dnorm, args = list(mean = mean(wine$pH), sd = sd(wine$pH)), color = "red", size = 0.7)




# 3. ALCOHOL

# boxplot
ggplot(data = wine, mapping = aes(y = alcohol)) + 
  geom_boxplot(width = 1.2, fill = c("#CC99FF"), outlier.size = 2, 
               alpha = 0.5, size = 0.6) +
  ylab("Alcohol") + coord_flip() + stat_boxplot(geom = 'errorbar')
# skosnosc prawostronna? (lekko xd)


# 4. DENSITY

# boxplot
ggplot(data = wine, mapping = aes(y = density)) + 
  geom_boxplot(width = 1.2, fill = c("#CC99FF"), outlier.size = 2, 
               alpha = 0.5, size = 0.6) +
  ylab("Density") + coord_flip() + stat_boxplot(geom = 'errorbar')
# rozklad symetryczny? 


# TODO: reszta boxplotów


# obserwacja: wiekoszosc badanych cech ma wiele wartosci odstajacych
# zauwazyc mozna, ze pH oraz alkohol maja rozklad symetryczny
# by sprawdzic, czy jest to rozklad normalny (a nie plato- lub leptokurtyczny)
# mozemy posluzyc sie obliczeniem kolejno sredniej arytm., mediany oraz mody  

# wiemy, ze rozklad dla rozkladu normalnego mamy jedno maksimum, kurtoza wynosi 0,
# w punkci centralnym znajduja sie srednia arytm., mediana, moda (dominanta)
# zatem sprawdzmy je:

# funkcja wyliczajaca mode
Mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# sprawdzmy zatem rozklad pH:

ph_mean = mean(wine$pH)
ph_med = median(wine$pH)
ph_mode = Mode(wine$pH)

cat("mean   ", ph_mean, "\nmedian:", ph_med, "\nmode   ", ph_mode)


# kurtoza - miara splaszczenia wartosci rozkladu cechy
kurtosis(wine$pH)

# kurtoza > 0 => rozklad wysmukly (wartosci bardziej skoncentrowane niz przy 
#                rozkladzie normalnym)

# dla pewnosci uzyjmy testu
# TODO: test   http://www.sthda.com/english/wiki/normality-test-in-r




# KORELACJA R-PEARSONA - sluzy do sprawdzenia, czy zmienne ilosciowe sa
# powiazane zwiazkiem liniowym

# wsp. korelacji miedzy parami zmiennych (correlation matrix)
cor.test(wine$alcohol, wine$pH)
res <- cor(wine)   
round(res,2)


# wsp. korelacji oraz p-wartosc (poziom istotnosci)
install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(wine))
res2
res2$r     # tylko wsp. korelacji
round(res2$P,2)     # tylko p-wartosci


# funkcja formatujaca powyzsza macierz
# 4 kolumny: nazwa wiersza, nazwa kolumny, wsp. korelacji, p-wartosc

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(res2$r, res2$P)


# correlogram - graficznie przedstawiona macierz korelacji
install.packages("corrplot")

# type = "upper" - odpowiada za wywietlenie tylko macierzy górnej trójkatnej
# order = "hlust" - uporzadkowanie macierzy wg. wspolczynnika korelacji
#                   "hierarchical clustering order"
library(corrplot)
corrplot(res, type = "upper", order = "hclust", tl.cex = 0.8,
         tl.col = "black", tl.srt = 45)


# polaczenie correlogramu z testem istotnosci - za wynik 
# nieistotny statystycznie uznajemy taki, ze p-wartosc <= 0.05
# (oznaczone jako puste pole na wykresie)


corrplot(res2$r, type="upper", order="hclust",
         tl.cex = 0.8,tl.col = "black", tl.srt = 45,
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


# jeszcze inne przedstawienie macierzy korelacji
# na przekatnej rozklad kazdej ze zmiennych
# pod przekatna dwuwymiarowe wykresy punktowe
# nad przekatna wspolczynnik korelacji (liczba) oraz poziom istotnosci (gwiazdki)
