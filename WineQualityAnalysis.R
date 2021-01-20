install.packages("tidyverse")
library(ggplot2)

install.packages("ggpubr")
library(ggpubr)

install.packages("propagate")
library (propagate)

install.packages("Hmisc")
library(Hmisc)

install.packages("corrplot")
library(corrplot)

install.packages("ggcorrplot")
library(ggcorrplot)


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
# rozklad symetryczny? moze normalny?


# histogram z naniesiona gestoscia oraz gestoscia rozkladu normalnego
ggplot(data = wine, aes(pH)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1/50, color = "darkblue", fill = c("#99CCFF")) +
  geom_density(size = 0.9, color = c("#000033")) + 
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

# histogram z naniesiona gestoscia oraz gestoscia rozkladu normalnego
ggplot(data = wine, aes(density)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1/4000, color = "darkblue", fill = c("#99CCFF")) +
  geom_density(size = 0.9, color = c("#000033")) + 
  stat_function(fun = dnorm, args = list(mean = mean(wine$density), sd = sd(wine$density)), color = "red", size = 0.7)





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


# Shapiro-Wilk’s Test:

# H0: rozklad zblizony do normalnego
# H1: rozklad niezblizony do normalnego
# jezeli p-wartosc > 0.05 (nieistotne statystycznie) => 
#     przyjmujemy H0 =>
#     rozklad zblizony do normalnego

shapiro.test(wine$pH)

# p-value = 1.712e-06,
# a wiec niestety nie mamy do czynienia z rokzladem normalnym


# a teraz zajmijmy sie gestoscia:


den_mean = mean(wine$density)
den_med = median(wine$density)
den_mode = Mode(wine$density)

cat("mean   ", den_mean, "\nmedian:", den_med, "\nmode   ", den_mode)

kurtosis(wine$density)
# # kurtoza > 0 => rozklad wysmukly (wartosci bardziej skoncentrowane niz przy 
#                rozkladzie normalnym)

# Shapiro-Wilk’s Test:
shapiro.test(wine$density)

# p-value = 1.936e-08 => nie jest to rozklad zblizony do normalnego


# Q-Q Plot - służy do porównywania rozkładu w próbie z wybranym 
#            rozkładem hipotetycznym – w tym wypadku rozkładem normalnym


# PH
ggplot(wine, aes(sample = pH)) + 
  stat_qq(color = c("#FF6633"), size = 1.5) +
  stat_qq_line(color = c("#6633CC"), size = 0.8) +
  ggtitle("Q-Q Plot") +
  theme(plot.title = element_text(hjust = 0.5))


# DENSITY
ggplot(wine, aes(sample = density)) + 
  stat_qq(color = c("#FF6633"), size = 1.5) +
  stat_qq_line(color = c("#6633CC"), size = 0.8) +
  ggtitle("Q-Q Plot") +
  theme(plot.title = element_text(hjust = 0.5))



# zauwazyc mozna ze rozklad pH jest bardzo zblizony do normalnego, jednak 
# rozni sie przez wartosci odstajace, ktorych polozenie znacznie odbiega od prostej



# sprawdzmy, jak wiele jest wartosci odstajacych pH oraz gestosci:

outliers <- function(x){
  length(which(x >  mean(x) + 3 * sd(x) | x < mean(x) - 3 * sd(x))  ) / length(x)
}

outliers(wine$density)
outliers(wine$pH)




# ----------------------------------------------------------------------
# KORELACJA R-PEARSONA - sluzy do sprawdzenia, czy zmienne ilosciowe sa
# powiazane zwiazkiem liniowym, jednak jest ona wrazliwa na wartosci odstajace


# wsp. korelacji oraz p-wartosc (poziom istotnosci)
res2 <- rcorr(as.matrix(wine))
res2
res2$r     # tylko wsp. korelacji
round(res2$P,2)     # tylko p-wartosci


# funkcja formatujaca macierz korelacji
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


# polaczenie correlogramu z testem istotnosci - za wynik 
# nieistotny statystycznie uznajemy taki, ze p-wartosc > 0.05
# (oznaczone jako puste pole na wykresie)

corrplot(res2$r, type="upper", order="hclust",
         tl.cex = 0.8,tl.col = "black", tl.srt = 45,
         p.mat = res2$P, sig.level = 0.05, insig = "blank",
         diag = FALSE)



# ladniejszy
corr <- cor(wine)
corr
pval <- cor_pmat(wine)
pval

ggcorrplot(corr, hc.order = TRUE, type = "upper", lab = TRUE,
           colors = c("#2166AC", "white", "#CC0000"), p.mat = pval, 
           sig.level = 0.05, insig = "blank", ggtheme = ggplot2::theme_gray)



# TODO:
# KORELACJA SPEARMANA - lepsza, w przypadku wartosci odstajacych



# skupmy sie na tym, od czego w glownej mierze zalezy jakosc wina
# jak widzimy na wykresie, najwieksza korelacja (liniowa) wystepuje kolejno 
# dla: alcohol, votalite.acidity, sulphates, citric.acid, etc.
# mozemy zatem przeanalizowac regresje

# REGRESJA LINIOWA

# scatter plot?

ggplot(wine, aes(x=quality, y=alcohol)) + 
  geom_point(color = c("#6633CC")) + 
  geom_smooth(method = "lm", color = "#CC99FF")


# duza korelacja takze pomiedzy: 

# ph - fixed.acidity
ggplot(wine, aes(x=pH, y=fixed.acidity)) + 
  geom_point(color = c("#6633CC")) + 
  geom_smooth(method = "lm", color = "#CC99FF")


# total.sulfur.dioxide - free.sulfur.dioxide
ggplot(wine, aes(x=total.sulfur.dioxide, y=free.sulfur.dioxide)) + 
  geom_point(color = c("#6633CC")) + 
  geom_smooth(method = "lm", color = "#CC99FF")


# citric.acid - fixed.acidity
ggplot(wine, aes(x=citric.acid, y=fixed.acidity)) + 
  geom_point(color = c("#6633CC")) + 
  geom_smooth(method = "lm", color = "#CC99FF")


# fixed.acidity - density
ggplot(wine, aes(x=fixed.acidity, y=density)) + 
  geom_point(color = c("#6633CC")) + 
  geom_smooth(method = "lm", color = "#CC99FF")

# alcohol - density
ggplot(wine, aes(x=alcohol, y=density)) + 
  geom_point(color = c("#6633CC")) + 
  geom_smooth(method = "lm", color = "#CC99FF")

  
  
# TODO: Adding a trend line to a scatter plot 
# https://bio304-class.github.io/bio304-fall2017/ggplot-bivariate.html
