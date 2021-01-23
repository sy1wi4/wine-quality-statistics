install.packages("tidyverse")
install.packages("ggpubr")
install.packages("propagate")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("dplyr")

library(ggplot2)
library(ggpubr)
library (propagate)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(dplyr)



# TODO: 
#zmien sciezke xdd, 
# przedzial ufnosci

wine <- read.csv("D:/studia/RPIS/projekt/winequality.csv", sep=";")


# poszczegolne zmienne w naszym datasecie:
# - fixed.acidity
# - volatile.acidity     (zawartosc kwasu octowego, wysoka odpowiada za nieprzyjemny, octowy posmak)
# - citric.acid          (zawartosc kwasu cytrynowego, w malych ilosciach dodaje "swiezosci")
# - residual.sugar       (zawartosc cukru pozostalego po zakonczeniu fermentacji, > 45g/l => wino uznawane za slodkie)
# - chlorides            (zawartosc soli)
# - free.sulfur.dioxide  (wolny dwutlenek siarki (SO2), zapobiega rozwojowi drobnoustrojow i utlenianiu wina)
# - total.sulfur.dioxide (wolny i zwiazany SO2 lacznie, w malych ilosciach niewykrywalny)
# - density              (gestosc, zblizona do gestosci wody, zalezna od zawartosci alkoholu oraz cukru)
# - pH                   (skala zasadowosci/kwasowosci - od 0 (bardzo kwasowy) do 14 (bardzo zasadowy); wino zazwyczaj 3-4 pH)
# - sulphates            (siarczany - dodatek, mogacy przyczyniac sie do wzrostu zawartosci SO2, dziala przeciwbakteryjnie)
# - alcohol              (zawartosc procentowa alkoholu)
# - quality              (jakosc wina, w skali [0-10])


# moznaby spodziewac sie m.in., ze im wieksza zawartosc kwasu octowego (volatile.acidity, tym jakosc wina nizsza)
# co najbardziej wplywa na ogolna jakosc wina?
# czy zalezy to od wielu czynnikow?
# czy istnieje jakas liniowa zaleznosc?
# czy ktoras ze zmmiennych ma ciekawy rozklad?



# brakujace dane?
anyNA(wine)
# FALSE - no missing values


# wykres slupkowy przedstawiajacy licznosc poszczegolnych grup wina,
# ze wzgledu na jego jakosc - jest ona okreslona jako liczba calkowita
# z przedzialu [0,10], im wysza ocena, tym lepsze jakosciowo jest wino


data <- as.data.frame(table(wine$quality))
data
ggplot(data, aes(x=Var1, y =Freq)) + geom_bar(stat = "identity", fill = c("#66CC99")) +
  geom_text(aes(label = Freq), nudge_y = 20) + xlab("quality") + ylab("count")


# zauwazamy, ze najbardziej liczna jest grupa win o jakosci 5 (w skali 0-10)
# najnizsza wystepujaca jakosc wina to 3, najwyzsza zas - 8 (18 win najwyzszej jakosci)
# wina "Sredniej" jakosci (5/6) wystepuja najczesciej, duzo rzadziej wina wysokiej i niskiej 
# jakosci (najrzadziej niskiej)


# odchylenie standardowe - miara rozrzutu wokol sredniej
sd(wine$alcohol)

summary(wine)

# boxplot dla kazdej ze zmiennych
# rozklad danych oparty na: 
#   "min", kwartyl dolny(Q1), mediana(Q2), kwartyl górny(Q3), "max",
#   gdzie "min" = Q1 - 1.5*IQR, "max" = Q3 + 1.5*IQR, IQR = Q3 - Q1
#   (rozstep miedzykwartylowy)



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

# histogram
ggplot(data = wine, aes(alcohol)) + 
  geom_histogram( color = "darkblue", fill = c("#99CCFF"), binwidth = 1/5)

# najczesciej wystepujaca zawartosc alkoholu oscyluje miedzy 9.5 a 11.1 [%]
# im wieksza zawartosc wina, tym rzedziej ono wystepuje



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



# 5. SULPHATES

# boxplot
ggplot(data = wine, mapping = aes(y = sulphates)) + 
  geom_boxplot(width = 1.2, fill = c("#CC99FF"), outlier.size = 2, 
               alpha = 0.5, size = 0.6) +
  ylab("Sulphates") + coord_flip() + stat_boxplot(geom = 'errorbar')

# rozklad jakby eksponencjalny???

# histogram z naniesiona gestoscia oraz gestoscia rozkladu wykladniczego
ggplot(data = wine, aes(sulphates)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1/50, color = "darkblue", fill = c("#99CCFF")) +
  geom_density(size = 0.9, color = c("#000033")) 



# 6. RESIDUAL SUGAR

# histogram
ggplot(data = wine, aes(residual.sugar)) + 
  geom_histogram( color = "darkblue", fill = c("#99CCFF"), binwidth = 1/4) 



# 7. CITRIC ACID

# histogram
ggplot(data = wine, aes(citric.acid)) + 
  geom_histogram( color = "darkblue", fill = c("#99CCFF"), binwidth = 1/100)

# OBS: na wykresie widac 3 interesujace maksima lokalne w okolicy 0, 0.25 oraz 0.5 [g] 
# co ma na to wplyw?



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
# kurtoza > 0 => rozklad wysmukly (wartosci bardziej skoncentrowane niz przy 
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
# rozklad density odbiega jeszcze bardziej



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
# dla: alcohol, volatile.acidity, sulphates, citric.acid, etc.


# spojrzmy jak zmienia sie zawartosc alkoholu wraz ze wzrostem jakosci :

ggplot(data = wine, mapping = aes(x = as.factor(quality), y = alcohol)) + 
  geom_boxplot(fill = c("#CC99FF"), outlier.size = 2, alpha = 0.5, size = 0.6) +
  ylab("Alcohol") + xlab("Quality") + stat_boxplot(geom = 'errorbar') 


# zauwazyc mozna, ze im wyzsza jakosc wina, tym wieksza mediana zawartosci alkoholu,
# wyjatekiem jest wino jakosci 5, tam tez pojawia sie spora ilosc wartosci odstajacych



# przeanalizujmy jeszcze zawartosc kwasu octowego (volatile.acidity), by sprawdzic,
# czy poczatkowe przypuszczenie, jakoby wieksza jego zawartosc powodowala spadek 
# jakosci, jest sluszne

ggplot(data = wine, mapping = aes(x = as.factor(quality), y = volatile.acidity)) + 
  geom_boxplot(fill = c("#CC99FF"), outlier.size = 2, alpha = 0.5, size = 0.6) +
  ylab("Volatile acidity") + xlab("Quality") + stat_boxplot(geom = 'errorbar') 

# widac wyraznie, ze im wieksza zawartosc kwasu octowego w winie, tym gorsza
# jest jego jakosc - co zgadza sie z przypuszczeniami


# przeanalizujmy regresje


# REGRESJA LINIOWA


# quality - alcohol
ggplot(wine, aes(x=quality, y=alcohol)) + 
  geom_point(color = c("#6633CC")) + 
  geom_smooth(method = "lm", color = "#CC99FF")

# alcohol - density
ggplot(wine, aes(x=alcohol, y=density)) + 
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



# stosunkowo duza korelacja (ujemna) wystepuje pomiedzy zawartoscia kwasu octowego
# a kwasu cytrynowego (-0.55), a takze miedzy kazdym z nich oraz jakoscia wina

# sprawdzmy jak mediana (ktora odporna jest na wartosci odstajace) zawartosci 
# kazdego z powyzszych kwasow zmienia sie wraz ze wzrostem jakosci wina

ggplot(wine, aes(quality, citric.acid)) +
  stat_summary(fun.y = median, color = "lightsalmon", geom = "line", size =1) 
  
ggplot(wine, aes(quality, volatile.acidity)) +
  stat_summary(fun.y = median, color = "lightsteelblue4", geom = "line", size =1)


# polaczmy w jeden wykres:


# grupujemy wiec wg jakosci wina i obliczamy mediane dla zawartosci kwasow
# w kazdej z grup
by_quality <- wine %>% 
  group_by(quality)%>%
  summarise(cit_med = median(citric.acid), vol_med = median(volatile.acidity), n = n())
  


ggplot() + 
  geom_line(data = by_quality, aes(x = quality, y = cit_med, color = "lightsalmon"), size = 1) + 
  geom_line(data = by_quality, aes(x = quality, y = vol_med, color = "lightsteelblue4"), size = 1) +
  ylab("acid") + scale_color_discrete(name = "acids:", labels = c("citric", "volatile"))


# OBS:
# wraz ze wzrostem jakosci wina, do pewnego momentu wzrasta zawartosc kwasu cytrynowego 
# ("swiezosc"), natomiast maleje zawartosc kwasu octowego (octowy posmak)
# im wyzsza jakosc, tym roznica miedzy zawartoscia kwasow maleje
# zauwazmy, ze wino najwyszej jakosci (7/8) posiadaja podobna ilosc obu z tych kwasow 



# bardzo mala korelacja pomiedzy np. free.sulfur.dioxide - sulphates

# zobaczmy wykres:

ggplot(wine, aes(x=free.sulfur.dioxide, y=sulphates)) + 
  geom_point(color = c("#6633CC")) + 
  geom_smooth(method = "lm", color = "#CC99FF")
# zdecydowanie brak tutaj jakiejkolwiek liniowej zaleznosci



 
# najwieksze korelacje ma fixed.acidity (3 x powyzej 0.6)
# (citric.acid, pH, density)



# TODO: dopasowanie rozkladu???
# beznadzieja : ))))

install.packages("fitdistrplus")
library(fitdistrplus)

descdist(wine$alcohol, discrete = FALSE)

descdist(wine$pH, discrete = FALSE)

descdist(wine$density, discrete = FALSE)

descdist(wine$density, discrete = TRUE)

fitdist(wine$alcohol, "beta")
