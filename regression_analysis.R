################################## 
########## Inicializace ##########
##################################
library(MASS)
library(lmtest)
library(nortest)
library(corrplot)
library(car)
library(rpart)
library(effects)

#fix(Boston)
head(Boston)
?Boston

load.libraries <- c('data.table','car','MASS','ggplot2','ISLR','graphics','effects','lattice')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

d <- data.frame(Boston)
attach(d)

################################## 
############## Q1 ################
##################################
# Dataset chybejici hodnoty neobsahuje
apply(d, 2, function(x) sum(is.na(x)))
dim(d)
summary(d)
head(d)
# Vydime, ze promenne chas a rad jsou nespojite a vhodne pro faktorizaci
d$chas = factor(d$chas)
# rad je ovsem ordinalni faktor - je zcela v poradku ho nechat spojity
# d$rad = factor(d$rad, ordered=T)
summary(d)

################################## 
############## Q2 ################
##################################
# Histogram a odhad hustoty pro odezvu medv
hist(d$medv, breaks=20, col='red', prob=T)
lines(density(d$medv), col="blue", lwd=2)

################################## 
############## Q3 ################
##################################
# Vykreslene zavislosti promennych na odezve prolozene krivkami
# Odkomentovat pro ostraneni outlieru:
# cc <- c(seq(162,167,1),seq(369,373,1),258,268)
# d = d[-cc,]

plot(d$crim, d$medv, main='odezva na kriminalitu na hlavu')
lines(lowess(d$crim, d$medv), col='red')

plot(d$nox, d$medv, main='odezva na koncentraci NO')
lines(lowess(d$nox, d$medv), col='red')

plot(d$rm, d$medv, main='odezva na pocet mistnosti')
lines(lowess(d$rm, d$medv), col='red')

plot(d$lstat, d$medv, main='odezva na procento nizsiho socialniho statutu')
lines(lowess(d$lstat, d$medv), col='red')

plot(d$ptratio, d$medv, main='odezva na pomeru zaku na ucitele')
lines(lowess(d$ptratio, d$medv), col='red')

plot(d$dis, d$medv, main='odezva na vzdalenost ke prumyslovym centrum')
lines(lowess(d$dis, d$medv), col='red')


################################## 
############## Q4 ################
##################################
# Boxplot promennych chas a rad
boxplot(medv~chas, data=d, main="", xlab="chas", ylab="medv", main='odezva na sousednost s rekou')
# Odezva na rad==24 se vyrazne lisi od rad<24
# (Vsimneme si take potencialnich outlieru medv==50)
boxplot(medv~rad, data=d, main="", xlab="rad", ylab="medv", main='odezva na pristupnost k dalnicim')
# Pocet datovych bodu pro jednotlive hodnoty - rad==24 zastoupena dostatecne
table(d$rad)
table(d$chas)
# Rozdelime pomoci funkce cut
boxplot(medv~cut(rad, breaks=c(0,23,24), labels=c("0","1")), data=d, main="", xlab="rad", ylab="medv")

################################## 
############## Q5 ################
##################################
# Nektere pozorovatelne promenne mohou byt zavisle - proto si vykreslime zavislosti mezi nimi (faktory pro tento ucel vynechame)
pairs(d[, !(colnames(d) %in% c("chas","rad"))])
# Ze stejneho duvodu nas zajimaji napriklad korelace
corrplot(cor(d[, !(colnames(d) %in% c("chas","rad"))]), method="number")

# Existuje linearni vztah mezi promennymi po log-transformaci?
plot(log(crim), medv)
plot(log(crim), log(medv))

# Hodnoty promennych tax a zn jsou velmi nerovnomerne rozdelene
table(d$tax)
table(d$zn)
hist(d$tax, nclass=1000)
hist(d$zn, nclass=1000)


################################## 
############## Q6 ################
##################################
# Jednoduchy model zavislosti ceny na kriminalite
# Neni duvod se domnivat, ze by regresni primka mela prochazet pocatkem - intercept je nenulovy
lm0 <- lm(medv ~ crim, data=d)
# Model vysvetluje pouze priblizne 15% rozptylu - je velmi nekvalitni
summary(lm0)

# Rezidua jsou heteroskedasticka, nejsou normalne rozdelena, vydime nekolik spatnych leverage pointu
par(mfrow=c(2,2)); plot(lm0); 
dev.off();

# Vykreslena regresni primka
plot(medv ~ crim, data=d)
lines(fitted(lm0) ~ crim, data = d)

# Graf standardizovanych rezidui naznacuje heteroskedasticitu (absolutni hodnota residui misty roste s hodnotou predikovane ceny)
# Mame ale nepomerne mnozstvi datovych bodu pro nizke hodnoty kriminality (nabizi se logaritmicka transformace)
plot(rstandard(lm0) ~ fitted(lm0))
abline(0, 0)
spreadLevelPlot(lm0)

# To muze byt duvod, proc statisticky test hypotezu homoskedasticity nezamita
bptest(lm0)
# ncvTest(lm0)

# Histogram residui take naznacuje, ze nejsou normalne rozdelena ("heavy tail")
hist(residuals(lm0), breaks=20)

# Tuto domnenku potvrzuje i pohled na QQ-plot a statisticke testy normality
qqnorm(residuals(lm0)); qqline(residuals(lm0), col = 2);
shapiro.test(residuals(lm0))
lillie.test(residuals(lm0))
ad.test(residuals(lm0))

################################## 
############## Q7 ################
##################################
# Logaritmicka transformace odezvy
# Vydime, ze promenna zustavaji signifikantni, std. chyba se snizila, podil vysvetlovaneho rozptylu se zvysil, F-statistika se zvysila, ...
# Zrejme se jedna o lepsi model (stale ale neprilis dobry)
lm1 <- lm(log(medv) ~ crim, data=d)
summary(lm1)

# Nemame heteroskedasticitu residui, stale nekolik spatnych leverage pointu
par(mfrow=c(2,2)); plot(lm1); 
dev.off();

# Vykreslime regresni primku
plot(log(medv) ~ crim, data=d); lines(fitted(lm1) ~ crim, data=d);

# Strucna analyza rezidui ale naznacuje, ze stale nejsou homoskedasticka
plot(rstandard(lm1) ~ fitted(lm1)); abline(0, 0);
bptest(lm1) # Homoskedasticita je nyni jednoznacne zamitnuta

# Statisticke testy normalitu zamitaji, ale QQ plot je vizualne temer linearni
hist(residuals(lm1), breaks=20)
qqnorm(residuals(lm1)); qqline(residuals(lm1), col = 2);
shapiro.test(residuals(lm1))

# Box-Cox
# Z log-verohodnostniho profilu je videt, ze lambda=0 lezi uvnitr 95% konfidencniho intervalu, tedy B-C logaritmickou transformaci pripousti jako vhodnou 
bc <- boxcox(medv ~ crim, data=d)
lambda <- bc$x[which.max(bc$y)] # navrhovana lambda=-0.1010101 (resp. 0.020202 bez outlieru)
# Std. chyba se snizila, R^2 a F-statistika naznacuji, ze tento model prestavuje mirne zlepseni vuci log. transformaci 
lmbc <- lm((medv^lambda-1)/lambda ~ crim, data=d)
summary(lmbc)
plot((medv^lambda-1)/lambda ~ crim, data=d); lines(fitted(lmbc) ~ crim, data=d);

# Predpoklad homoskedasticity residui stale neni splnen, ...
par(mfrow=c(2,2)); plot(lmbc); 
dev.off();
plot(rstandard(lmbc) ~ fitted(lmbc)); abline(0, 0)
bptest(lmbc) 

# a stale nejsou normalne rozdelena
hist(residuals(lmbc), breaks=20)
qqnorm(residuals(lmbc)); qqline(residuals(lmbc), col = 2);
shapiro.test(residuals(lmbc))

################################## 
############## Q8 ################
##################################
# Z posledniho vztahu v zadani dostavame exp(beta) = 1 + (E[Y|X=x+1] - E[Y|X=x])/(E[Y|X=x])
# Pri vzrustu crim o 1 jednotku se dle modelu lm1 medv tedy snizi o cca 2.5%
(1-exp(lm1$coefficients['crim']))*100

################################## 
############## Q9 ################
##################################
# Transformace promenne crim
# Vydime, ze lowess krivka ma 4 uzly - bez outlieru ale velmi nevyrazne - trasnformace do 3. radu by mely stacit
lm1.0 <- lm(log(medv) ~ crim,data=d); summary(lm1.0); crPlots(lm1.0);

# Zkusme log-transformovat nezavislou promennou
# Lowess krivka kopiruje regresni krivku - vztah je temer linearni
lm1.1 <- lm(log(medv) ~ log(crim),data=d); summary(lm1.1); crPlots(lm1.1);
# Vizualne se rezidua zdaji byt homoskedasticka a temer normalni
par(mfrow=c(2,2)); plot(lm1.1); 
dev.off();
# Testy vsak heteroskedasticitu i normalitu stale zamitaji
shapiro.test(residuals(lm1.1)); bptest(lm1.1);

s <- seq(0, 90)

# Polynomialni transformace 2. radu je pochopitelne velmi nevhodna
lm2.0 <- lm(log(medv) ~ I(crim^2), data=d); summary(lm2.0); crPlots(lm2.0);
plot(d$crim, log(d$medv)); lines(s, predict(lm2.0,data.frame(crim=s)), col = "red" , lwd = 2);
plot(I(d$crim^2), log(d$medv)); abline(lm2.0, col = "red" , lwd = 2);
# Residua
par(mfrow=c(2,2)); plot(lm2.0); 
dev.off();

hist(residuals(lm2.0), breaks=20, probability=T); lines(density(residuals(lm2.0)), col="blue", lwd=2);
qqnorm(residuals(lm2.0)); qqline(residuals(lm2.0), col = 2); shapiro.test(residuals(lm2.0));

# Stejne plati pro transformaci 3. radu
lm2.1 <- lm(log(medv) ~ I(crim^3)); summary(lm2.1); crPlots(lm2.1);
plot(d$crim, log(d$medv)); lines(s, predict(lm2.1,data.frame(crim=s)), col = "red" , lwd = 2);
plot(I(d$crim^3), log(d$medv)); abline(lm2.1, col = "red" , lwd = 2);
# Residua
par(mfrow=c(2,2)); plot(lm2.1); 
dev.off();

hist(residuals(lm2.1), breaks=20, probability=T); lines(density(residuals(lm2.1)), col="blue", lwd=2);
qqnorm(residuals(lm2.1)); qqline(residuals(lm2), col = 2); shapiro.test(residuals(lm2.1));

# Po castech konstantni transformace
pwc <- rpart(log(medv) ~ crim); summary(pwc);
plot(crim, log(medv)); lines(s, predict(pwc,data.frame(crim=s)), col = "red" , lwd = 2);
# Residua
hist(residuals(pwc), breaks=20, probability=T); lines(density(residuals(pwc)), col="blue", lwd=2);
qqnorm(residuals(pwc)); qqline(residuals(pwc), col = 2);
shapiro.test(residuals(pwc)); bptest(pwc);

# Po castech linearni transformace
# Tento model ma oproti log. transformaci mirne nizsi std. chybu, vyssi R^2 i f-stat.
z = 10
lm2.2 <- lm(log(medv) ~ crim + I((crim-z)*(crim>z)), data=d); summary(lm2.2); crPlots(lm2.2);
b.0 <- coef(lm2)[1]
b.1 <- coef(lm2)[2]
b.2 <- coef(lm2)[3]
x.0 <- seq(0,z,1)
x.1 <- seq(z,100,1)
y.0 <- b.0 + b.1 * x.0
y.1 <- (b.0 + b.1 * z + (b.1 + b.2)* x.1)
plot(d$crim,log(d$medv))
lines(x.0,y.0, col="red"); lines(x.1,y.1, col="blue");
# Rezidua se zdaji byt pomerne homoskedasticka a normalni, stale se ale vyskytuje par leverage pointu
par(mfrow=c(2,2)); plot(lm2.2); 
dev.off();

hist(residuals(lm2.2), breaks=20, probability=T); lines(density(residuals(lm2.2)), col="blue", lwd=2);
qqnorm(residuals(lm2.2)); qqline(residuals(lm2.2), col = 2);
# Homoskedasticita NENI zamitnuta!
shapiro.test(residuals(lm2.2)); bptest(lm2.2);

# Spline
# Lowess krivka ma 4 uzly
lms <- smooth.spline(d$crim, log(d$medv),nknots = 4); plot(log(d$medv) ~ d$crim); lines(lms, lwd = 2, col="red");

# Rezidua se vizualne zdaji byt homoskedasticka a normalni
hist(residuals(lms), breaks=20, probability=T); lines(density(residuals(lms)), col="blue", lwd=2);
qqnorm(residuals(lms)); qqline(residuals(lms), col = 2);
plot(residuals(lms) ~ fitted(lms)); abline(0, 0);
shapiro.test(residuals(lms));

# Zkusme experimentovat s ruznymi mocninami
lm2.3 <- lm(log(medv) ~ I(crim^(-0.001)),data=d); summary(lm2.3); crPlots(lm2.3);
par(mfrow=c(2,2)); plot(lm2.3);
dev.off();
shapiro.test(residuals(lm2.3)); bptest(lm2.3);

lm2.3 <- lm(log(medv) ~ I(crim^(0.01)),data=d); summary(lm2.3); crPlots(lm2.3);
par(mfrow=c(2,2)); plot(lm2.3);
dev.off();
shapiro.test(residuals(lm2.3)); bptest(lm2.3);

# Okolo exponentu 0.3 bptest nezamita homoskedasticitu, model vysvetluje temer polovinu rozptylu a ma relativne
# nizkou chybu
lm2.3 <- lm(log(medv) ~ I(crim^(0.3)),data=d); summary(lm2.3); crPlots(lm2.3);
par(mfrow=c(2,2)); plot(lm2.3);
dev.off();
shapiro.test(residuals(lm2.3)); bptest(lm2.3);

################################## 
############## Q10 ###############
##################################
# Vybereme model
lmc1 <- lm2.3
# Testy residui provedeny v ramci otazky Q9 
plot(log(medv) ~ I(crim^0.3),data=d); abline(lmc1, col="red", lwd=2);
# Vykreslime graf efektu
plot(allEffects(lmc1))
# Testy na rezidua - normalita je zamitnuta, ale homoskedasticita nikoliv
shapiro.test(residuals(lmc1))
lillie.test(residuals(lmc1))
ad.test(residuals(lmc1))
bptest(lmc1)
ncvTest(lmc1)
# Vizualizace rezidui - QQ-plot naznacuje, ze normalitu nam kazi nekolik extremnich hodnot
par(mfrow=c(2,2)); plot(lm2.3);
dev.off();
plot(residuals(lmc1)); abline(0, 0);
plot(rstudent(lmc1)); abline(0, 0);
plot(residuals(lmc1) ~ fitted(lmc1)); abline(0, 0);
hist(residuals(lmc1), breaks=20, probability=T); lines(density(residuals(lmc1)), col="blue", lwd=2);
qqnorm(residuals(lmc1)); qqline(residuals(lmc1), col = 2);

################################## 
############## Q11 ###############
##################################
# Opetovna inicializace
d <- data.frame(Boston)
attach(d)
# d$rad = factor(d$rad, ordered=T)
d$chas = factor(d$chas)
# Vidime, ze hodnota 50 se vyskytuje 16x - muze se jednat o chybu pri kolekci dat (defaultni hodnota), zaokrouhleni (max. dovolena hodnota), ...
table(d$medv)
hist(d$medv, nclass=300)
# Vypiseme si prislusne radky
d[d$medv==50,]
# Obzvlaste podezrele jsou radky s vyssi kriminalitou. Zaroven vidime, ze to jsou ty same radky, ktere maji
# vyssi hodnotu indus, nox, age a misty lstat, o kterych vime, ze jsou negativne korelovane s medv,
# a misty nizsi hodnotu rm, ktera je postivne korelovana s medv
cc <- c(seq(162,164,1), 167, seq(369,373,1),258,268)
# Vykreslime podezrele body na grafech zavislosti na odezve
plot(medv ~ indus); points(indus[cc], medv[cc], col = "red");
plot(medv ~ nox); points(nox[cc], medv[cc], col = "red");
plot(medv ~ age); points(age[cc], medv[cc], col = "red");
plot(medv ~ lstat); points(lstat[cc], medv[cc], col = "red")
plot(medv ~ rm); points(rm[cc], medv[cc], col = "red");
plot(medv ~ crim); points(crim[cc], medv[cc], col = "red");
# Nelze ucinit jednoznacny zaver. Zrejme jde o outliery vzhledem k rm, nox, indus, age a crim, ale napriklad
# lstat hodnoty jsou pomerne realne. Je mozne, ze jsou tyto pozemky drazsi z dat nevysvetlitelneho duvodu,
# napr. pekneho vyhledu. Pokud je to pravda, pak bychom tuto informaci meli do dat dodat.
# My tyto body odstranime.
d = d[-cc,]

################################## 
############## Q12 ###############
##################################
# V teto otazce nebereme v uvahu korelaci mezi promennymi, tou se zabyva otazka Q13
# Model s transformovanou odezvou je o neco lepsi z hlediska R^2, F stat., a ma podstatne mensi std. chybu
# Opet, odstranovat intercept nema smysl
lm3 <- lm(medv~(.), data=d); summary(lm3);
lm3l <- lm(log(medv)~(.), data=d); summary(lm3l);

# Nejdrive zkusime zkonstruovat model automaticky pomoci stepwise AIC
# Stejne jako vyse lze rict i o techto modelech
lm3s <- step(lm3); summary(lm3s)
lm3sl <- step(lm3l); summary(lm3sl)

# Zkusme model vybrat rucne - dale pouze s transformovanou odezvou
# zn, indus, age nejsou vyznamne, chas je na hranici vyznamnosti
lm3.10l <- lm(log(medv)~(.)-zn-indus-age, data=d); summary(lm3.10l);
anova(lm3.10l)
# Odstranenim promenne chas nesnizime pomer vysvetlovaneho rozptylu ani jeho velikost, navic zvysime F-stat.
lm3.11l <- lm(log(medv)~(.)-zn-indus-chas-age, data=d); summary(lm3.11l);
anova(lm3.11l)
# Za povsimnuti stoji napriklad, ze uroven znecisteni ma na odezvu vetsi vliv, nez napr. vzdalenost
# k pracovnim mistum - lide radeji budou dojizdet delsi vzdalenost, nez zit ve znecistene oblasti
# Zkusme binarizovat rad - model se jen nepatrne zlepsi
lm3.120l <- lm(log(medv) ~(.)-zn-indus-chas-age-rad+cut(rad, breaks=c(0,23,24), labels=c("0","1")), data=d); summary(lm3.120l)
# Nyni pouziti log(crim) model zhorsi
lm3.121l <- lm(log(medv) ~(.)-zn-indus-chas-age-crim+log(crim), data=d); summary(lm3.121l)
crPlots(lm3.121l)
# Zkusme odsranit promennou black - je nejmene vyznamna a vysvetluje pomerne malou cast rozptylu
# Jak by se dalo cekat, nepatrne se snizilo R^2 a zvysila std. chyba, ale efekt je temer zanedbatelny a mame 
# jednodussi (a vice politicky korektni) model
lm3.13l <- lm(log(medv)~(.)-zn-indus-chas-age-black, data=d)
summary(lm3.13l)
crPlots(lm3.13l);
# dis, rad a tax jsou dalsimi kandidaty na vyrazeni - navic dis a rad jsou silne korelovane - vyradime v Q13
anova(lm3.13l)

# Binarizace rad i odstraneni promenne black znatelne zvysi AIC
# Model bez promenne black ma vsak vyrazne vyssi hodnotu F-statistiky
# F statistika nam vsak jen urcuje 
# AIC bere v uvahu komplexitu modelu - zjednoduseni nevyvazilo ztratu informace z promenne black.
c(summary(lm3)$adj.r.squared, summary(lm3s)$adj.r.squared, summary(lm3sl)$adj.r.squared, summary(lm3.11l)$adj.r.squared, summary(lm3.120l)$adj.r.squared, summary(lm3.13l)$adj.r.squared)
c(summary(lm3)$fstatistic[1], summary(lm3s)$fstatistic[1], summary(lm3sl)$fstatistic[1], summary(lm3.11l)$fstatistic[1], summary(lm3.120l)$fstatistic[1], summary(lm3.13l)$fstatistic[1])
c(summary(lm3)$sigma, summary(lm3s)$sigma, summary(lm3sl)$sigma, summary(lm3.11l)$sigma, summary(lm3.120l)$sigma, summary(lm3.13l)$sigma)
AIC(lm3, lm3s, lm3sl, lm3.11l, lm3.120l, lm3.13l)
BIC(lm3, lm3s, lm3sl, lm3.11l, lm3.120l, lm3.13l)

# Na zaklade uvazovanych kriterii vybirame model lm3.11l.
lmc <- lm3.11l

# Validace
# Rezidua bohuzel stale nejsou heteroskedasticka - vydime nekolik outlieru
# To samozrejme neznamena, ze je model nepouzitelny, jen nemame zaruceno, ze je BLU
par(mfrow=c(2,2))
plot(lmc)
dev.off()

# Bohuzel stale nemame homoskedasticka residua
plot(residuals(lmc) ~ fitted(lmc)); abline(0, 0)
bptest(lmc)

# I normalitu testy bohuzel jednoznacne zamitaji
hist(residuals(lmc), breaks=20, probability=T); lines(density(residuals(lmc)), col="blue", lwd=2);
qqnorm(residuals(lmc))
qqline(residuals(lmc), col = 2)

shapiro.test(residuals(lmc))
lillie.test(residuals(lmc))
ad.test(residuals(lmc))


################################## 
############## Q13 ###############
##################################
# Jak lze ocekvat, rm je pomerne nekorelovana s ostatnimi promennymi (s vyjimkou lstat), ktere jsou primeji
# determinovane s konkretnim geografickym umistenim. Dale nox je pochopitelne korelovana se vzdalenosti
# k prumyslovym centrum, jako napriklad tovarnam emitujici NO. Velmi silna je korelace mezi rad a tax.
# Dalsi promenne mezi kterymi je korelace > 0.7: indus ~ nox, indus ~ -dis, nox ~ age, age ~ -dis, 
# ale age a indus jsme jiz z naseho modelu odstranili
cor(d)
corrplot(cor(d), method="number")
# Podivejme se na inflacni faktory dvou nejslibnejsich modelu
# V obou pripadech maji hodnotu > 4 silne korelovane promenne tax a rad
# Ma smysl je zkombinovat do jedne, nebo jednu odstranit
vif(lm3.10l)
vif(lmc)

# Zkusime odstranit promennou rad, tax, nebo obe, a nalezt nejlepsi model bez multikolinearity
lm3.21 <- lm(log(medv)~(.)-zn-indus-chas-age-rad, data=d); summary(lm3.21);
lm3.22 <- lm(log(medv)~(.)-zn-indus-chas-age-tax, data=d); summary(lm3.22);
lm3.23 <- lm(log(medv)~(.)-zn-indus-chas-age-rad-tax, data=d); summary(lm3.23);
c(summary(lm3.21)$adj.r.squared, summary(lm3.22)$adj.r.squared, summary(lm3.23)$adj.r.squared)
c(summary(lm3.21)$fstatistic[1], summary(lm3.22)$fstatistic[1], summary(lm3.23)$fstatistic[1])
c(summary(lm3.21)$sigma, summary(lm3.22)$sigma, summary(lm3.23)$sigma)
anova(lm3.21)
anova(lm3.22)
anova(lm3.23)
AIC(lm3.21, lm3.22, lm3.23)
# Modely maji srovnatelne R^2. Posledni ma nejvyssi F-stat., ale druhy ma nejnizsi AIC.
lmc2 <- lm3.22

################################## 
############## Q14 ###############
##################################
# Vliv kriminality na median ceny se priblizne 2.5x snizil vuci modelu lm1
lm1$coefficients[2] / lmc2$coefficients['crim']
# Opet pouzijeme vzorec ze zadani a exp(beta_k) = 1 + (E[Y|X_k=x+1] - E[Y|X_k=x])/(E[Y|X_k=x])
# Pri vzrustu crim o 1 jednotku se medv snizi o cca 0.99%
(1-exp(lmc2$coefficients['crim']))*100

################################## 
############## Q15 ###############
##################################
# Prezentace vysl. modelu
summary(lmc2)
avPlots(lmc2)
# CR plot promenne rm naznacuje nelinearni vztah
crPlots(lmc2)
########
# Transformace rm rezidua prilis nevylepsi
lmcp <- lm(log(medv)~(.)-zn-indus-chas-age-rad-tax-rm+I(rm^2), data=d); summary(lmcp); crPlots(lmcp);
# Transformace crim zlepsi rezidua, ale zhorsi model
lmcp <- lm(log(medv)~(.)-zn-indus-chas-age-rad-tax-crim+log(crim), data=d); summary(lmcp); crPlots(lmcp);
########

# Vydime, ze jen mala cast rozptylu je vysvetlovana promennou dis
anova(lmc2)
########
# Po jejim odstraneni ale ziskame nevyznamne promenne a po jejich odstraneni ziskame pomerne dobry jednoduchy model
lmcp <- lm(log(medv)~(.)-zn-indus-chas-age-rad-tax-dis-black-rm-nox, data=d)
summary(lmcp)
hist(residuals(lmcp), breaks=20, prob=T); lines(density(residuals(lmcp)), col="blue", lwd=2);
qqnorm(residuals(lmcp)); qqline(residuals(lmcp), col = 2);
crPlots(lmcp)
anova(lmcp)
AIC(lmcp)
########

# Mezi rezidui vydime nekolik potencialnich outlieru
par(mfrow=c(2,2))
plot(lmc2)
dev.off()

# Zadny z nich ale neni spatnym leverage pointem - jen zhorsuji homoskedasticitu, prilis neovlivnuji 
# regresni primku
ck <- cooks.distance(lmc2)
plot(ck)
sort(abs(residuals(lmc2)), decreasing=TRUE)[1:20]
points(c(413,402,401,490,400), ck[c(413,402,401,490,400)] , col = "red")

c(summary(lmc2)$adj.r.squared, summary(lmc2)$sigma, summary(lmc2)$fstatistic[1])

# Rezidua stale nejsou homoskedasticka ani normalni
hist(residuals(lmc2), breaks=20, prob=T); lines(density(residuals(lmc2)), col="blue", lwd=2);
qqnorm(residuals(lmc2)); qqline(residuals(lmc2), col = 2);
bptest(lmc2)

shapiro.test(residuals(lmc2))
lillie.test(residuals(lmc2))
ad.test(residuals(lmc2))

########
# Zahozenim promennych rm a black ziskame trochu homoskedastictejsi rezidua, ale mimo jine podstatne zhorsime std. chybu
lmcp <- lm(log(medv)~(.)-zn-indus-chas-age-rad-tax-rm-black, data=d); summary(lmcp); crPlots(lmcp);

par(mfrow=c(2,2)); plot(lmcp); 
dev.off();

hist(residuals(lmcp), breaks=20, prob=T); lines(density(residuals(lmcp)), col="blue", lwd=2);
qqnorm(residuals(lmcp)); qqline(residuals(lmcp), col = 2);
########

# Zkusme jeste provest Box-Coxovu transformaci
# 0 je v 95% konfidencnim intervalu, tedy log. transformace je pripustna
bc <- boxcox(medv~(.)-zn-indus-chas-age-tax, data=d)
lambda <- bc$x[which.max(bc$y)] # navrhovana lambda=0.2626
# Rezidua zrejme stale nejsou homoskedasticka ani normalni
lmbc <- lm((medv^lambda-1)/lambda ~ (.)-zn-indus-chas-age-tax, data=d)
summary(lmbc)
par(mfrow=c(2,2)); plot(lmbc); 
dev.off();


################################## 
############## Q16 ###############
##################################
# Diskuse
# Pro overeni jestli odstranena data jsou skutecne nevalidni by bylo vhodné zjistit, jestli maji podezrele body nejakou
# nemerenou vlastnost, ktera je cini drazsimi (vyhled, historicka / umelecka hodnota).
# Dale by bylo vhodne zjisti způsob sběru dat - byl vyplnovan formular s defaultni / maximalni hodnotou medv $50k,
# a muze jit o chybu pri zaznamu?
# Mohli bychom tyto hodnoty zanechat, a pridat promennou indikujici prislusnost mezi tyto podezrele hodnoty.
# Promenna rad je zrejme diskretni, ale jde o ordinalni faktor a je tedy mozne ho do modelu zadat jako spojitou
# promennou.
# Mohli bychom se pokusit zkombinovat silne korelovane promenne jako napr. dist a rad do jedne promenne.
# Pro vyber vhodnych transformaci by bylo treba rozsahle experimentace. Zkusil bych napriklad binarizovat
# promennou zn (zn==0, zn>0), indus (indus<8, indus>=8), tax (tax<500, tax>=500) (bimodalni distribuce), 
# log-transformovat lstat, pouzit kvadratickou transformaci na black.
# Pro presnejsi evaluaci modelu bychom mohli data rozdelit na tranovaci / testovaci mnozinu.

################################## 
############## Q17 ###############
##################################
# Otazka na vztah
# Ne nutne. Prestoze v nasich datech pozorujeme vztah mezi kriminalitou a medianem ceny nemovitosti,
# je mozne, ze tento vztah neni kauzalni - napr. nam neznamy faktor muze zpusobovat jak snizeni ceny,
# tak zvyseni kriminality.
# Ackoli tato moznost existuje, prislusne scenare jsou velmi umele. Je tedy duvod se domnivat, ze snizeni
# kriminality bude znamenat zvyseni poptavky a tedy i ceny. Pro overeni teto hypotezy je vsak treba dobre
# navrzene kontrolovane studie. 
# Dalsi spekulace a postrehy: 
# * Na zaklade naseho modelu lze ocekavat, ze kriminalita se na ceny nemovitosti projevi nerovnomerne - levnejsi
# pozemky po zvyseni kriminality zlevni mene nez drazsi.
# * V izolovanych pripadech muze zvyseni kriminality zpusobit zvyseni ceny pozemku, napr. kvuli nutnosti instalace
# bezpecnostnich zarizeni (mozne vysvetleni outlieru?)
# * Cilene potlaceni kriminality muze mit nezamyslene vedlejsi efekty zamezujici zvyseni ceny pozemku. Zvyseni dani
# muze snizit poptavku (a tedy cenu), nebo finance utracene za potlacovani mohou schazet v rozpoctu lokalnich
# zarizeni na udrzbu parku, detskych hrist, verejne dopravy, apod. (a tedy snizovat cenu).
# * Je treba mit na pameti, ze efekt na cenu by zdaleka nebyl okamzity - cena muze zustat stejna roky, dokud se
# nezmeni reputace oblasti. Finance nutne na potlaceni kriminality by bylo treba vyplacet po celou tuto dobu,
# ale naklady pravdepodobne budou klesat.
