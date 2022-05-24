####### pakiety #######
if (!("lmtest" %in% rownames(installed.packages()))) install.packages("lmtest")
if (!("tsoutliers" %in% rownames(installed.packages()))) install.packages("tsoutliers")
if (!("MuMIn" %in% rownames(installed.packages()))) install.packages("MuMIn")
library(tsoutliers)
library(lmtest)
library(MuMIn)

####### model ####### 
y <- c(5751174,5820763,5869959,6006608,6063721,6123726,6182136,6244730,6308344,6375734,6443611,6629920,6636883)
x1 <- c(174686,142901,158064,162200,141798,127392,148122,168403,173932,205990,221907,237281,223842)
x2 <- c(9.5,12.1,12.4,12.5,13.4,13.4,11.4,9.7,8.2,6.6,5.8,5.2,6.2)
x3 <- c(58407.1,62276,65428.2,70088.7,74687.5,79597.2,84695.3,90323.9,95255.2,99703.2,104910.1,111079.3,117238.9)

wynik <- lm(y~x1+x2+x3)

####### weryfikacja ####### 

# podsumowanie
summary(wynik)
plot(wynik)

# reszty
reszty <- resid(wynik)
acf(reszty)

JarqueBera.test(reszty)
shapiro.test(reszty)

BIC <- BIC(wynik)
AIC <- AIC(wynik)
AICC <- AICc(wynik)

effect_plot(wynik, pred = Illiteracy, interval = TRUE, plot.points = TRUE)

