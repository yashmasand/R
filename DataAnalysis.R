# 1) Baculum Length vs Other Baculum Measurements

library("ggplot2", lib.loc="~/R/win-library/3.4")
library("tidyverse", lib.loc="~/R/win-library/3.4")

Baculum <- read_excel("C:/Users/yashmasand/Downloads/Baculum.xlsx")
View(Baculum)
str(Baculum)

par(mfrow=c(2,4))

plot(Baculum$Baculum.Length,Baculum$Baculum.Weight, xlab= "Baculum Length (mm)", ylab=
"Baculum Weight (g)")

plot(Baculum$Baculum.Length,Baculum$Dist.Hgt, xlab= "Baculum Length (mm)", ylab= "Distal
Height (mm)")

plot(Baculum$Baculum.Length,Baculum$Dist.Wdth, xlab= "Baculum Length (mm)", ylab= "Distal
Weight (g)")

plot(Baculum$Baculum.Length,Baculum$Prox.Hgt, xlab= "Baculum Length (mm)", ylab=
"Proximal Height (mm)")

plot(Baculum$Baculum.Length,Baculum$Prox.Wdth, xlab= "Baculum Length (mm)", ylab=
"Proximal Width (mm)")

plot(Baculum$Baculum.Length,Baculum$Shaft.Hgt.Prox, xlab= "Baculum Length (mm)", ylab=
"Proximal Shaft Height (mm)")

plot(Baculum$Baculum.Length,Baculum$Shaft.Hgt.Mid, xlab= "Baculum Length (mm)", ylab=
"Middle Shaft Height (mm)")

plot(Baculum$Baculum.Length,Baculum$Shaft.Hgt.Dist, xlab= "Baculum Length (mm)", ylab=
"Distal Shaft Height (mm)")

mtext("Scatterplots of Baculum Length vs Other Baculum Measurements", outer=TRUE, font=2,
cex=1.3, line=-2.5)


# 2) Baculum Length vs Other Baculum Measurements with respect to Age Groups

library("ggplot2", lib.loc="~/R/win-library/3.4")
library("tidyverse", lib.loc="~/R/win-library/3.4")

BaculumAgeData <- read_excel("C:/Users/yashmasand/Downloads/Baculum.xlsx")
View(BaculumAgeData)
str(BaculumAgeData)
new_BaculumAgeData <- subset(BaculumAgeData, !is.na(BaculumAgeData$Age.group))
new_BaculumAgeData$Age.group <- as.factor(new_BaculumAgeData$Age.group)
new_BaculumAgeData$Age.group<-factor(new_BaculumAgeData$Age.group,
levels=c(1,2,3),labels=c("Yearlings", "Sub-adults", "Adults"))

Weight <- ggplot(data=new_BaculumAgeData) +
geom_point(mapping=aes(x=Baculum.Weight,y=Baculum.Length,color=Age.group)) + labs
(title= "Bac. Weight Vs Bac. Length", x="Baculum Weight (g)", y="Baculum Lenth (mm)")

p1 <-Weight+theme_classic()
Distal_Hgt <- ggplot(data=new_BaculumAgeData) +
geom_point(mapping=aes(x=Dist.Hgt,y=Baculum.Length,color=Age.group)) + labs (title= "Distal
Height Vs Bac. Length", x="Distal Height Length (mm)", y="Baculum Length (mm)")

p2 <-Distal_Hgt+theme_classic()
Distal_Width <- ggplot(data=new_BaculumAgeData) +
geom_point(mapping=aes(x=Dist.Wdth,y=Baculum.Length,color=Age.group)) + labs (title=
"Distal Width Vs Bac. Length", x="Distal Width Length (mm)", y="Baculum Length (mm)")

p3 <-Distal_Width+theme_classic()
Prox_Hgt <- ggplot(data=new_BaculumAgeData) +
geom_point(mapping=aes(x=Prox.Hgt,y=Baculum.Length,color=Age.group)) + labs (title=
"Proximal Height Vs Bac. Length", x="Proximal Height Length (mm)", y="Baculum Length
(mm)")

p4 <-Prox_Hgt+theme_classic()
Prox_Width <- ggplot(data=new_BaculumAgeData) +
geom_point(mapping=aes(x=Prox.Wdth,y=Baculum.Length,color=Age.group)) + labs (title=
"Proximal Width Vs Bac. Length", x="Proximal Width Length (mm)", y="Baculum Length (mm)")

p5 <-Prox_Width+theme_classic()
Shaft_Hgt_Prox <- ggplot(data=new_BaculumAgeData) +
geom_point(mapping=aes(x=Shaft.Hgt.Prox,y=Baculum.Length,color=Age.group)) + labs (title=
"Shaft Prox. Height Vs Bac. Length", x="Shaft Proximal Height Length (mm)", y="Baculum
Length (mm)")

p6 <-Shaft_Hgt_Prox+theme_classic()
Shaft_Hgt_Mid <- ggplot(data=new_BaculumAgeData) +
geom_point(mapping=aes(x=Shaft.Hgt.Mid,y=Baculum.Length,color=Age.group)) + labs (title=
"Shaft Height Mid Vs Bac. Length", x="Shaft Height Mid Length (mm)", y="Baculum Length
(mm)")

p7 <-Shaft_Hgt_Mid+theme_classic()
Shaft_Hgt_Distal <- ggplot(data=new_BaculumAgeData) +
geom_point(mapping=aes(x=Shaft.Hgt.Dist,y=Baculum.Length,color=Age.group)) + labs (title=
"Shaft Height Distal Vs Bac. Length", x="Shaft Height Distal Length (mm)", y="Baculum Length
(mm)")

p8 <-Shaft_Hgt_Distal+theme_classic()
figure<-ggarrange(p1, p2, p3, p4, p5, p6, p7, p8 + rremove("x.text"),ncol = 4, nrow =
2,common.legend = TRUE, legend = "bottom")

annotate_figure(figure, top = text_grob("Scatterplots of Baculum Length vs Other Baculum
Measurements for 3 Age Groups ", color = "red", face = "bold", size = 14))


# 3) Baculum Length vs Standard Body Length

library("ggplot2", lib.loc="~/R/win-library/3.4")
library("tidyverse", lib.loc="~/R/win-library/3.4")
library("modelr", lib.loc="~/R/win-library/3.4")
library("purrr", lib.loc="~/R/win-library/3.4")
library(readxl)

Baculumx <- read_excel("C:/Users/yashmasand/Downloads/Baculumx.xlsx")
View(Baculumx)
grid<-Baculumx %>%data_grid(x)
grid
Baculumx_mod<-lm(y ~ x, data=Baculumx)
coef(Baculumx_mod)
grid<-Baculumx%>%data_grid(x)
grid
summary(Baculumx_mod)grid <- Baculumx%>%data_grid(x)
grid
grid <- grid %>%add_predictions(Baculumx_mod)
grid

ggplot(Baculumx, aes(x)) +
geom_point(aes(y=y)) +
geom_line(aes(y=pred), data=grid, color = "red", size = 1)

Baculumx <-Baculumx %>%add_residuals(Baculumx_mod)
Baculumx
ggplot(Baculumx, aes(resid)) +
geom_freqpoly(binwidth = 0.5)
ggplot(Baculumx, aes(x, resid)) +
geom_ref_line(h=0) +
geom_point()
par(mfrow=c(2,2))
plot(Baculumx_mod)
summary(Baculumx_mod)
