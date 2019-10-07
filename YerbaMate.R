setwd("C:/Users/Lechu/Desktop/DiscoPosMorten/Trabajillos2019/YerbaMate")
#install.packages("car")
#install.packages("agricolae")
library(agricolae)
library(car)

datos <- read.csv("C:/Users/Lechu/Desktop/DiscoPosMorten/Trabajillos2019/YerbaMate/datos.csv")

plot(datos[,4:8])
names(datos)
# Resumen ----
# Genero una base de datos resumen de las variables con Media y Desvio
res<- aggregate(Viruta~Tratamiento+Bloque, datos, mean); res
res1<- cbind(res,(aggregate(Viruta~Tratamiento+Bloque, datos, sd))[,3])
res2 <- cbind(res1,(aggregate(Copa~Tratamiento+Bloque, datos, mean))[,3])
res3 <- cbind(res2,(aggregate(Copa~Tratamiento+Bloque, datos, sd))[,3])
res4 <- cbind(res3,(aggregate(Total~Tratamiento+Bloque, datos, mean))[,3])
res5 <- cbind(res4,(aggregate(Total~Tratamiento+Bloque, datos, sd))[,3])
res6 <- cbind(res5,(aggregate(Diam_copa~Tratamiento+Bloque, datos, mean))[,3])
res7 <- cbind(res6,(aggregate(Diam_copa~Tratamiento+Bloque, datos, sd))[,3])
res8 <- cbind(res7,(aggregate(N_rama~Tratamiento+Bloque, datos, mean))[,3])
res9 <- cbind(res8,(aggregate(N_rama~Tratamiento+Bloque, datos, sd))[,3])
dim(res9)
colnames(res9) <- c("Tratamiendo", "Repeticion", "Viruta", "Desvio", "Copa", "Des_Copa", "Total", "Des_total","DiametroCopa", "DesvioDiam", "NRama", "DesvioNrama"); res9
write.csv(res9, "resumen.csv")
datos2 <- read.csv("C:/Users/Lechu/Desktop/DiscoPosMorten/Trabajillos2019/YerbaMate/resumen.csv")

# Graficos de cajas y bigotes ----
#install.packages("extrafont")
#install.packages("ggpubr")
require(ggplot2);library(extrafont);library(ggpubr)

# Grafico viruta ----
trata <- c("T0","T1","T2","T3","T4","T5")
trata <- c(paste0("T",0:5)) #Pregunta
p1 <- ggplot(datos,aes(Tratamiento, Viruta))+
  geom_boxplot(aes(fill=Tratamiento))+
  xlab("Tratamientos")+
  ylab("Gramos [g]")+
  scale_x_discrete(labels=trata)+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme( # tama?o de la letra y tipos de letras
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family="Times New Roman", size=12, face="bold"),
    axis.title.x = element_text(family="Times New Roman",size=12, face="bold"),
    axis.title.y = element_text(family="Times New Roman",size=12, face="bold"),
    legend.position = "none"
  ); p1
p2 <- ggplot(datos,aes(Tratamiento, Copa,  fill=Tratamiento))+
  geom_boxplot()+
  xlab("Tratamientos")+
  ylab("Gramos [g]")+
  scale_x_discrete(labels=trata)+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme( # tama?o de la letra y tipos de letras
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family="Times New Roman", size=12, face="bold"),
    axis.title.x = element_text(family="Times New Roman",size=12, face="bold"),
    axis.title.y = element_text(family="Times New Roman",size=12, face="bold"),
    legend.position = "none"
  ); p2

p3 <- ggplot(datos,aes(Tratamiento, Total,  fill=Tratamiento))+
  geom_boxplot()+
  xlab("Tratamientos")+
  ylab("Gramos [g]")+
  scale_x_discrete(labels=trata)+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme( # tama?o de la letra y tipos de letras
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family="Times New Roman", size=14, face="bold"),
    axis.title.x = element_text(family="Times New Roman",size=12, face="bold"),
    axis.title.y = element_text(family="Times New Roman",size=12, face="bold"),
    legend.position = "none"
  ); p3
#Unir dos graficos ggplot
theme_set(theme_pubr())

ggarrange(
  p3,# First row with line plot
  # Second row with box and dot plots
  ggarrange(p1, p2, ncol = 2, labels = c("B", "C")),
  nrow = 2,
  labels = "A"       # Label of the line plot
)
names(datos)
p4 <- ggplot(datos,aes(Tratamiento, Diam_copa,  fill=Tratamiento))+
  geom_boxplot()+
  xlab("Tratamientos")+
  ylab("Di?metro [m]")+
  scale_x_discrete(labels=trata)+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme( # tama?o de la letra y tipos de letras
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family="Times New Roman", size=14, face="bold"),
    axis.title.x = element_text(family="Times New Roman",size=12, face="bold"),
    axis.title.y = element_text(family="Times New Roman",size=12, face="bold"),
    legend.position = "none"
  ); p4
p5 <- ggplot(datos,aes(Tratamiento, N_rama,  fill=Tratamiento))+
  geom_boxplot()+
  xlab("Tratamientos")+
  ylab("Numero de ramas")+
  scale_x_discrete(labels=trata)+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme( # tama?o de la letra y tipos de letras
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family="Times New Roman", size=14, face="bold"),
    axis.title.x = element_text(family="Times New Roman",size=12, face="bold"),
    axis.title.y = element_text(family="Times New Roman",size=12, face="bold"),
    legend.position = "none"
  ); p5


  ggarrange(p4, p5, ncol = 2, labels = c("D", "E"))


hist(datos2$Total)
hist(datos2$Viruta)
hist(datos2$Copa)

par(mfrow=c(3,1))
boxplot(datos2$Total~datos2$Trat, xlab="Tratamiento", ylab = "Gramos [g]", main= "Variabilidad total por planta")
boxplot(datos2$Viruta~datos2$Trat, xlab="Tratamiento", ylab = "Gramos [g]", main= "Variabilidad en viruta", col="green")
boxplot(datos2$Copa~datos2$Trat, xlab="Tratamiento", ylab = "Gramos [g]", main= "Variabilidad en copa", col= "blue")
par(mfrow=c(1,1))

mod1<- lm(datos$Total~datos$Tratamiento, datos)
summary(mod1)
anova(mod1)

par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

#hsd32<- HSD.test(mod32,"datos$Tratamiento")
test <- HSD.test(mod1, "datos$Trat")
test
##############################3
mod2<- lm(datos$Viruta~datos$Tratamiento, datos)
summary(mod2)
anova(mod2)

par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))

#hsd32<- HSD.test(mod32,"datos$Tratamiento")
test2 <- HSD.test(mod2, "datos$Trat")
test2

##############################3
mod3<- lm(datos$Copa~datos$Tratamiento, datos)
summary(mod3)
anova(mod3)

par(mfrow=c(2,2))
plot(mod3)
par(mfrow=c(1,1))

#hsd32<- HSD.test(mod32,"datos$Tratamiento")
test3 <- HSD.test(mod3, "datos$Trat")
test3

leveneTest(datos2$Viruta, datos2$Trat)
leveneTest(datos2$Copa, datos2$Trat)
leveneTest(datos2$Total, datos2$Trat)


mod1 <- lm(log(Total)~Tratamiento, datos)
summary(mod1)

#Probar lotgaritmizando las variables
datos$LogViruta <- log(datos$Viruta)
datos$LogCopa <- log(datos$Copa)
datos$LogTotal <- log(datos$Total)

#Histogrma de las variables lotgaritmizadas
hist(datos$LogViruta)
hist(datos$LogCopa)
hist(datos$LogTotal)

#grafico con las varibles ya logarimizadas
boxplot(datos$LogViruta~datos$Tratamiento)
boxplot(datos$LogCopa~datos$Tratamiento)
boxplot(datos$LogTotal~datos$Tratamiento)

modl1<- lm(datos$LogViruta~datos$Tratamiento)
modl2<- lm(datos$LogCopa~datos$Tratamiento)
modl3<- lm(datos$LogTotal~datos$Tratamiento)

anova(modl1) #Viruta
anova(modl2)
anova(modl3)

summary(modl1) #Viruta
summary(modl2)
summary(modl3)

plot(modl1)
plot(modl2)
plot(modl3)

##### Mediante el sistema de bloques
boxplot(datos$LogViruta~datos$Bloque)
mod2 <- lm(datos$LogViruta~datos$Bloque)
summary(mod2)
anova(mod2)

#install.packages("agricolae")
require(agricolae)
library(agricolae)

prueba.lsd <-LSD.test(mod2, "Bloque",p.adj = "bon", console =T)
prueba.tukey <- HSD.test(mod2, "Bloque", console = T)


resumen <- aggregate(datos$Viruta~datos$Tratamiento+datos$Bloque, datos, mean)
resumen1 <- aggregate(datos$Copa~datos$Tratamiento+datos$Bloque, datos, mean)
resumen2 <- aggregate(datos$Total~datos$Tratamiento+datos$Bloque, datos, mean)


resumen
resumen1
resumen2

resumenT <- cbind(resumen,resumen1[,3], resumen2[,3])

resumen$Trat<-resumen[,1]

mod32 <- lm(datos$Viruta~datos$Tratamiento,datos)
anova(mod32)
plot(mod32)

shapiro.test(mod32$residuals)
hsd32<- HSD.test(mod32,"datos$Tratamiento")

write.csv(resumenT, "resumen.csv")

###################### Ejemplo de lsd
library(agricolae)
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
model
out <- LSD.test(model,"virus", p.adj="bonferroni")

out
####################################################

modlsd <- aov(`datos$LogViruta`~`datos$Tratamiento`, data= resumen)
modlsd
out1 <- LSD.test(modlsd,"Trat", p.adj = "bonferroni")
out1

#lsd no funciona para muestras no balanceadas
resumen2 <- aggregate(datos$LogViruta~datos$Tratamiento, datos, mean)
modlsd2 <- aov(resumen2$`datos$LogViruta`~resumen2$`datos$Tratamiento`, data = resumen2)
out2 <- LSD.test(modlsd2, "Tratamiento", p.adj = "bonferroni")
out2

#tampoco funciona

############# TUKEY ###############
a1 <- aov(resumen$`datos$LogViruta`~resumen$`datos$Tratamiento`)
posthoc<- TukeyHSD(x=a1, "resumen$`datos$Tratamiento`",  conf.level=0.95)
posthoc
plot(a1)

a2 <- aov(resumen$`datos$LogViruta`~resumen$`datos$Tratamiento`+resumen$`datos$Bloque`)
posthoc2<- TukeyHSD(x=a2,  conf.level=0.95)
posthoc
plot(a2)

library(ggplot2)

b1 <- ggplot(datos, aes(x=Tratamiento, y=Viruta, col=Tratamiento))+
  geom_boxplot()+
  xlab("Tratamiento")+
  ylab("Kg Viruta")
b1

datosST0<- subset(datos, datos$Tratamiento != "T0")
b2 <- ggplot(datosST0, aes(x=Tratamiento, y=Viruta))+
  geom_boxplot()+
  xlab("Tratamiento")+
  ylab("Kg Viruta")
b2

resumenST0 <-aggregate(datos$LogViruta~datos$Tratamiento+datos$Bloque, datos, mean)
resumenST0
a3 <- aov(resumenST0$`datos$LogViruta`~resumenST0$`datos$Tratamiento`)
posthoc<- TukeyHSD(x=a3, "resumenST0$`datos$Tratamiento`",  conf.level=0.99)
posthoc
plot(a3)


b3 <- ggplot(datos, aes(Tratamiento, Total))+
  geom_boxplot()
b3

b4 <- ggplot(datos, aes(Planta, Total))+
  geom_boxplot()
b4

b4 <- ggplot(datos, aes(as.factor(Bloque), Total))+
  geom_boxplot()
b4

# Analisis de los datos de Viruta
summary(datos$Viruta)
sd(datos$Viruta)
hist(datos$Viruta)

sdNeg <- mean(datos$Viruta) - 3*(sd(datos$Viruta))
sdPos <- mean(datos$Viruta) + 2*(sd(datos$Viruta))

filViru <- subset(datos, datos$Viruta<sdPos)
hist(filViru$Viruta)
shapiro.test(filViru$Viruta)

boxplot(datos$Viruta~datos$Tratamiento)
boxplot(datos$Copa~datos$Tratamiento)
boxplot(datos$Diam_copa~datos$Tratamiento)
boxplot(datos$N_rama~datos$Tratamiento)

names(datos)


mod20 <- lm(Diam_copa~Tratamiento, datos)
summary(mod20)
anova(mod20)

mod30 <- lm(N_rama~Tratamiento, datos)
summary(mod30)
anova(mod30)

plot(N_rama~Tratamiento, datos)
plot(Diam_copa~Tratamiento, datos)

names(res9)
res10 <- aggregate(NRama~Tratamiendo, res9, mean)
res11 <- aggregate(DiametroCopa~Tratamiendo, res9, mean)

par(mfrow=c(1,2))
barplot(res10$NRama, ylab = "Numero de ramas cortadas", names.arg = c(paste("T",0:5)),
        xlab="Tratamientos",col = rainbow(25))
barplot(res11$DiametroCopa, ylab = "Diametro de copa [m]", names.arg = c(paste("T",0:5)),
        xlab="Tratamientos",col = rainbow(25))
par(mfrow=c(1,1))

res20 <- aggregate(datos$Viruta~Tratamiento, datos, mean)
res21 <- aggregate(datos$Copa~Tratamiento, datos, mean)
res22 <- aggregate(datos$Total~Tratamiento, datos, mean)
res23 <- aggregate(datos$Diam_copa~Tratamiento, datos, mean)
res24 <- aggregate(datos$N_rama~Tratamiento, datos, mean)

mean(datos$N_rama)
res24$Por <- res24$`datos$N_rama`/mean(datos$N_rama)*100

mean(datos$Diam_copa)
res23$Por <- res23$`datos$Diam_copa`/mean(datos$Diam_copa)*100

res22$Por <- res22$`datos$Total`/mean(datos$Total)*100
res22[3,2]-mean(datos$Total)
res22[4,2]-mean(datos$Total)


####---- Nuevo Planteo ----
library("dplyr")
library("ggplot2")

names(datos)

ggplot(datos)+
  geom_point(aes(Diam_copa,Viruta))+
  facet_wrap(~Tratamiento, nrow = 2)+
  geom_smooth(aes(Diam_copa,Viruta), method = "lm")

ggplot(datos)+
  geom_point(aes(Diam_copa,Copa))+
  facet_wrap(~Tratamiento, nrow = 2)+
  geom_smooth(aes(Diam_copa,Copa), method = "lm")


d1 <- datos %>% group_by(Tratamiento) %>% filter(Tratamiento=="T0")
ggplot(d1)+
  geom_point(aes(Diam_copa,Viruta, color=Tratamiento))+
  geom_smooth(aes(Diam_copa,Viruta), method = "lm")

#Aplicación de clasificacion por clusters K means
clusters <- kmeans(datos[,c(4,7)],3)

length(clusters)

datos$Cluster <- clusters$cluster

ggplot(datos, aes())+
  geom_point(aes(Diam_copa,Viruta,color=as.factor(Cluster)))+
  facet_wrap(~Tratamiento, nrow = 2)+
  geom_smooth(aes(Diam_copa,Viruta), method = "lm")

ggplot(datos, aes(Diam_copa, Viruta, color = as.factor(Cluster)))+
  geom_point()

ggplot(datos)+
  geom_point(aes(Diam_copa,Copa, color = as.factor(Cluster)))+
  facet_wrap(~Tratamiento, nrow = 2)+
  geom_smooth(aes(Diam_copa,Copa), method = "lm")


# Analisis por grupo
P2 <- datos %>% filter(Cluster==2)

ggplot(P2, aes(Tratamiento, Viruta))+
  geom_boxplot()

ggplot(P2, aes(Tratamiento, N_rama))+
  geom_boxplot()

library(agricolae)


