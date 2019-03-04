#Taller 1 Aplicada III(diseño completamente aleatorizado)
#Punto 1:
Calcio<-c(23.46,23.48,23.56,23.39,23.40,23.59,23.46,23.42,23.49,23.50,23.51,23.64,23.46,
          23.52,23.49,23.28,23.40,23.37,23.46,23.39,23.29,23.46,23.37,23.32,23.38)
Lote<-as.factor(c(rep("Lote 1",5),rep("Lote 2",5),rep("Lote 3",5),rep("Lote 4",5),rep("Lote 5",5)))
datos<-data.frame(Lote=Lote, Calcio=Calcio)
attach(datos) 
head(datos)
#Descriptivos
library(ggplot2)
p<-ggplot(datos, aes(Lote,Calcio)) + geom_point()
p + scale_x_discrete(name="Lote",
                     labels=c("Lote 1","Lote 2","Lote 3","Lote 4","Lote 5")) + labs(y="Calcio")

#Incluye la media en el gr?fico
x11()
p + scale_x_discrete(name="Lote",
                     labels=c("Lote 1","Lote 2","Lote 3","Lote 4","Lote 5")) + labs(y="Calcio") +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=19, size=2)
#Construimos el modelo y obtenemos la ANOVA:
mod<-aov(Calcio~Lote, data=datos)
summary(mod)
#Si no se cumplen los supuestos, no sirve interpretar los resultados de la ANOVA

#Verificamos los supuestos:
#Se obtienen los residuales:
resid<-residuals(mod)

#Normalidad:
#Pruebas gr?ficas:
library(car)
x11()
par(mfrow=c(1,2))
hist(resid, freq=FALSE)
curve(dnorm(x,mean(resid), sd(resid)), xlim=c(-4,6), add=TRUE, col=2)
qqPlot(resid, pch=20,main="QQ-Plot de los residuos")

#Prueba formal
#Shapiro para muestras menores a 30, Anderson- Darling para muestras mayores a 30
#Shapiro
shapiro.test(resid) 

#Anderson-Darling
library(nortest)
ad.test(resid) 

#Media Cero
t.test(resid, mu = 0, alternative = c("two.sided"))

#Homogeneidad de Varianzas
x11()
plot(mod$fitted.values, rstandard(mod), main="Valores ajustados vs Residuos",xlab = "Valores Ajustados",ylab = "Residuos Estandarizados")
abline(h=0, col="red") ## traza una l?nea horizontal (h) por el Y=0
#Bajo normalidad utilizar Barlett
bartlett.test(resid~Lote, data=datos) 

#Independencia en los errores
#Prueba de rachas: H0:Los residuales se distribuyen de manera aleatoria
library("tseries")
residualesfactor<-c()
for (i in 1:length(residuals(mod))) {
  if (residuals(mod)[i]>0){
    residualesfactor[i]=1
  }
  if (residuals(mod)[i]<0){
    residualesfactor[i]=-1
  }
}
runs.test(factor(residualesfactor))



#Pruebas de Comparaciones M?ltiple (Pruebas Postanova)
library(agricolae)
#Prueba de Fisher (no consevardor, da prioridad al error tipo II, tiende a rechazar la hip?tesis nula)
#Alpha individual para cada comparaci?n.
LSD.test(mod,"Lote", alpha = 0.05, console=TRUE, group=FALSE)

#P(rechazar al menos una comparaci?n)=1-P(aceptar todas)
#                                    =1-(1-0.05)^10=0.4

LSD.test(mod,"Lote", alpha = 0.05, console=TRUE, group=TRUE)

#Prueba de Tukey (consevardor, da prioridad al error tipo I, tiene a no rechazar H0)
#Alpha individual para cada comparaci?n.
HSD.test(mod,"Lote", alpha=0.05, console=TRUE, group=FALSE)
HSD.test(mod,"Lote", alpha=0.05, console=TRUE, group=TRUE)

#C?DIGO ALTERNO:
library(lsmeans)
library(emmeans)
Comp1<-lsmeans(mod,~Nutriente)
pairs(Comp1, adjust="tukey")

library(multcompView)
cld(Comp1, alpha=0.05, Letters=letters, adjust="tukey")

#Graficando los intervalos de confianza:
plot(TukeyHSD(mod,"Nutriente"))

#Prueba de Newman-Kewls (Intermedio)
#Alpha Intermedio entre Fisher y Tukey
SNK.test(mod,"Nutriente", group=TRUE, console=TRUE, alpha=0.05)

#CUANDO NO SE CUMPLE EL SUPUESTO DE NORMALIDAD:
#Prueba No Param?trica de Kruskal-Wallis(no importa el supuesto de normalidad, pero debe cumplirse el supuesto de homogeneidad de varianzas)
kruskal.test(Calcio~Lote, data=datos)

#Prueba de comparaci?n m?ltiple cuando todos los valores de la variable respuesta son diferentes (no hay empates) .
library(PMCMR)
posthoc.kruskal.nemenyi.test(x=datos$Calcio, g=datos$Lote, dist="Tukey")

#Prueba de comparaci?n m?ltiple cuando algunos valores de la variable respuesta son iguales (hay empates).
posthoc.kruskal.nemenyi.test(x=datos$Calcio, g=datos$Lote, dist="Chisquare")


#Punto 2
#DATOS
trabajador=c(rep("1",6),rep("2",6),rep("3",6),rep("4",6),rep("5",6))
y=c(45,47,43,48,50,44,52,55,58,49,47,57,39,37,46,45,42,41,57,49,52,50,48,55,48,44,55,53,49,52)
datos=data.frame(matrix(c(trabajador,y),ncol=2,nrow=length(y)))
#Descriptivos
tr=data.frame(matrix(y,ncol = 5,nrow = 6))
colnames(tr)=c(seq(1:5))
summary(tr) #descriptivas
de=sqrt(diag(var(tr)))*(((6-1)/6)^2)#desviacion
#vector de medias
ma=c(mean(tr$`1`),mean(tr$`2`),mean(tr$`3`),mean(tr$`4`),mean(tr$`5`))
cv=(de/ma)*100#coeficiente de variacion %
#grafico
library(ggplot2)
x11()
p<-ggplot(datos, aes(trabajador, y)) + geom_point()
p + scale_x_discrete(name="trabajador",
                     labels=c("1","2","3","4","5")) + labs(y="Unidades / periodo")
x11()
#Incluye la media en el gráfico
p<-ggplot(datos, aes(trabajador, y)) + geom_point()
p + scale_x_discrete(name="trabajador",
                     labels=c("1","2","3","4","5")) + labs(y="Unidades / periodo") +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=19, size=2)

#Construimos el modelo y obtenemos la ANOVA:
mod<-aov(y~trabajador, data=datos)
summary(mod)

#Si no se cumplen los supuestos, no sirve interpretar los resultados de la ANOVA

#Verificamos los supuestos:
#Se obtienen los residuales:
resid<-residuals(mod)

#Normalidad:
#Pruebas gráficas:
install.packages("carData")
library(car)
x11()
par(mfrow=c(1,2))
hist(resid, freq=FALSE)
curve(dnorm(x,mean(resid), sd(resid)), xlim=c(-8,6), add=TRUE, col=2)

qqPlot(resid, pch=20)

#Prueba formal
#Shapiro para muestras menores a 30, Anderson- Darling para muestras mayores a 30
#Shapiro
shapiro.test(resid) 

#Anderson-Darling
install.packages("nortest")
library(nortest)
ad.test(resid) 

#Media Cero
t.test(resid, mu = 0, alternative = c("two.sided"))

#Homogeneidad de Varianzas
#Bajo normalidad utilizar Barlett
bartlett.test(resid~trabajador, data=datos) 

#Homocedasticidad de los residuos
x11()
plot(mod$fitted.values, rstandard(mod), main="Valores ajustados vs Residuos",xlab = "Valores Ajustados",ylab = "Residuos Estandarizados")
abline(h=0, col="red") ## traza una línea horizontal (h) por el Y=0

#Prueba de rachas: H0:Los residuales se distribuyen de manera aleatoria
library("tseries")
residualesfactor<-c()
for (i in 1:length(residuals(mod))) {
  if (residuals(mod)[i]>0){
    residualesfactor[i]=1
  }
  if (residuals(mod)[i]<0){
    residualesfactor[i]=-1
  }
}
runs.test(factor(residualesfactor))

#Pruebas de Comparaciones Múltiple (Pruebas Postanova)
install.packages("lsmeans")
install.packages("emmean")
install.packages("multcompView")
#CÓDIGO ALTERNO:
library(lsmeans)
library(emmeans)
Comp1<-lsmeans(mod,~trabajador)
pairs(Comp1)

library(multcompView)
cld(Comp1, alpha=0.05, Letters=letters)

#CUANDO NO SE CUMPLE EL SUPUESTO DE HOMOGENEIDAD DE VARIANZA
install.packages("nlme")
library(nlme)
#modelo
mod_heterog<-gls(y~1+trabajador,
                 weight=varComb(varIdent(form=~1|trabajador)),
                 data=datos)
summary(mod_heterog)
#ANOVA
anova(mod_heterog)

#post-ANOVA
Comp2<-lsmeans(mod_heterog,~trabajador)
pairs(Comp2, adjust="tukey")

cld(Comp2, alpha=0.05, Letters=letters, adjust="tukey")

#comparacion de modelos
mod_hom<-gls(y~1+trabajador, data=datos, method="ML")

mod_het<-gls(y~1+trabajador,
             weight=varComb(varIdent(form=~1|trabajador)),
             data=datos, method="ML")

anova(mod_hom, mod_het)