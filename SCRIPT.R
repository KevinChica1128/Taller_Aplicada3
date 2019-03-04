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

#Incluye la media en el gráfico
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
#Pruebas gráficas:
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
abline(h=0, col="red") ## traza una línea horizontal (h) por el Y=0
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



#Pruebas de Comparaciones Múltiple (Pruebas Postanova)
library(agricolae)
#Prueba de Fisher (no consevardor, da prioridad al error tipo II, tiende a rechazar la hipótesis nula)
#Alpha individual para cada comparación.
LSD.test(mod,"Lote", alpha = 0.05, console=TRUE, group=FALSE)

#P(rechazar al menos una comparación)=1-P(aceptar todas)
#                                    =1-(1-0.05)^10=0.4

LSD.test(mod,"Lote", alpha = 0.05, console=TRUE, group=TRUE)

#Prueba de Tukey (consevardor, da prioridad al error tipo I, tiene a no rechazar H0)
#Alpha individual para cada comparación.
HSD.test(mod,"Lote", alpha=0.05, console=TRUE, group=FALSE)
HSD.test(mod,"Lote", alpha=0.05, console=TRUE, group=TRUE)

#CÓDIGO ALTERNO:
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
#Prueba No Paramétrica de Kruskal-Wallis(no importa el supuesto de normalidad, pero debe cumplirse el supuesto de homogeneidad de varianzas)
kruskal.test(Calcio~Lote, data=datos)

#Prueba de comparación múltiple cuando todos los valores de la variable respuesta son diferentes (no hay empates) .
library(PMCMR)
posthoc.kruskal.nemenyi.test(x=datos$Calcio, g=datos$Lote, dist="Tukey")

#Prueba de comparación múltiple cuando algunos valores de la variable respuesta son iguales (hay empates).
posthoc.kruskal.nemenyi.test(x=datos$Calcio, g=datos$Lote, dist="Chisquare")

#Prueba alternativa Paramétrica cuando no hay homogeneidad de varianzas
#Estimación mediante Mínimos Cuadrados Generalizados (asume normalidad).
library(nlme)
#Incorporamos en el modelo la matriz de varianzas desiguales, donde cada varianza depende de su tratamiento:
mod_heterog<-gls(Altura~1+Nutriente,
                   weight=varComb(varIdent(form=~1|Nutriente)),
                   data=datos)
library(car)
summary(mod_heterog)

#Estimamos el ANOVA para el modelo construido:
anova(mod_heterog)

#Prueba de comparación
Comp1<-lsmeans(mod_heterog,~Nutriente)
pairs(Comp1, adjust="tukey")
cld(Comp1, alpha=0.05, Letters=letters, adjust="tukey")

#Si desearamos comparar entre el modelo con Varianzas Homogeneas vs Varianzas Heterogeneas
mod_hom<-gls(Altura~1+Nutriente, data=datos, method="ML")
mod_het<-gls(Altura~1+Nutriente,
             weight=varComb(varIdent(form=~1|Nutriente)),
             data=datos, method="ML")
anova(mod_hom, mod_het)

#Prueba Alternativa No Paramétrica cuando no hay normalidad y no hay homogeneidad de varianzas mediante Test de Permutaciones.
library(coin)
independence_test(Altura ~ Nutriente, data = datos)

#Para determinar entre que tratamientos existe diferencias, hacemos uso de un Test de Permutaciones.
library(rcompanion)
PT = pairwisePermutationTest(Altura ~ Nutriente, data = datos, method="fdr")
PT

#Presentando las diferecnias en letras a partir de los p.adjust:
cldList(p.adjust ~ Comparison, data = PT, threshold  = 0.05)

