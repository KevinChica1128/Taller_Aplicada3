datos<-read.table("clipboard", header=TRUE, dec=".")
attach(datos) 
head(datos)

#Descriptivos
library(ggplot2)

p<-ggplot(datos, aes(Nutriente, Altura)) + geom_point()
p + scale_x_discrete(name="Nutriente",
                     labels=c("Sin","N","NP","NPK","NK")) + labs(y="Altura (cm)")
#Incluye la media en el gráfico
p + scale_x_discrete(name="Nutriente",
                     labels=c("Sin","N","NP","NPK","NK")) + labs(y="Altura (cm)") +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=19, size=2)

#Construimos el modelo y obtenemos la ANOVA:
mod<-aov(Altura~Nutriente, data=datos)
summary(mod)
#Si no se cumplen los supuestos, no sirve interpretar los resultados de la ANOVA

#Verificamos los supuestos:
#Se obtienen los residuales:
resid<-residuals(mod)

#Normalidad:
#Pruebas gráficas:
hist(resid, freq=FALSE)
curve(dnorm(x,mean(resid), sd(resid)), xlim=c(-4,6), add=TRUE, col=2)

library(car)
qqPlot(resid, pch=20)

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
#Bajo normalidad utilizar Barlett
bartlett.test(resid~Nutriente, data=datos) 

#Bajo no normalidad utilizar la prueba de levene
library(car)
leveneTest(resid~Nutriente, data=datos) 

#Pruebas de Comparaciones Múltiple (Pruebas Postanova)
library(agricolae)
#Prueba de Fisher (no consevardor, da prioridad al error tipo II, tiende a rechazar la hipótesis nula)
#Alpha individual para cada comparación.
LSD.test(mod,"Nutriente", alpha = 0.05, console=TRUE, group=FALSE)

#P(rechazar al menos una comparación)=1-P(aceptar todas)
#                                    =1-(1-0.05)^10=0.4

LSD.test(mod,"Nutriente", alpha = 0.05, console=TRUE, group=TRUE)

#Prueba de Tukey (consevardor, da prioridad al error tipo I, tiene a no rechazar H0)
#Alpha individual para cada comparación.
HSD.test(mod,"Nutriente", alpha=0.05, console=TRUE, group=FALSE)
HSD.test(mod,"Nutriente", alpha=0.05, console=TRUE, group=TRUE)

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
kruskal.test(Altura~Nutriente, data=datos)

#Prueba de comparación múltiple cuando todos los valores de la variable respuesta son diferentes (no hay empates) .
library(PMCMR)
posthoc.kruskal.nemenyi.test(x=datos$Altura, g=datos$Nutriente, dist="Tukey")

#Prueba de comparación múltiple cuando algunos valores de la variable respuesta son iguales (hay empates).
posthoc.kruskal.nemenyi.test(x=datos$Altura, g=datos$Nutriente, dist="Chisquare")

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

