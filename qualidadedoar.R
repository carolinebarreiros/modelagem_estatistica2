# Definindo o projeto de curso
# ----------------------------------------------------------
# Pergunta: O que afeta (quais vari?veis) a qualidade do ar?
# Como? (rela??o entre as vari?veis)
# ----------------------------------------------------------

# install.packages("Ecdat") # se necess?rio - para pegar a base de dados
library(Ecdat) # carregando
data(Airq) # carregando o banco de dados do pacote
names(Airq) # exibe os nomes das vari?veis

# Descrevendo as vari?veis
# ------------------------------------------------------------
# airq: ?ndice de qualidade do ar (quanto menor, melhor) - vari?vel resposta
# vala: valor das empresas nas cidades (milhares de d?lares)
# rain: quantidade de chuva (em polegadas)
# coas: posi??o costeira da cidade (sim ou n?o) - bin?ria e as outras cont?nuas
# dens: densidade populacional (miha quadrada)
# medi: renda m?dia per capita (d?lares)
# -------------------------------------------------------------

# An?lise descritiva ou explorat?ria

summary(Airq) = #sum?rio das vari?veis
  
  # as vari?veis podem ser cont?nuas ou categ?ricas (divididas em categ?ricas)
  # a vari?vel resposta ? a qualidade do ar (airq)
  plot(airq~vala, data=Airq)
# ~ em fun??o de

# Criando um modelo estat?stico
# y (resposta - smp ser? ?nica - q sofre o efeito) ~ x (explicativa)
# x (crescimento da planta) ~ x (quantidade de adubo) + x (quantidade de luz)
# y ~ x1 + x2 + x3
# airq ~ vala + coas + rain

# ------------------------------------
# ------------------------------------

# Montando o modelo
# Regress?o linear - qdo vc tem duas vari?veis cont?nuas e uma reta

m1<- lm(airq ~ vala, data=Airq) # lm (modelo linear)
# alguns dados n?o s?o lineares
# existe efeito da "vala" na airq?
summary(m1) # para saber a signific?ncia do modelo
plot(airq~vala, data=Airq) #plot de regress?o linear

# p-valor indica a signific?ncia do modelo ou da vari?vel (foi signaficativo?)
# se o p-valor for menor (<) 0.05 - 95% de chances de estar certo - ? significativo
# se o p-valor for maior (>) 0.05 - n?o existe o efeito esperado

# A vari?vel "vala" n?o influenciou na qualidade do ar nas cidades("airq")

# A vari?vel "coas" afeta a "airq"?
m2<-lm(airq~coas, data=Airq)
summary(m2)
# Sim! A posi??o costeira da cidade afeta a qualidade do ar nas cidades.
# as cidades costeiras apresentam uma melhor qualidade do ar.
plot(airq~coas, data=Airq)

# A vari?vel "medi" afeta a qualidade do ar?
m3<-lm(airq~medi, data=Airq)
summary(m3)
plot(airq~medi, data=Airq)
# a vari?vel n?o afetou a qualidade do ar

# A quantidade de chuva influencia a qualidade do ar?
m4<-lm(airq~rain, data=Airq)
summary(m4)
plot(airq~rain, data=Airq)
# A qtdade de chuva n?o afeta a qualidade do ar.

# A densidade populacional afeta a qualidade do ar?
m5<-lm(airq~dens, data= Airq)
summary(m5)
plot(airq~dens, data=Airq)
# N?o existe o efeito da densidade populacional na qualidade do ar nessas cidades.

# --------------------------
# termos: anova (vari?vel cont?nua ~ de uma vari?vel categ?rica)
# regress?o (vari?vel cont?nua ~ vari?vel cont?nua)
# regress?o m?ltipla (vari?vel cont?nua ~ vari?veis cont?nuas ou n?o)

# Retas de modelos n?o significativos s?o opcionais nos gr?ficos. 

# Retas nos gr?ficos
plot(airq~medi, data=Airq)
# y=a+b*x (equa??o da reta)
# a<- intecepto (onde a reta vai tocar o eixo y)
# b<- ? a inclina??o da reta
curve(9.936e+01+5.638e-04*x, add=TRUE)

# Melhorar o gr?fico
plot(airq~medi, data=Airq, xlab="Renda m?dia per capita", ylab="Qualidade do ar", 
     pch=1, col="blue", cex.lab=1.3, main="Renda m?dia - 2010") #pch - pontos no help - par?metro
curve(9.936e+01+5.638e-04*x, add=TRUE, col="darkblue", lwd=2,lty=2) #curve no help

plot(airq~vala, data=Airq, xlab="Valor das empresas ($)", ylab="Qualidade do ar",
     col="blue", pch=1, cex=1.2)
curve(96.451419+0.001969*x, add=TRUE, col="darkblue", lwd=2, lty=2)

plot(airq~coas, data=Airq, xlab="Posi??o costeira", ylab="Qualidade do ar",
     col="pink", ylin=c(50,170), cex.lab=1.3, main="An?lise da qualiade do ar")

# Regress?o m?ltipla
# --------------------------------------------------
mRM1<-lm(airq~vala+coas, data=Airq)
summary(mRM1)

# Ent?o existe um efeito da posi??o costeira e do valor das empresas na qualidade do ar

# Gr?fico regress?o m?ltipla
plot(airq~vala, data=Airq, xlab="Valor das empresas ($)", ylab="Qualidade do ar")
curve(1.171e+02+1.999e-03*x, add=TRUE) #cidade n?o costeira
curve(1.171e+02+1.999e-03*x+-2.968e+01,lty=2,add=TRUE) #cidade costeira
legend("bottomright", c("N?o costeiras", "Costeiras"), pch=1, lty=c(1,2),bty="n")

# A qualidade do ar das cidades ? afetada tanto pelo valor das empresas
# quanto pela posi??o costeira das cidades. Quanto maior o valor das empresas,pior a
# qualidade do ar das cidades. Al?m disso as cidades n?o costeiras apresentam qualidade
# do ar pior do que nas costeiras.

mRM2<-lm(airq~vala+coas+dens, data=Airq)
summary(mRM2)


# Contraste de modelos
# --------------------------------------------------
# Comparar um modelo completo com um modelo sem a vari?vel em quest?o
modelocompleto<-lm(airq~vala+coas+dens, data=Airq)
modeloincompleto<-lm(airq~vala+coas, data=Airq)
# os modelos s?o iguais?
# Se p>0,05 n?o existe diferen?a entre os modelos - se n?o tem diferen?a fico com o mais simples
# Se p<0,05 os modelos s?o diferentes e a vari?vel n?o deve ser retirada do modelo.

# Retirada do modelo
anova(modelocompleto, modeloincompleto)

# Gr?fico final
# --------------------------------------------------
plot(airq~vala, data=Airq, xlab="Valor das empresas ($)", ylab="Qualidade do ar",
     cex.lab=1.3,col="blue")
curve(1.171e+02+1.999e-03*x, add=TRUE,col="darkblue",lwd=1.4) 
curve(1.171e+02+1.999e-03*x+-2.968e+01,lty=2,add=TRUE,col="darkblue",lwd=1.4) 
legend("bottomright", c("N?o costeiras", "Costeiras"), pch=1, lty=c(1,2),bty="n")

# Conclus?o
# --------------------------------------
# O que afeta a qualidade do ar nas cidades?
# As vari?veis que afeteram foram: (a) o valor das empreasas e (b) a posi??o costeira
# das cidades. Quanto maior o valor das empresas, pior a qualidade do ar. Cidades
# costeiras apresentam uma melhor qualidade do ar.