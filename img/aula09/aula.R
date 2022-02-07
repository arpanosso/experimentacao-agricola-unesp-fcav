# Carregando o pacote par análise de variância
library(ExpDes.pt)
caminho<-"C:\\Users\\Panosso\\Desktop\\ExpAgri061\\aula.txt"
d<-read.table(caminho,h=TRUE)
head(d)
Cultivar<-factor(d$C)
Espaçamento<-factor(d$E)
y<-d$y
interaction.plot(Cultivar,Espaçamento,y,mean)
interaction.plot(Espaçamento,Cultivar,y,mean)
fat2.dic(Cultivar,Espaçamento,y,fac.names = c("Cultivar", "Espaçamento"))
