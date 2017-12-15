#SISTEMAS NEBULOSOS - CLASSIFICADOR FUZZY
#ALUNOS: Thiago Mattar e Pedro Soares
#PROFESSOR: Cristiano Leito

#Observação
#Linhas de comando para iterar o método 20 vezes para eliminar o viés
#e para variar o K de 2:8 estão comentadas


rm(list=ls())
library('rgl')
library('e1071')
library('readr')

probSum <- function(x)
{
  n=length(x)
  aux=0
  for(i in 1:n)
  {
    aux=aux+x[i]-aux*x[i]
  }
  return(aux)
}

SplitData_TraindAndTest <- function(x,y,r)
{
  xc1=x[which(y==1),]
  xc2=x[which(y==0),]
  yc1=y[which(y==1)]
  yc2=y[which(y==0)]
  
  trainX=rbind(xc1[1:(r*nrow(xc1)),],xc2[1:(r*nrow(xc2)),])
  trainY=c(yc1[1:(r*nrow(xc1))],yc2[1:(r*nrow(xc2))])
  testX=rbind(xc1[(r*nrow(xc1)+1):nrow(xc1),],xc2[(r*nrow(xc2)+1):nrow(xc2),])
  testY=c(yc1[(r*nrow(xc1)+1):nrow(xc1)],yc2[(r*nrow(xc2)+1):nrow(xc2)])
  
  out=list(trainX=trainX,trainY=trainY,testX=testX,testY=testY)
  return(out)
}

#Função normal unidimensional
fnormal1var <- function(x,m,r) (1/sqrt(2*pi*r^2)*exp(-0.5*((x-m)/r)^2))

#Lendo os dados
q4data <- read_csv("~/Sistemas Nebulosos/TP2/Q4/q4data.csv",col_names = FALSE)

#accuracy<-matrix(0,nrow=20,ncol=8)
#for(q in 1:20)
#{
  #Reordenando os dados
  q4data <- data.matrix(q4data)[sample(nrow(q4data)),]
  x <- q4data[,1:2]
  y <- q4data[,3]
  
  #Visualizando os dados
  plot(x[y==1,1],x[y==1,2],col='red',xlim=c(-1,2),ylim=c(-1,2),
       xlab='',ylab='')
  par(new=T)
  plot(x[y==0,1],x[y==0,2],col='blue',xlim=c(-1,2),ylim=c(-1,2),
       xlab='x1',ylab='x2',main='Amostras e classes')
  
  #Dividindo em treinamento e teste
  split_data<-SplitData_TraindAndTest(x,y,0.7)
  trainX<-split_data[[1]]
  trainY<-split_data[[2]]
  testX<-split_data[[3]]
  testY<-split_data[[4]]
  
  #Fuzzy C-Means para k centroides
  k<-8
  acc<-c()
  #acc[1]<-0
  #for(k in 2:8)
  #{
    fcm<-cmeans(trainX,k,iter.max = 500,dist="euclidean",method="cmeans")
    
    #Extraindo os parâmetros para distribuição dos dados
    m<-list()
    sig<-list()
    p<-c()
    plot_colour<-colours()[sample(length(colours()),k)]
    for (i in 1:k)
    {
      #Média de cada cluster
      m[[i]]<-c(mean(trainX[fcm[[3]]==i,1]),mean(trainX[fcm[[3]]==i,2]))
      
      #Dispersão de cada cluster
      sig[[i]]<-c(sd(trainX[fcm[[3]]==i,1]),sd(trainX[fcm[[3]]==i,2]))
        #cov(trainX[fcm[[3]]==i,])
      
      #Plot dos clusters
      plot(trainX[fcm[[3]]==i,1],trainX[fcm[[3]]==i,2],
           xlim=c(-1,2),ylim=c(-1,2),col=plot_colour[i],xlab='',ylab='')
      par(new=T)
    }
    plot(trainX[fcm[[3]]==i,1],trainX[fcm[[3]]==i,2],
         xlim=c(-1,2),ylim=c(-1,2),col=plot_colour[i],xlab='',ylab='')
    
    #Teste de a qual classe cada cluster pertence
    clustertoclass<-matrix(0,nrow=k,ncol=2)
    for(i in 1:k)
    {
      clustertoclass[i,1]<-sum(fcm[[3]][which(trainY=='1')]==i)
      clustertoclass[i,2]<-sum(fcm[[3]][which(trainY=='0')]==i)
    }
    #Função de pertinência entre o cluster e a classe
    clustertoclass<-clustertoclass/apply(clustertoclass,1,sum)
    
    #Cálculo dos consequentes para as k regras, para os dados de teste
    u<-matrix(0,nrow=nrow(testX),ncol=2)
    aux<-matrix(0,nrow=k,ncol=2)
    for(i in 1:nrow(testX))
    {
      for(j in 1:k)
      {
        #Cálculo do valor da função de pertinência em cada dimensão
        aux[j,1]<-fnormal1var(testX[i,1],m[[j]][1],sig[[j]][1])
        aux[j,2]<-fnormal1var(testX[i,2],m[[j]][2],sig[[j]][2])
      }
      
      #Operador de soma probabilística
      u[i,1]<-probSum(aux[,1]*aux[,2]*clustertoclass[,1])
      u[i,2]<-probSum(aux[,1]*aux[,2]*clustertoclass[,2])
    }
    
    yhat<-1*(u[,1]>u[,2])
    #acc[k]<-sum(diag(table(testY,yhat)))/sum(table(testY,yhat))
  #}
  #accuracy[q,]<-acc

#}

#Cálculo das acurácias médias e variâncias
# plot(100*colMeans(na.omit(accuracy)),xlab='K',ylab='Acurácia (%)',
#      main='Acurácia x Número de regras',col='blue',
#      xlim=c(1,8),ylim=c(0,100))
# par(new=T)
# plot(which.max(100*colMeans(na.omit(accuracy))),
#      max(100*colMeans(na.omit(accuracy))),xlab='K',ylab='Acurácia (%)',
#      main='Acurácia x Número de regras',col='red',
#      xlim=c(1,8),ylim=c(0,100))
# 
# accuracy_sd<-100*apply(na.omit(accuracy),2,sd)
# accuracy_mean<-100*colMeans(na.omit(accuracy))

#Plot 3D para k=2,4,8
seqi<-seq(-1,2,0.1)
seqj<-seq(-1,2,0.1)
M1<-matrix(0,nrow=length(seqi),ncol=length(seqj))
M2<-matrix(0,nrow=length(seqi),ncol=length(seqj))
ci<-0
for(i in seqi)
{
  ci<-ci+1
  cj<-0
  for(j in seqj)
  {
    cj<-cj+1
    xt<-t(as.matrix(c(i,j)))
    for(l in 1:k)
    {
      aux[l,1]<-fnormal1var(xt[,1],m[[l]][1],sig[[l]][1])
      aux[l,2]<-fnormal1var(xt[,2],m[[l]][2],sig[[l]][2])
    }
    M1[ci,cj]<-probSum(aux[,1]*aux[,2]*clustertoclass[,1])
    M2[ci,cj]<-probSum(aux[,1]*aux[,2]*clustertoclass[,2])
  }
}
M3<-1*(M1>M2)
persp3d(seqi,seqj,M3,alpha=0.5,col='lightblue',
                xlab='X',ylab='Y',zlab='u')
points3d(x[y==0,1],x[y==0,2],0.3,col='blue',size=6)
points3d(x[y==1,1],x[y==1,2],0.3,col='red',size=6)

