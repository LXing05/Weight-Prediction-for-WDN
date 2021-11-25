rm(list=ls())
setwd("/Users/lxing/Documents/Conference&Journal paper/paper for SSN 06.05")
source("fairness_goodness_func.R")
#--------------------predict the edge weights using kNN, 2nd method----------------------#
#metric of edges is defined only involving the edge weights in the training set#
#----------------------------------------------#
data=read.table("epinion_data.txt",header=T)
N.edge<-nrow(data)

#------select the subgraph A----------# #seed:112233 wikisign; 111222 epinions; 11111 bitcoin
set.seed(11111)
idx.sub<-sort(sample(1:N.edge,0.8*N.edge,replace=F))
A<-data[idx.sub,] #the subgraph

#-----compute the metric on edges-------#
C_edge<-rep(0,N.edge)
h=sd(A[,3]) #h is the tuning parameter
#h=0.1
for(i in 1:N.edge){
  e=data[i,-3]
  u_e=e[1,1]
  p_e=e[1,2]
  f.1<-A[A[,1]==u_e,]
  f.2<-A[(A[,1]!=u_e)&(A[,2]==p_e),]
  f<-rbind(f.1,f.2) #f are the neighbors of e
  avg_Wf<-mean(f[,3]) #average edge weights of the neighbor of e
  
  C_edge[i]<-sum((f[,3]>=avg_Wf-h)&(f[,3]<=avg_Wf+h))
}
D_edge<-matrix(0,nrow=N.edge,ncol=N.edge)
for(i in 1:N.edge){
  for(j in 1:N.edge){
    D_edge[i,j]=abs(C_edge[i]-C_edge[j])
    D_edge[j,i]=D_edge[i,j]
  }
}
#----------------predict edges weights using kNN based on the D_edge----------------#
idx.test<-c(1:N.edge)[-idx.sub]
kNN.edge<-function(idx.test,idx.train,data,metric,K){
  N.test<-length(idx.test)
  E.test<-data[idx.test,]
  pred_weight<-matrix(NA,nrow=N.test,ncol=3)
  pred_weight[,1]=E.test[,1]
  pred_weight[,2]=E.test[,2]
  for(i in 1:N.test){
    idx.e<-idx.test[i]
    Distance=metric[idx.e,idx.train]
    D=sort(unique(Distance))
    if(D[1]==0){
      kNN.train=idx.train[Distance%in%D[2:(K+1)]]
    }else{
      kNN.train=idx.train[Distance%in%D[1:K]]
    }
    weight_kNN.train<-data[kNN.train,3]
    pred_weight[i,3]<-mean(weight_kNN.train)
  }
  return(pred_weight)
}

data.test<-data[idx.test,]
pred_weight<-kNN.edge(idx.test,idx.sub,data,D_edge,K=4)
error=pred_weight[,3]-data.test[,3]
mean_error=mean(abs(error))
RMSE=sqrt(sum(error^2)/length(error))

#---------------for Bitcoin data-------------------#
pred_bitcoin<-round(pred_weight[,3],1)
error.rate<-sum(pred_bitcoin!=data.test[,3])/length(pred_bitcoin)
#---------------for Epinions data------------------#
epinion.pred<-function(weight){
  if(weight>=-1 & weight< -0.75){ pred=-1}
  if(weight>=-0.75 & weight< -0.25){pred=-0.5}
  if(weight>=-0.25 & weight<0.25){pred=0}
  if(weight>=0.25 & weight<0.75){pred=0.5}
  if(weight>=0.75 & weight<=1){pred=1}
  return(pred)
}
pred_epinion<-sapply(pred_weight[,3],epinion.pred)
error.rate=sum(pred_epinion!=data.test[,3])/length(pred_epinion)
#------------predict fairness using SVM-------------#
library("e1071")
C_edge_A<-C_edge[idx.sub]
C_edge_test<-C_edge[idx.test]

model_svm<-svm(x=C_edge_A,y=A[,3],kernel="linear")
pred_test<-predict(model_svm,C_edge_test)
error_svm<-pred_test-data.test[,3]
mean_error.svm=mean(abs(error_svm))
RMSE.svm<-sqrt(sum(error_svm^2)/length(error_svm))

