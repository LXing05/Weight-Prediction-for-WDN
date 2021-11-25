rm(list=ls())
setwd("/Users/lxing/Documents/Conference&Journal paper/2nd paper_SSN")
source("fairness_goodness_func.R")
#--------------------predict the fairness using kNN and SVM, 2nd method----------------------#
#metric of raters is defined without involving the edge weights#
#using the fairness of raters in subgraph to compute the metric#

data=read.table("bitcoin_data.txt",header=T)
Rater=sort(unique(data[,1]))
N.rater=length(Rater)

#------select the subgraph A----------#
set.seed(11111)
idx.sub<-sort(sample(1:N.rater,0.8*N.rater,replace=F))
V_A<-Rater[idx.sub] #origins in A
A<-data[data[,1]%in% V_A,] #the subgraph 
F_A<-fair_good.func(A,ite=200,epsilon=0.0001)$fairness #fairness of originss in A
#----------------------compute the metric on origins----------------------#
C_h<-rep(0,N.rater)
h=sd(F_A[,2]) #set the tuning parameter
for(i in 1:N.rater){
  u=Rater[i]
  p_u<-data[,2][data[,1]==u]
  v<-unique(A[,1][A[,2]%in%p_u]) #v is the neighbor of u and v belongs to A
  F_v<-F_A[,2][V_A%in%v] # fairness of all v
  avg_Fv<-mean(F_v)
  
  C_h[i]<-sum(abs(F_v-avg_Fv)<=h)
}

D_rater<-matrix(0,nrow=N.rater,ncol=N.rater)
for(i in 1:(N.rater-1)){
  for(j in (i+1):N.rater){
    D_rater[i,j]=abs(C_h[i]-C_h[j])
    D_rater[j,i]=D_rater[i,j]
  }
}
#----------------predict fairness using kNN based on the D_rater----------------#

kNN.rater<-function(U.test,U.train,fair_train, Rater,metric,K){
  N.test=length(U.test)
  pred_fairness<-matrix(NA,nrow=N.test,ncol=2)
  
  idx.train<-c(1:length(Rater))[Rater%in%U.train]
  
  for(i in 1:N.test){
    u=U.test[i]
    idx.u=which(Rater==u)
    Distance=metric[idx.u,idx.train]
    D=sort(unique(Distance))
    if(D[1]==0){
      idx.kNN.train=idx.train[Distance%in%D[2:(K+1)]]
    }else{
      idx.kNN.train=idx.train[Distance%in%D[1:K]]
    }
    kNN.train<-Rater[idx.kNN.train]
    fair_kNN.train<-fair_train[,2][U.train %in% kNN.train]
    pred_fairness[i,2]<-mean(fair_kNN.train)
    pred_fairness[i,1]<-u
  }
  return(pred_fairness)
}

U.test=Rater[!Rater%in%V_A]
fairness<-fair_good.func(data,ite=200,epsilon=0.0001)$fairness #fairness of the whole dataset
fair_test<-fairness[fairness[,1]%in%U.test, ]
pred_fairness<-kNN.rater(U.test,V_A,F_A,Rater,D_rater,K=3)
error=pred_fairness[,2]-fair_test[,2]
mean_error=mean(abs(error)) # mean of the absolute value of error
RMSE=sqrt(sum(error^2)/length(error))

cor(pred_fairness[,2],fair_test[,2],method="spearman")
#------------predict fairness using SVM-------------#
library("e1071")
C_h_A<-C_h[Rater%in% V_A]
C_h_test<-C_h[!Rater%in%V_A]

model_svm<-svm(x=C_h_A,y=F_A[,2],kernel="linear")
pred_test<-predict(model_svm,C_h_test)
error_svm<-pred_test-fair_test[,2]
mean_error.svm=mean(abs(error_svm))
RMSE.svm<-sqrt(sum(error_svm^2)/length(error_svm))
