rm(list=ls())
setwd("/Users/lxing/Documents/Conference&Journal paper/2nd paper_SSN")
source("fairness_goodness_func.R")
#--------------------predict the goodness using kNN, 2nd method----------------------#
#metric of ratees is defined without involving the edge weights#
#using the goodness of ratees in subgraph to compute the metric#

data=read.table("epinion_data.txt",header=T)
Ratee=sort(unique(data[,2]))
N.ratee=length(Ratee)

#------select the subgraph A----------#
set.seed(112233)
idx.sub<-sort(sample(1:N.ratee,0.7*N.ratee,replace=F))
P_A<-Ratee[idx.sub] #ratees in A
A<-data[data[,2]%in%P_A,] #the subgraph
G_A<-fair_good.func(A,ite=200,epsilon=0.0001)$goodness #goodness of ratees in A

#----------------------compute the metric on ratees----------------------#
C_l<-rep(0,N.ratee)
l=sd(G_A[,2]) #set the tuning parameter
for(i in 1:N.ratee){
  p=Ratee[i]
  u_p<-data[,1][data[,2]==p]
  q<-unique(A[,2][A[,1]%in% u_p]) #q is the neighbor of p and q belongs to A
  G_q<-G_A[,2][P_A%in%q] # goodness of all q
  avg_Gq<-mean(G_q)
  
  C_l[i]<-sum(abs(G_q-avg_Gq)<=l)
}
D_ratee<-matrix(0,nrow=N.ratee, ncol=N.ratee)
for(i in 1:(N.ratee-1)){
  for(j in (i+1):N.ratee){
    D_ratee[i,j]=abs(C_l[i]-C_l[j])
    D_ratee[j,i]=D_ratee[i,j]
  }
}
#----------------predict goodness using kNN based on the D_ratee----------------#

kNN.ratee<-function(P.test,P.train,good_train, Ratee,metric,K){
  N.test=length(P.test)
  idx.train<-c(1:length(Ratee))[Ratee%in%P.train]
  pred_goodness<-matrix(NA,nrow=N.test,ncol=2)
  
  for(i in 1:N.test){
    p=P.test[i]
    idx.p=which(Ratee==p)
    Distance=metric[idx.p,idx.train]
    D=sort(unique(Distance))
    if(D[1]==0){
      kNN.train=P.train[Distance%in%D[2:(K+1)]]
    }else{
      kNN.train=P.train[Distance%in%D[1:K]]
    }
    good_kNN.train<-good_train[,2][P.train %in% kNN.train]
    pred_goodness[i,2]<-mean(good_kNN.train)
    pred_goodness[i,1]<-p
  }
  return(pred_goodness)
}

P.test<-Ratee[!Ratee%in%P_A]
goodness<-fair_good.func(data,ite=200,epsilon=0.0001)$goodness #fairness of the whole dataset
good_test<-goodness[goodness[,1]%in%P.test,]
pred_goodness<-kNN.ratee(P.test,P_A,G_A,Ratee,D_ratee,K=3)
error=pred_goodness[,2]-good_test[,2]
mean_error=mean(abs(error))
RMSE=sqrt(sum(error^2)/length(error))

#------------predict fairness using SVM-------------#
library("e1071")
C_l_A<-C_l[Ratee%in% P_A]
C_l_test<-C_l[!Ratee%in%P_A]

model_svm<-svm(x=C_l_A,y=G_A[,2],kernel="linear")
pred_test<-predict(model_svm,C_l_test)
error_svm<-pred_test-good_test[,2]
mean_error.svm=mean(abs(error_svm))
RMSE.svm<-sqrt(sum(error_svm^2)/length(error_svm))
