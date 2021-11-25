fair_good.func<-function(data,ite,epsilon){ 
  rater=sort(unique(data[,1])) #the set of origins 
  N.rater=length(rater)
  ratee=sort(unique(data[,2])) #the set of terminals
  N.ratee=length(ratee)
  #set the initial value of fairness and goodness
  fair<-rep(1,N.rater)
  good<-rep(1,N.ratee)
  for(t in 1:ite){
    #fair_t is the fairness at the t step, fair is fairness at t-1 step
    fair_t<-rep(NA,N.rater) 
    #good_t is the goodness at the t step
    good_t<-rep(NA,N.ratee)
    for(i in 1:N.rater){
      u=rater[i]
      data.u=data[data[,1]==u,]
      p=data.u[,2]
      weight.up=data.u[,3]
      good.p<-good[ratee%in%p] #use goodness at t-1 step to compute fair_t1
      fair_t[i]<-1-sum(abs(weight.up-good.p))/2/length(p)
    }
    for(j in 1:N.ratee){
      q=ratee[j]
      data.q=data[data[,2]==q,]
      v=data.q[,1]
      weight.vq=data.q[,3]
      fair.v<-fair_t[rater%in%v] #use fairness at t step
      good_t[j]<-sum(fair.v*weight.vq)/length(v)
    }
    if((max(fair_t-fair)<epsilon) & (max(good_t-good)<epsilon)){
      break
    }else{
      fair=fair_t
      good=good_t
    }
  }
  fairness<-matrix(c(rater,fair_t),ncol=2,byrow=F)
  goodness<-matrix(c(ratee,good_t),ncol=2,byrow=F)
  return(list("fairness"=fairness,"goodness"=goodness))
}