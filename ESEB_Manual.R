library(forecast)
library(ggplot2)
library(reshape2)

n=nrow(price)
m=n-4
o=m+1
out.sample=as.matrix(price)[o:n]
testing=out.sample
testing_df<-data.frame(testing)
names(testing_df)<-c("price")
in.sample=as.matrix(price)[1:m]
training=in.sample
dataset = in.sample
dataset_df<-data.frame(dataset) # in sample
names(dataset_df)<-c("price")

prediction<-function(x,alpha){
  n1=nrow(x)
  f<-c()
  p<-c()
  index1<-c()
  index2<-c()
  event1<-c(8,19,31,43,54,66,78)
  event2<-c(12,24,36,48,60,72)
  f[[1]]<-(x[1,1]*alpha)+((1-alpha)*x[1,1])
  for (i in 1:n1){
    f[[i+1]]=((x[i,1]*alpha)+((1-alpha)*f[[i]]))
  }
  for (i in event1){
    indexx1=x[i,1]/f[[i]]
    index1=cbind(index1,indexx1)
  }
  for(i in event2){
    indexx2=x[i,1]/f[[i]]
    index2=cbind(index2,indexx2)
  }
  G1<-mean(index1)
  G2<-mean(index2)
  for (i in 1:n1){
    if (i==8||i==19||i==31||i==43||i==54||i==66||i==78){
      p[[i]]=f[[i]]*G1
    } else if (i==12||i==24||i==36||i==48||i==60||i==72){
      p[[i]]=f[[i]]*G2
    } else {
      p[[i]]=f[[i]]
    }
  }
  df<-data.frame(unlist(p))
  names(df)<-c("Prediction")
  a <- abs((df[,1]-x[,1])/df[,1])
  mape <- (sum(a)/n1)*100
  names(x)<-c("Actual")
  df_total<-cbind(x,df)
  cat("Grup Index Event 1:",G1,"\n")
  cat("Grup Index Event 2:",G2,"\n")
  cat("Predicted Data Using ESEB:","\n")
  print(df)
  cat("MAPE:", mape,"%","\n")
  save(df_total,file="df_total.Rda")
  # PLOT
  time<-data.frame(time=1:n1)
  datafix<-cbind(time,df_total)
  dataa<-reshape2::melt(datafix, id.var="time")
  ggplot(dataa, aes(x=time, y=value, col=variable))+geom_line()+labs(title="Actual Vs Predicted Data Training Using ESEB", y="Price", x="Time")
}

load("df_total.Rda")

prediction_testing<-function(x1,x2,alpha,G2){
  n2<-nrow(x1)
  ft<-c()
  pt<-c()
  ft[[1]]<-((x2[80,1]*alpha)+((1-alpha)*x2[[80,2]]))
  for (i in 1:n2){
    ft[[i+1]]=((x1[i,1]*alpha)+((1-alpha)*ft[[i]]))
  }
  for (i in 1:n2){
    if (i==4){
      pt[[i]]=ft[[i]]*G2
    } else {
      pt[[i]]=ft[[i]]
    }
  }
  dft<-data.frame(unlist(pt))
  names(dft)<-c("Prediction Testing")
  at <- abs((dft[,1]-x1[,1])/dft[,1])
  mape <- (sum(at)/n2)*100
  names(x1)<-c("Actual Testing")
  dft_total<-cbind(x1,dft)
  
  b <-((dft[,1]-x1[,1])^2)
  mse <- (sum(b)/n2)
  
  cat("Prediction Testing Using ESEB:","\n")
  print(dft)
  cat("MAPE:", mape,"%","\n")
  cat("MSE:", mse,"\n")
  save(dft_total,file="dft_total.Rda")
  
  
  time1<-data.frame(time1=1:n2)
  out_total<-cbind(time1,dft_total)
  dataaa<-reshape2::melt(out_total, id.var="time1")
  ggplot(dataaa, aes(x=time1, y=value, col=variable))+geom_line()+labs(title="Actual Vs Prediction Data Testing Using ESEB", y="Price", x="Time")
}
prediction_testing(testing_df,df_total,0.5695562,1.080454)

load("dft_total.Rda")

predict_eseb<-function(x3,alpha,G1,G2,period,e1,e2){
  fp<-c()
  pp<-c()
  predictions<-c()
  fp[[1]]<-(x3[4,1]*alpha)+((1-alpha)*x3[4,2])
  for (i in 1:period){
    fp[[i+1]]=((fp[i,1]*alpha)+((1-alpha)*fp[[i]]))
  }
  for (j in e1){
    pp[[j,1]]=fp[[j,1]]*G1
    predictions<-rbind(predictions, pp[[j,1]])
  }
  for (k in e2){
    pp[[k,1]]=fp[[k,1]]*G2
    predictions<-rbind(predictions, pp[[k,1]])
  }
  
  df_predict<-data.frame(unlist(predictions))
  names(df_predict)<-c("Special Event Predictions ")
  cat("Data Ramalan Special Event:","\n")
  print(df_predict)
  save(df_predict, file="df_predict.Rda")
}
e1<-c(5)
e2<-c(12)
predict_eseb(dft_total,0.5695562,1.106226,1.080454,12,e1,e2)

