options(width = 1500)
library(tsfa)
library(nFactors)
library(vars)
library(readr)
library(foreach)
library(doParallel)
library(lubridate)
library(forecast)
library(animation)

load('tabla_festivos.RData')
des = c("Performance of algorithm using TSFA, VAR and ensembling ets and arima
        with extra time features")
saveHTML({


{
  load('tabla_festivos.RData')
  param = 310
  dif = 364-param
  datos=matrix(news$`General [kWh]`,nrow=param,ncol=96, byrow = T)
  predicc=matrix(news$`General [kWh]`[-c(1:(param*96))],nrow=dif,ncol=96, byrow = T)
  
  optnf=nBartlett(cor(datos),N = ncol(datos), alpha=0.05, cor=TRUE, details=TRUE, correction=TRUE)
  nf=optnf$nFactors[1]
  
  
  ##domingos y festivos
  valdom=ifelse(weekdays((news$Date))=="domingo",1,0)
  datdom=matrix(valdom,nrow = param,ncol = 96,byrow = T)[,1]
  preddom=matrix(valdom[-c(1:(param*96))],nrow=dif,ncol=96, byrow = T)[,1]
  
  ##festivos y fiestas navidad y sem santa
  festiusdat=matrix(news$festivo,nrow = param,ncol = 96,byrow = T)[,1]
  festiuspred=matrix(news$festivo[-c(1:(param*96))],nrow=dif,ncol=96, byrow = T)[,1]
  
  hwfiesdat=matrix(news$holw,nrow=param,ncol=96, byrow = T)[,1]
  hwfiespred=matrix(news$holw[-c(1:(param*96))],nrow=dif,ncol=96, byrow = T)[,1]
  
  j<-1
  datos2 <- datos
  datdom2<-datdom
  fest2 <- festiusdat
  nad2 <- hwfiesdat
  p<-vector()
  factorspred <- matrix(nrow = nf,ncol = nrow(predicc))
  resppred <- matrix(nrow = nrow(predicc),ncol = 96)
  resppredarima <- matrix(nrow = nrow(predicc),ncol = 96)
  for(i in nrow(datos):363){
    
    factores<-estTSF.ML(datos2,nf,normalize=F,maxit=30000)
    fmodel<-factors(factores)
    serief1ts <- ts(fmodel)
    ##Mejor modelo con const.
    varf1 <- VAR(serief1ts,p=1, type="const",exogen = as.matrix(cbind(dat=datdom2,fest=fest2,nad=nad2)))
    
    
    ##Autoarima with also three covariables auxiliary. And ets with trend of 0.2
    prediccarima <- vector()
    for(t in 1:ncol(serief1ts)){
      vararima <- auto.arima(serief1ts[,t],max.p = 7,max.q = 0,stationary = TRUE,xreg = cbind(dat=datdom2,fest=fest2,nad=nad2))
      varets <- as.vector(forecast(ets(serief1ts[,t],model='AAN'),h = 1)[2]$mean)
      ab<-predict(vararima,newxreg = cbind(preddom[j],festiuspred[j],hwfiespred[j]))
      #prediccarima <- c(prediccarima,ab$pred)
      prediccarima <- c(prediccarima,c(1*ab$pred+0*varets))
    }
    
    resppredarima[j,]<-t(factores$loadings%*%prediccarima)
    
    
    aa<-predict(varf1,dumvar = as.matrix(cbind(preddom[j],festiuspred[j],hwfiespred[j])),n.ahead = 1)
    
    f1<-aa$fcst$Factor.1[,1]
    f2<-aa$fcst$Factor.2[,1]
    f3<-aa$fcst$Factor.3[,1]
    f4<-aa$fcst$Factor.4[,1]
    f5<-aa$fcst$Factor.5[,1]
    
    
    fac <- t(matrix(c(f1,f2,f3,f4,f5),nrow=length(f1),ncol=5))
    factorspred[,j]<-fac
    p<-c(p,t(factores$loadings%*%factorspred[,j]))
    resppred[j,]<-t(factores$loadings%*%factorspred[,j])
    # print(j)
    datos2 <- rbind(datos2,predicc[j,])
    datdom2 <- c(datdom2,preddom[j])
    fest2 <- c(fest2,festiuspred[j])
    nad2 <- c(nad2,hwfiespred[j])
    

    par(mfrow=c(2,2))
    ## plot individual
    plot(ts(as.vector(t(datos2[-c(1:(nrow(datos2)-1)),])),freq=96),type="l",ylab='',
         xlab=paste0('Time | RMSE=',round(Metrics::rmse(as.vector(t(datos2[-c(1:(nrow(datos2)-1)),])),
                                                            as.vector(t(resppred[j,]))),2)),
         lty=2,col="blue",
         ylim=c(0,max(c(as.vector(t(datos2[-c(1:(nrow(datos2)-1)),])),as.vector(t(resppred[j,]))))))
    title("Individual VARX Model")
    lines(ts(as.vector(t(resppred[j,])),freq=96))

    ##plot colectivo
    plot(ts(as.vector(t(datos2[-c(1:nrow(datos)),])),freq=96),type="l",lty=2,col="blue",ylab='')
    title("VARX Collective Model")
    lines(ts(as.vector(t(resppred)),freq=96))

    ##plot individual arima
    plot(ts(as.vector(t(datos2[-c(1:(nrow(datos2)-1)),])),freq=96),type="l",ylab='',
         xlab=paste0('Time | RMSE=',round(Metrics::rmse(as.vector(t(datos2[-c(1:(nrow(datos2)-1)),])),
                                                        as.vector(t(resppredarima[j,]))),2)),
         lty=2,col="red",ylim=c(0,max(c(as.vector(t(datos2[-c(1:(nrow(datos2)-1)),])),as.vector(t(resppredarima[j,]))))))
    title("Ensemble Model")
    lines(ts(as.vector(t(resppredarima[j,])),freq=96))

    ##plot colectivo arima
    plot(ts(as.vector(t(datos2[-c(1:nrow(datos)),])),freq=96),type="l",lty=2,col="red",ylab='')
    title("Ensemble Collective Model")
    lines(ts(as.vector(t(resppredarima)),freq=96))
    j<-j+1
    
  }
}},
autoplay = FALSE, 
interval = 0.5, imgdir = "animate_images", htmlfile = "animate_with_exo.html", 
ani.height = 600, ani.width = 800, title = "Performance of algorithm", 
description = des)
