####################################################################### Modeling ##################################
######################################################### Q-AIC FUNCTION
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}

########################################################## GADL
data2$PM2.5[data2$PM2.5>90]<-90
vk <- equalknots(data2$PM2.5,nk=1)
lk <- logknots(7,nk=1)
cb1.pm <- crossbasis(data2$PM2.5,lag=7,argvar=list(fun="lin"),arglag=list(fun="bs",degree=2,knots=lk))


model1 <- glm(CVD ~ cb1.pm +as.factor(screening)+as.factor(DOW)+as.factor(holiday)+ns(data2$time, 7)+as.factor(covid),family=quasipoisson(), data2)
summary(model1)
fqaic(model1)
pred1.pm1 <- crosspred(cb1.pm,model1)
pred1.pm <- crosspred(cb1.pm, model1, by=0.2,bylag=0.2)
# plot the lag-response curves for specific and incremental cumulative effects
pdf("DLNMout.pdf",height=6,width=8.5)
par(mar=c(1.5,1,1,1)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))

plot(pred1.pm1,xlab="PM2.5",zlab="RR",ylab="Lag(day)",zlim=c(0.95,1.15),phi=35,theta=205,ltheta=170,
     shade=0.4,main="Exposure–lag–response association",cex.main=1.1)
par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))
plot(pred1.pm,"overall",ylab="RR",xlab=expression("PM"[2.5]),col=2,lwd=2,cex.main=1.1,
     ylim=c(0.8,1.6),lwd=1.5,main="Cumulative Exposure–response association")

plot(pred1.pm, "slice", var=10,  ylab="RR",col=2,lwd=2,xlab="Lag(day)",ylim=c(0.99,1.03),
     main="Lag-response association with a 10-unit increase in PM2.5",cex.main=1.1)
pred1.pm2 <- crosspred(cb1.pm, model1, by=0.2,bylag=0.2,cumul=TRUE)
plot(pred1.pm2, "slices", var=10, col=2,lwd=2, cumul=TRUE,xlab="Lag(day)", ylab="Cumulative RR",
     main="Cumulative association with a 10-unit increase in PM2.5",ylim=c(0.99,1.03),cex.main=1.1)
dev.off()

