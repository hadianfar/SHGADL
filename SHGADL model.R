##SHGADL model
data2=read.csv("district level data.csv")
data2$PM2.5[data2$PM2.5>90]<-90
shape1 <- read_sf("Mashahd.shp")
# Create a neighborhood list (queen contiguity)
nb <- poly2nb(shape1)
names(nb) <- attr(nb, "region.id")
# Create the MRF precision matrix
data2$District <- as.factor(data2$District)
addlag <- crossbasis(data2$PM2.5, lag=7, argvar=list(fun="lin"),
                     arglag=list(fun="bs",degree=2))
# Fit a GAM with an MRF smooth
gam_model <- gam(CVD ~addlag+as.factor(screening)+as.factor(DOW)
                 +as.factor(holiday)+ns(time, 7)
                 +as.factor(covid)+offset(log(population))+ s( District, bs =  "mrf",
                          xt =  list(nb =nb), fx = TRUE), data = data2 ,method = 'REML', family=quasipoisson)
#predict
pred3dgam1 <- crosspred(addlag,gam_model,bylag=1)
predslgam1 <- crosspred(addlag,gam_model,by=0.2,bylag=0.2)
pdf("DLNM.mrf.pdf",height=6,width=8.5)
par(mar=c(1.5,1,1,1)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))
plot(pred3dgam1,zlab="RR",xlab=expression(PM2.5),ylab="Lag(day)",zlim=c(0.95,1.15),phi=35,theta=205,ltheta=170,
     shade=0.4,main="Exposure–lag–response association",cex.main=1.1)
par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))
plot(predslgam1,"overall",ylab="RR",xlab=expression("PM"[2.5]),
     ylim=c(0.8,1.6),lwd=2,col=2,main="Cumulative Exposure–response association",cex.main=1.1)
plot(predslgam1, "slices", var=10, xlab="Lag(day)", ylab="RR",col=2,lwd=2,cex.main=1.1,
     ylim=c(0.99,1.03),main="Lag-response association for 10-unit increase in PM2.5")
pred1.pm3 <- crosspred(addlag, gam_model, by=0.2,bylag=1,cumul=TRUE)
plot(pred1.pm3, "slices", xlab="Lag(day)",var=10, col=2,lwd=2,cex.main=1.1, cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association with a 10-unit increase in PM2.5",ylim=c(0.99,1.03))
dev.off()
