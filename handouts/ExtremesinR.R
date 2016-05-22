### Exercises for Extremes in R Lab 24 July 2015.

# No commenting!

### 1 Stationary Block Maxima
data(Fort)
fc.amax <- blockmaxxer(Fort, blocks=Fort$year, which ="Prec")
fit.fc0 <- fevd(Prec, data=fc.amax)
plot(fit.fc0, "qq")
ci(fit.fc0, type="parameter")
profliker(fit.fc0, type="parameter", which.par=3, xrange=c(-0.1,0.3))

# Repeat for Boulder Temperatures
# Just in case the data aren't still loaded
obs <- read.table("smalldata.txt", header=T)
obs$year <- year(obs$date)
b.tmax <- na.omit(blockmaxxer(obs, blocks=obs$year, which="TxC"))
plot(TxC~year, data=b.tmax, t="l", col="orange", lwd=3, ylab="Temp dec C")
trend <- lm(TxC~year, data=b.tmax)
abline(trend, col="red", lty=2)
fit.bgev <- fevd(TxC, data=b.tmax)
plot(fit.bgev)

#### 2 Stationary Peaks over Threshold
quantile(Fort$Prec, seq(0.9,0.99,0.01))
mrlplot(Fort$Prec)
threshrange.plot(Fort$Prec, c(0.2,0.5), nint=10)
par(mfrow=c(1,1))
plot(Fort$Prec, type="h", col="blue")
abline(h=0.395, col="red", lty=2, lwd=2)
gp.fit0 <- fevd(Prec, data=Fort, type="GP", threshold=0.395)
par(mfrow=c(1,2))
plot(gp.fit0, "qq2", main="GP Fit")	
plot(fit.fc0, "qq2", main="GEV Fit")	

tmp <- extremalindex(Fort$Prec, 0.395, method="runs", run.length=9, blocks=Fort$year)
ci(tmp)
Fort.dc <- decluster(Fort$Prec, 0.395)
plot(Fort.dc)
abline(h=0.395, col="red")

gp.fit1 <- fevd(Fort.dc, type="GP", threshold=0.395)
fpois(Fort.dc>0.395)
length(Fort.dc[Fort.dc>0.395])/length(Fort.dc)
mean(Fort.dc>0.395)
pp.fit1 <- fevd(Fort.dc, type="PP", threshold=0.395)
pars <- findpars(pp.fit1)
mu <- pars$location[1]
sigma <- pars$scale[1]
xi <- pars$shape[1]

lambda.hat <- (1 + (xi/sigma)*(0.395-mu))^(-1/xi)
1/lambda.hat

#### Linear temporal Trends
data(Denmint)
Denmint$negMin <- -(Denmint$Min)
Denmint2 <- blockmaxxer(Denmint, blocks=Denmint$Year, which="negMin")
plot(negMin~Year, data=Denmint2, t="l")
lmfit <- lm(negMin~Year, data=Denmint2)
abline(lmfit, lty=2, col=2)
dm.fit0 <- fevd(negMin, data=Denmint2) 
plot(dm.fit0)
ci(dm.fit0, type="parameter", which.par=3)

dm.fit1 <- fevd(negMin, data=Denmint2, location.fun= ~Time)
lr.test(dm.fit0, dm.fit1)

dm.fit2 <- fevd(negMin, data=Denmint2, location.fun = ~Time, scale.fun= ~Time, use.phi=TRUE)
lr.test(dm.fit1, dm.fit2)
lr.test(dm.fit0, dm.fit2)

####Cyclic Variation

Fort$PrecGTu <- Fort$Prec > 0.395
fit.P <- glm(PrecGTu ~ sin(2 * pi * tobs / 365.25) + cos(2 * pi * tobs / 365.25), data = Fort, family = poisson)
pp.fit2 <- fevd(Prec, data=Fort, location.fun=~ sin(2 * pi * tobs / 365.25) + cos(2 * pi * tobs / 365.25), type="PP", threshold=0.395)					 
lr.test(pp.fit1, pp.fit2)
pp.fit3 <- fevd(Prec, Fort, threshold=0.395,
						scale.fun=~sin(2 * pi * tobs/365.25) + cos(2 * pi * tobs/365.25),
						type="PP", use.phi=TRUE, verbose=TRUE)
summary(pp.fit3)
plot(pp.fit2)
plot(pp.fit2, "trace")
plot(pp.fit3)
