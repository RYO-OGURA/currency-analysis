# currency-analysis
currency analysis


##The simple forecasting model  of the JP Yen and / US Dollar exchange rate

```{r echo=FALSE, message=FALSE }
library(quantmod)

## EXUSjp: US/jp exchange rate 
##CPIAUCNS: Japan monthly CPI NSA
## JPNPROINDMISMEI: Japan monthly industrial production SA
## CPIAUCSL: US CPI NSA
## INDPRO: US Industricl Production: SA
getSymbols(c("EXJPUS","CPIAUCSL", "INDPRO", "CPIAUCNS", "JPNPROINDMISMEI") ,src = "FRED")

date.jpcpi <- index(CPIAUCNS)
jpcpi <- as.numeric(CPIAUCNS[, 1])
jpcpi <- as.xts(jpcpi, order.by = date.jpcpi)
fx.r <- Delt(EXJPUS)[-1]
jpcpi.r <- Delt(jpcpi)[-1]
uscpi.r <- Delt(CPIAUCSL)[-1]
jpprod.r <- Delt(JPNPROINDMISMEI)[-1]
usprod.r <- Delt(INDPRO)[-1]

data <- merge(fx.r, jpcpi.r,join = "inner")
data <- merge(data, uscpi.r,join = "inner")
data <- merge(data, jpprod.r,join = "inner")
data <- merge(data, usprod.r, join = "inner")
names(data) <- c("fx.r", "jpcpi.r", "uscpi.r", "jpprod.r", "usprod.r")

diff.cpi <- data$jpcpi.r - data$uscpi.r
diff.prod <- data$jpprod.r - data$usprod.r

lm <- lm(data$fx.r ~ diff.cpi + diff.prod)
summary(lm)

####################lagged model################
###create dataframe
lag.diff.cpi<-lag(diff.cpi)
lag.diff.prod<-(lag(diff.prod))
lm.lag<-data.frame(data$fx.r,lag.diff.cpi,lag.diff.prod)

##clean up dataset
lm.lag<-na.omit(lm.lag)

names(lm.lag)<-c("deltajpus","lagcpidif","lagproddif")

laglm<-lm(deltajpus~lagcpidif+lagproddif, lm.lag)
summary(lm.lag)

###graphing###########

with(lm.lag,plot(lagcpidif, deltajpus, col="blue"))
abline(laglm, lwd=3, col="red")
with(lm.lag,plot(lagproddif,deltajpus))

#####graphing the density function of deltajpus
d.deltajpus<-with(lm.lag,density(deltajpus))
plot(d.deltajpus,main="delta JPY USD")
polygon(d.deltajpus, col="red", border="blue", lwd=3) 

The first model doesn't explain anything.
The second (lagged) model doesn't explain anything much either.
However, the economic growth differential variable is weakly significant(.05).
This means that the increase in the previous monthly percentage change in the economic growth differnetial(Japan economic growth minus U.S. economic growth) will depreciate JP Yen. 
