#open csv data
beerSales = read.csv(file="/Users/Josh/Google Drive/TimeSeriesAnalysis/beersales.csv",header=TRUE)

#transform data to a time series
beerSales = ts(beerSales,start=1958,freq=12)

#plot the time series
plot(beerSales)

#decompose the time series into...
beerSales.decom = decompose(beerSales,type="mult")

#trend
trend = beerSales.decom$trend

#seasonal
seasonal = beerSales.decom$seasonal

#residuals
resids = beerSales.decom$random

#plot trend with seasonal effect
ts.plot(cbind(trend,trend*seasonal),lty=1:2)

#plot randomly distributed time series
set.seed(2)
normalTS = rnorm(48)
ts.plot(normalTS)

set.seed(2)
chisqTS = rchisq(48,2)
ts.plot(chisqTS)

set.seed(20)
tTS = rt(48,5)
ts.plot(tTS)