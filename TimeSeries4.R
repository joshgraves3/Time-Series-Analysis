#problem 1

year = seq(1993,2010,1)
storesOpen = c(38,45,61,80,108,141,186,241,311,396,519,629,721,809,888,971,1037,1100)

storesYearZero = year - 1993

storesYearZero

#plot(year,storesYearZero)
plot(ts(storesOpen))

#linear trend
linTrendEq = lm(storesOpen~storesYearZero)
predictLin = predict(linTrendEq)
lines(ts(predictLin),type="l",col="red",lwd=3)

predict1 = 18
predict2 = 19

predict2011 = linTrendEq[[1]][2]*predict1 + linTrendEq[[1]][1]
predict2012 = linTrendEq[[1]][2]*predict2 + linTrendEq[[1]][1]

predict2011
predict2012

#quadratic trend
quadTrendEq = lm(storesOpen~storesYearZero + I(storesYearZero^2))
predictQuad = predict(quadTrendEq)
lines(ts(predictQuad),type="l",col="green",lwd=3)

predict2011 = predict15 = quadTrendEq[[1]][3]*(predict1^2) + quadTrendEq[[1]][2]*predict1 + quadTrendEq[[1]][1]
predict2012 = predict15 = quadTrendEq[[1]][3]*(predict2^2) + quadTrendEq[[1]][2]*predict2 + quadTrendEq[[1]][1]

predict2011
predict2012

#exponential trend
expTrendEq = lm(log10(storesOpen)~storesYearZero)
predictExp = predict(expTrendEq)
predictR = 10^predictExp
lines(ts(predictR),type="l",col="blue",lwd=3)

beta0 = 10^expTrendEq[[1]][1]
beta1 = 10^expTrendEq[[1]][2]

predict2011 = (beta1^predict1) * beta0
predict2012 = (beta1^predict2) * beta0

predict2011
predict2012

#each forecast is different because each model uses a different calculation
#based on the graph, the quadratic trend is the most accurate because it most closely matches with the given plot

#problem 2

baseballYears = seq(2000,2010,1)
baseballSalary = c(1.99,2.29,2.38,2.58,2.49,2.63,2.83,2.92,3.13,3.26,3.27)

baseballYearZero = baseballYears - 2000

plot(ts(baseballSalary))

predict1 = 11

#linear trend
baseballLin = lm(baseballSalary~baseballYearZero)
predictBbLin = predict(baseballLin)
lines(ts(predictBbLin),type="l",col="red",lwd=3)

predictBb2011 = baseballLin[[1]][2]*predict1 + baseballLin[[1]][1]
predictBb2011

#quadratic trend
baseballQuad = lm(baseballSalary~baseballYearZero + I(baseballYearZero^2))
predictBbQuad = predict(baseballQuad)
lines(ts(predictBbQuad),type="l",col="green",lwd=3)

predictBb2011 = baseballQuad[[1]][3]*(predict1^2) + baseballQuad[[1]][2]*predict1 + baseballQuad[[1]][1]
predictBb2011

#exponential trend
baseballExp = lm(log10(baseballSalary)~baseballYearZero)
predictBbExp = predict(baseballExp)
predictS = 10^predictBbExp
lines(ts(predictS),type="l",col="blue",lwd=3)

beta0 = 10^baseballExp[[1]][1]
beta1 = 10^baseballExp[[1]][2]

predictBb2011 = (beta1^predict1) * beta0
predictBb2011

#all 3 models are very close in their predictions, but based on the graph, the exponential model fits the best

#problem 3
travelData = read.csv(file="/Users/Josh/Google Drive/TimeSeriesAnalysis/Assignments/Assignment4/travel.csv",header=TRUE)

plot(ts(travelData$Travel),type='o')

dataPts = dim(travelData)[1] - 1

travelData$codedMonth = seq(0,dataPts,1)

jan = rep(c(1,0,0,0,0,0,0,0,0,0,0,0),6)
jan = c(jan,c(1,0,0,0,0,0,0,0))
travelData$Jan = jan

feb = rep(c(0,1,0,0,0,0,0,0,0,0,0,0),6)
feb = c(feb,c(0,1,0,0,0,0,0,0))
travelData$Feb = feb

mar = rep(c(0,0,1,0,0,0,0,0,0,0,0,0),6)
mar = c(mar,c(0,0,1,0,0,0,0,0))
travelData$Mar = mar

apr = rep(c(0,0,0,1,0,0,0,0,0,0,0,0),6)
apr = c(apr,c(0,0,0,1,0,0,0,0))
travelData$Apr = apr

may = rep(c(0,0,0,0,1,0,0,0,0,0,0,0),6)
may = c(may,c(0,0,0,0,1,0,0,0))
travelData$May = may

jun = rep(c(0,0,0,0,0,1,0,0,0,0,0,0),6)
jun = c(jun,c(0,0,0,0,0,1,0,0))
travelData$Jun = jun

jul = rep(c(0,0,0,0,0,0,1,0,0,0,0,0),6)
jul = c(jul,c(0,0,0,0,0,0,1,0))
travelData$Jul = jul

aug = rep(c(0,0,0,0,0,0,0,1,0,0,0,0),6)
aug = c(aug,c(0,0,0,0,0,0,0,1))
travelData$Aug = aug

sept = rep(c(0,0,0,0,0,0,0,0,1,0,0,0),6)
sept = c(sept,c(0,0,0,0,0,0,0,0))
travelData$Sept = sept

oct = rep(c(0,0,0,0,0,0,0,0,0,1,0,0),6)
oct = c(oct,c(0,0,0,0,0,0,0,0))
travelData$Oct = oct

nov = rep(c(0,0,0,0,0,0,0,0,0,0,1,0),6)
nov = c(nov,c(0,0,0,0,0,0,0,0))
travelData$Nov = nov

travelModel = lm(log10(travelData$Travel)~codedMonth + travelData$Jan + travelData$Feb + travelData$Mar + travelData$Apr + travelData$May + travelData$Jun + travelData$Jul + 
                   travelData$Aug + travelData$Sept + travelData$Oct + travelData$Nov)

p = predict(travelModel)
pr = 10^p

lines(ts(pr),col="red", type="o")

#prediction for august
fitValueAug = pr[80]
fitValueAug

#predictions for last four months
septPredic = 10^(travelModel$coefficients[1] + travelModel$coefficients[2]*80 + travelModel$coefficients[11])
octPredic = 10^(travelModel$coefficients[1] + travelModel$coefficients[2]*81 + travelModel$coefficients[12])
novPredic = 10^(travelModel$coefficients[1] + travelModel$coefficients[2]*82 + travelModel$coefficients[13])
decPredic = 10^(travelModel$coefficients[1] + travelModel$coefficients[2]*83)
septPredic
octPredic
novPredic
decPredic

#problem 4
silverQ = read.csv(file="/Users/Josh/Google Drive/TimeSeriesAnalysis/Assignments/Assignment4/quarter.csv",header=TRUE)

plot(ts(silverQ$Price),type='o')

dataPts = dim(silverQ)[1] - 1

silverQ$codedQuarter = seq(0,dataPts,1)

silverQ$q1 = c(1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0)
silverQ$q2 = c(0,1,0,0, 0,1,0,0, 0,1,0,0, 0,1,0,0, 0,1,0,0, 0,1,0,0)
silverQ$q3 = c(0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0)

silverQ

silverQModel = lm(log10(silverQ$Price)~silverQ$codedQuarter + silverQ$Q1 + silverQ$Q2 + silverQ$Q3)

s = predict(silverQModel)
sr = 10^s
lines(ts(sr),col="green",type='o')

silverQModel$coefficients

#fitted value for last quarter of 2009
fitValue2009 = sr[24]
fitValue2009

#forecasts for all four quarters of 2010
q1Predic = 10^(silverQModel$coefficients[1] + silverQModel$coefficients[2]*24 + silverQModel$coefficients[3])
q2Predic = 10^(silverQModel$coefficients[1] + silverQModel$coefficients[2]*25 + silverQModel$coefficients[4])
q3Predic = 10^(silverQModel$coefficients[1] + silverQModel$coefficients[2]*26 + silverQModel$coefficients[5])
q4Predic = 10^(silverQModel$coefficients[1] + silverQModel$coefficients[2]*27)

q1Predic
q2Predic
q3Predic
q4Predic

#these forecasts were not as accurate as they were for the monthly above, because an exponential model doesn't fit this data as well


