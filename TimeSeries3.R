autoData = read.csv(file="/Users/Josh/Google Drive/TimeSeriesAnalysis/Assignments/Assignment3/Auto.csv",header=TRUE)

#parse data into horsepower(x) and mpg(y)
mpg = autoData[,2]
hp = autoData[,5]

#closed form 1: mean of x, y, x*y, and x^2
mpgMultHp = mpg*hp
sumMpg = sum(mpg)
sumHp = sum(hp)
sumBoth = sum(mpgMultHp)
len = length(mpg)
hpSq = hp*hp
sumHpSq = sum(hpSq)

cfs1Slope = ((1/len)*(sumBoth - (sumMpg*sumHp)/len))/((1/len)*(sumHpSq - (sumHp*sumHp)/len))
cfs1Int = (sumMpg/len) - ((cfs1Slope*sumHp)/len)

cfs1Slope
cfs1Int

#closed form 2: correlation coeff and stand. dev.
sdHp = sd(hp)
sdMpg = sd(mpg)

sdHp
sdMpg

corCoef = cor(hp,mpg)

cfs2Slope = (corCoef*sdMpg)/sdHp
cfs2Int = cfs1Int

cfs2Slope
cfs2Int

#closed form 3: matrices Y = AX + E, E = Y - XA
y = mpg

ones = rep(1,392)
x = cbind(ones,hp)
x = matrix(x,nrow=392)

cfs3Slope = solve((t(x)%*%x)) %*% (t(x)%*%y)

cfs3Slope

#lm function
summary(lm(mpg~hp))

#standard error of regression - sd of residuals, sqrt(sum(residuals^2)/(n - 2)))
res = mpg - (cfs1Slope*hp + cfs1Int)
sumSqResid = sum(res^2)
regSE = sqrt(sumSqResid/(len - 2))

regSE

#standard error of predictor variable (hp)
hpMean = sum(hp)/len
meanResids = (hp - hpMean)^2
predictSE = regSE/sqrt(sum(meanResids))

predictSE

#t-value of predictor variable (hp)
tVal = cfs1Slope/predictSE

tVal

#p-value of predictor variable (hp)
pVal = dt(tVal,len - 2)

pVal

# There is not really a linear relationship between mpg and horsepower
# no the residuals are not normally distributed
qqnorm(res)
qqline(res)

marginOfError = qt(.975,len-2) * predictSE

marginOfError

aboveConfidence = cfs1Slope + marginOfError
belowConfidence = cfs1Slope - marginOfError
fReg = function(x) cfs1Slope*x + cfs1Int
fBelow = function(x) belowConfidence*x + cfs1Int
fAbove = function(x) aboveConfidence*x + cfs1Int

#lower bound of confidence interval
fBelow(hp)

#upper bound of confidence interval
fAbove(hp)

plot(fReg,ylim=range(c(39.7,40)),type="l",col="red")
par(new=TRUE)
plot(fBelow,ylim=range(c(39.7,40)),col="green")
par(new=TRUE)
plot(fAbove,ylim=range(c(39.7,40)),col="black")



