# Problem 1
n = 10
ar1 = 0.80
set.seed(0)
epsilon = rnorm(10,0,1)
numbers = c(1,0,0,0,0,0,0,0,0,0)
for(i in 2:n){
  numbers[i] = numbers[i-1]*ar1 + epsilon[i]
}
numbers
acf(numbers)
pacf(numbers)

ar1 = 0.43 
ar2 = -0.29
numbers = c(1,.43,0,0,0,0,0,0,0,0)
numbers[2] = numbers[1]*ar1 + epsilon[2]
for(i in 3:n){
  numbers[i] = numbers[i-1]*ar1 + numbers[i-2]*ar2 + epsilon[i]
}
numbers
acf(numbers)
pacf(numbers)

ar1 = 0.33 
ar2 = 0.21
ar3 = -0.89
numbers = c(1,0,0,0,0,0,0,0,0,0)
numbers[2] = numbers[1]*ar1 + epsilon[2]
numbers[3] = numbers[1]*ar1 + numbers[2]*ar2 + epsilon[3]
for(i in 4:n){
  numbers[i] = numbers[i-1]*ar1 + numbers[i-2]*ar2 + numbers[i-3]*ar3 + epsilon[i]
}
numbers
acf(numbers)
pacf(numbers)

ar1 = 1.20
numbers = c(1,0,0,0,0,0,0,0,0,0)
for(i in 2:n){
  numbers[i] = numbers[i-1]*ar1 + epsilon[i]
}
numbers
acf(numbers)
pacf(numbers)

ar1 = 1.6 
ar2 = -0.95
numbers = c(1,0,0,0,0,0,0,0,0,0)
numbers[2] = numbers[1]*ar1 + epsilon[2]
for(i in 3:n){
  numbers[i] = numbers[i-1]*ar1 + numbers[i-2]*ar2 + epsilon[i]
}
numbers
acf(numbers)
pacf(numbers)

#problem 2
#n changed to 500, along with epsilon, then run acf and pacf on each set of 500 numbers

#problem 3
x = rnorm(500,8,sqrt(2))
y = rnorm(500,15,sqrt(4))

z = x + y

#should be close to 23 (and it is)
meanZ = mean(z)
meanZ

#should be close to 6 (and it is)
varZ = var(z)
varZ


