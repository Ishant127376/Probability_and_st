 Assignment-5
question 1
 s = punif(45,0,60)
ans = 1 - s
print(ans)

ans2 = punif(30,0,60) - punif(20,0,60)
print(ans2)

QUESTION 2
exponential distribution
 
x = 3
l = 1/2
a =dexp(x,l)
print(a)


sequ = seq(0,5,by = 0.0001)
plot(sequ,dexp(sequ,l))

atmost3 = pexp(3,l)
print(atmost3)

plot(sequ,pexp(sequ,l))

n = 1000
plot(density(rexp(n,l)))



Question 3
 
ans_1 = 1-pgamma(1,2,1/3)
print(ans_1)

qgamma(0.7,2,1/3)



ASSIGNMENT-6


install.packages("pracma")
library(pracma)
f =function(x,y){
return (2*(2*x+3*y)/5)
}

ans1 = integral2(f,0,1,0,1)
ans1$Q
print(ans1)

mx = function(y){
return (2*(2+3*y)/5)
}
answer =integrate(mx,0,1)$value
print(answer)


my = function(x){
return (2*(2*x)/5)
}

answer_my = integrate(my,0,1)$value
print(answer_my)

expe = function(x,y){
return(x*y*2*(2*x+3*y)/5)
}

answe = integral2(expe,0,1,0,1)
answe$Q

QUESTION 2

f = function(x,y){
return ((x+y)/30);
}

x = c(0,1,2,3)
y = c(0,1,2)

mat = matrix(c(f(0,y),f(1,y),f(2,y),f(3,y)),nrow =4,ncol = 3,byrow='TRUE');
print(mat)


summ = sum(mat)
print(summ)


marginalx = apply(mat,1,sum)
print(marginalx)

marginal_y =apply(mat,2,sum)
print(marginal_y)

CONDITIONAL PROBABILITY


anss = mat[1,2]/marginal_y[2]
print(anss)

f1 = function(x,y){
return (x*(x+y)/30)
}

m1 = matrix(c(f1(0,y),f1(1,y),f1(2,y),f1(3,y)),nrow = 4,ncol=3, byrow ='TRUE')
print(m1)
e_x =sum(m1)
print(e_x)

f2 = function(x,y){
return (y*(x+y)/30)
}
m2 =matrix(c(f2(0,y),f2(1,y),f2(2,y),f2(3,y)),nrow = 4,ncol=3, byrow ='TRUE')
e_y = sum(m2)
print(e_y)

f3 = function(x,y){
return (x*y*(x+y)/30)
}
m3 = matrix(c(f3(0,y),f3(1,y),f3(2,y),f3(3,y)),nrow = 4,ncol=3, byrow ='TRUE')
e_xy = sum(m3)
print(e_xy)


SHORTCUT
ey = sum(y*apply(mat,2,sum))
print(ey)

ex =  sum(x*apply(mat,1,sum))
print(ex)

ex2 = sum(x*x*apply(mat,1,sum))
print(ex2)
ey2 = sum(y*y*apply(mat,2,sum))
print(ey2)

varx = ex2 - (ex)^2
print(varx)

vary = ey2 - (ey)^2
print(vary)


cov = exy - ex*ey
print(cov)


correlation_coff = cov/((varx^0.5)*(vary^0.5))
print(correlation_coff)




ASSIGNMENT-7


1)

n = 100
df = n-1
ans  = rt(100,df)
hist(ans)

2)

n = 100
df1=2
df2=10
df3=25
rchisq(n,df1)
rchisq(n,df2)
rchisq(n,df3)



3)
v  = seq(-6,6,length.out =100)
ans1 = dt(v,1)
ans2 = dt(v,4)
ans3 = dt(v,10)
ans4 = dt(v,30)
print(ans1)
print(ans2)
print(ans3)
print(ans4)
plot(v,ans4)

plot(v,ans1,xlab="x-axis",ylab="y-lab", col="blue",lwd=2,ylim=c(0,0.4))
lines(v,ans2,col="black",lwd=2)
lines(v,ans3,col="red",lwd=2)
lines(v,ans4,col="yellow",led=2)

legend("topleft",legend=c("df=1","df=4","df=10","df=30"),col=c("blue","black","red","yellow"),lwd=2)



4)

qf(0.95,10,20)

int1= pf(1.5,10,20)
int2 = 1-int1
print(int1)
print(int2)

q = c(0.25, 0.5, 0.75,0.999)
qf(q,10,20)


hist(rf(1000,10,20))



ASSIGNMENT-8 UPDATED

1)

data = read.csv("C://Users//Ayushi//Downloads//Clt-data.csv")
print(data)  

num_rows = nrow(data)
print(num_rows)

head(data,10)

mean_population = mean(data$Wall.Thickness)
print(mean_population)
hist(data$Wall.Thickness)
abline(v=mean_population,col="red",lwd=2)


values = data$Wall.Thickness

par(mfrow =c(2,2))
answer = replicate(1000,mean(sample(values,10,replace="TRUE")))
hist(answer)


answer1 =replicate(1000,mean(sample(values,50,replace="TRUE")))
hist(answer1)

answer2 = replicate(1000,mean(sample(values,500,replace="TRUE")))
hist(answer2)

answer3 = replicate(1000,mean(sample(values,9000,replace="TRUE")))
hist(answer3)
par(mfrow = c(1,1))

QUESTION 2

Age=c(58, 69 ,43, 39, 63, 52, 47 ,31 ,74 ,36)
Cholesterol=c(189 ,235, 193 ,177 ,154 ,191 ,213, 165 ,198, 181)

plot(Age,Cholesterol)

model = lm(Cholesterol~Age)

abline(model,col="blue")
summary(model)


predicted_value = predict(model, newdata= data.frame(Age=60))
print(predicted_value)

QUESTION 3

Btest =c(145 ,173 ,158 ,141 ,167 ,159, 154, 167, 145, 153)
Atest =c(155 ,167 ,156 ,149 ,168, 162, 158, 169 ,157, 161)

t.test(Btest,Atest,paired="TRUE",conf.level=0.95)






assignment-8 old

set.seed(123)
sample_means_binom <- replicate(1000, {
  sample <- rbinom(30, size = 10, prob = 0.8)
  mean(sample)
})
hist(sample_means_binom, breaks = 30, main = "Sample Means - Binomial(10, 0.8)", col = "lightgreen",lwd=2)


set.seed(123)
sample_mean_dist_pois = replicate(1000,{samplee = rpois(50,4) 
mean(samplee)})
emp_mean = mean(sample_mean_dist_pois)
emp_var = var(sample_mean_dist_pois)
print(emp_mean)
print(emp_var)

theo_mean = 4
theo_var = 4/50
print(theo_mean)
print(theo_var)


hist(sample_mean_dist_pois,col="blue",lwd=2)
par(mfrow= c(1,2))



set.seed(123)
sample_mean_dist_pois = replicate(1000,{samplee = rpois(50,10) 
mean(samplee)})
emp_mean = mean(sample_mean_dist_pois)
emp_var = var(sample_mean_dist_pois)
print(emp_mean)
print(emp_var)

theo_mean = 10
theo_var = 10/50
print(theo_mean)
print(theo_var)
hist(sample_mean_dist_pois,main="hist-l=10",col="blue",lwd=2)








par(mfrow = c(1,2))
set.seed(123)
mean_dist_exp = replicate(1000,{sample = rexp(10,1.5)
mean(sample)})
hist(mean_dist_exp,main="n=10",col="green")
mean_dist_exp = replicate(1000,{sample = rexp(50,1.5)
mean(sample)})
hist(mean_dist_exp,main="n=50",col="green")
par(mfrow=c(1,1))



par(mfrow=c(2,2))
set.seed(123)
mean_dist_normal= replicate(1000,{sample = rnorm(30,70,10)
mean(sample)})
hist(mean_dist_normal,main="n=30",col="grey")
mean_dist_normal= replicate(1000,{sample = rnorm(50,70,10)
mean(sample)})
hist(mean_dist_normal,main="n=50",col="grey")
mean_dist_normal= replicate(1000,{sample = rnorm(70,70,10)
mean(sample)})
hist(mean_dist_normal,main="n=70",col="grey")
mean_dist_normal= replicate(1000,{sample = rnorm(90,70,10)
mean(sample)})
hist(mean_dist_normal,main="n=90",col="grey")

par(mfrow = c(1,1))

par(mfrow = c(1,2))
set.seed(123)
mean_dist_gamma = replicate(1000,{ sample= rgamma(10,2,1)
mean(sample)})
hist(mean_dist_gamma,main="n=10",col="orange")
mean_dist_gamma = replicate(1000,{ sample= rgamma(100,2,1)
mean(sample)})
hist(mean_dist_gamma,main="n=100",col="orange")
