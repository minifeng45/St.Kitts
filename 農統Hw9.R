
rm(list=ls())
y = c(260,120,200,190,280,135,240,264,220,270,
      180,150,350,200,270)
x = c(54,40,42,48,70,20,44,60,52,78,39,30,67,21,58)
b = c(rep(c(1,2,3,4,5),3))
trt = factor((c(rep(c(1,2,3), each = 5))))
fit.f = lm(y~x+trt)
fit.r = lm(y~x)
anova(fit.r,fit.f)

ystar = y  - 2.9286*(x-mean(x))
fit = lm(ystar~trt)
anova(lm(y~x+trt))
lsd = LSD.test(fit,"trt")

y = c(18.6,20.5,30,26.2,22.7,20.2,24.2,28.7,22.3,20.5,
      15.6,12.5,23.4,20.8,22.7,20,18.2,29.5,32.4,24.8)
x = c(2.2,2.5,3.1,2.6,2.4,1.8,2.4,3.5,2.6,2.5,2.3,1.9,
      2.4,2,3.2,2.3,1.7,2.6,2.9,2.8)
b = factor(rep(c(1,2,3,4),each = 5))
t = factor(rep(c(1,2,3,4,5),4))
fit.f = lm(y~b+x+t)
fit.r = lm(y~b+x)
anova(fit.r,fit.f)

ystar = y  - 7.312*(x-mean(x))
fit = lm(ystar~t)
lsd = LSD.test(fit,"t")
