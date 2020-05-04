dat = read.csv("sweetpepperdata.csv")
dat1 = as.data.frame(dat)

hist(dat1$X...purple.g., breaks = 10 ,
     main = "Weight distribution of PSP", 
     xlab = "weight",
     ylab = "number",
     col = "purple")

legend(110,80,pch = 0.5,cex = 0.5,
        col = c("blue", "red", "black"),           
        legend = c("Date:19/11/2019", "Location:Mustneed", "Variety:"))
hist(na.omit(dat1$red.g.), breaks = 10,
     main = "Weight distribution of RSP", 
     xlab = "weight",
     ylab = "number",
     col = "red")
     
     
legend(90,12,pch = 0.5,cex = 0.5,
        col = c("blue", "red", "black"),           
        legend = c("Date:19/11/2019", "Location:Mustneed", "Variety:"))


# ??????: variety 
wilcox.test(dat$X...purple.g.,na.omit(dat1$red.g.),alternative = "g")

#t.test
t.test(dat1$X...purple.g., dat1$red.g., paired = F)