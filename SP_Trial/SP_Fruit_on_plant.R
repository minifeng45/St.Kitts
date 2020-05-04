###Fruit on plants####
# insert data, hand-written data
purple_trim = c(13,10,18,9,22,8,11,22,24,11,26,27,17,
                24,13,20,12,25,21,31)
purple_notrim = c(20,32,24,20,23,19,32,20,25,22)

red_trim = c(2,12,6,4,9,11,5,7,5,5,3,4,8,14,4,8,2,14,21,1)

red_notrim = c(6,6,6,2,6,9,8,7,4,12,3,8,14,7,5,5,6,6,9,5)

#calculate average, sd
ptrim_mu = mean(purple_trim)
pnotrim_mu = mean(purple_notrim)

ptrim_sd = sd(purple_trim)
pnotrim_sd = sd(purple_notrim)

rtrim_mu = mean(red_trim)
rnotrim_mu = mean(red_notrim)

rtrim_sd = sd(red_trim)
rnotrim_sd = sd(red_notrim)

mu = c(ptrim_mu,pnotrim_mu,rtrim_mu,rtrim_sd)

sd = c(ptrim_sd,pnotrim_sd,rtrim_sd,rnotrim_sd)

# form a dataframe by treatment & variety
Treatment = c("Trim","NoTrim","Trim","NoTrim")
color = c("Purple","Purple","Red","Red")

data = data.frame(cbind(mu,sd),Treatment,color)

# draw a barplot with errorbar
p = ggplot(data = data, aes(x = color,y = mu,fill = Treatment))+
  geom_col(position = 'dodge', width = 0.5)+
  scale_fill_manual(values = c("#004D99","#6495ED"))+
  geom_errorbar(aes(x=color, ymin=mu-sd, ymax=mu+sd),  
                width=0.1, color='black', position = position_dodge(0.5),
                size=0.3)+
  labs(title = "Fruit on plant", 
       x = "Variety", y=  "Number of fruits/plant")
  