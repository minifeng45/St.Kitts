###Fruit on plants####
# insert data, hand-written data
FN.purple_trim = c(13,10,18,9,22,8,11,22,24,11,26,27,17,
                24,13,20,12,25,21,31)
FN.purple_notrim = c(20,32,24,20,23,19,32,20,25,22)

FN.red_trim = c(2,12,6,4,9,11,5,7,5,5,3,4,8,14,4,8,2,14,21,1)

FN.red_notrim = c(6,6,6,2,6,9,8,7,4,12,3,8,14,7,5,5,6,6,9,5)

#Dataset labeling
FN.dataset = rbind(data.frame(fruit.number = FN.purple_trim,Variety = "purple" ,Treatment = "trim"),
                   data.frame(fruit.number = FN.purple_notrim, Variety = "purple", Treatment = "notrim"),
                   data.frame(fruit.number = FN.red_trim, Variety = "red", Treatment = "trim"),
                   data.frame(fruit.number = FN.red_notrim, Variety = "red", Treatment = "notrim"))


#calculate average, sd
FN.dataset.pivotable = FN.dataset %>%
  group_by(Variety,Treatment) %>%
  summarise(mu = mean(fruit.number),
            sd = sd(fruit.number))


# draw a barplot with errorbar
p.fruit_on_plant = ggplot(data = FN.dataset.pivotable, aes(x = Variety,y = mu,fill = Treatment))+
  geom_col(position = 'dodge', width = 0.5)+
  scale_fill_manual(values = c("#004D99","#6495ED"))+
  geom_errorbar(aes(x=Variety, ymin=mu-sd, ymax=mu+sd),  
                width=0.1, color='black', position = position_dodge(0.5),
                size=0.3)+
  labs(title = "Fruit on plant", 
       x = "Variety", y=  "Number of fruits/plant")
  