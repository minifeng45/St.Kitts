# move climate data file to desktop

setwd("/Users/supermonk00/desktop")

# insert climate data 
read.csv("climatedata.csv")
clmdat = read.csv("climatedata.csv")
head(clmdat) #check data

# average night temperature calculate
  # sort 18-06 data(1)
t = as.character(clmdat$TimesSeries)
grep("[1]0:",t)

grept =function(crt){
  f = clmdat$TimesSeries[grep(crt,t)]
  return(f)
}
nighttemp1 = grept("1[8-9]:")
nighttemp2 = grept("2[0-4]:")
nighttemp3 = grept("[^1][0-6]:") 
nighttemp4 = grept("[^2][0-6]:") 
nighttemp3 = intersect(nighttemp3,nighttemp4)
nighttemp =  union(nighttemp1,nighttemp2)
nighttemp =  union(nighttemp,nighttemp3)
  # sort isolation < 10e-4
iso = clmdat$Insolation.MJ.
nclmdat = subset(clmdat, as.numeric(as.vector((iso)))<=10e-4)
dclmdat = subset(clmdat, as.numeric(as.vector((iso)))>=10e-4)

# Average sweet pepper weight vs. night temperature
  #Night temperature Nov - Dec
  nighttemp1 = grept("11/[12-18]")
  #Purple sweet pepper trial
Pweight = c(27766,32564,19373,36473,52115,35665)
Pnum = c(340,407,344,568,821,594)
APweight = Pweight/Pnum
Sdweight = c(23.6,19.9,19.2,19.5,19.6,19.2)
Varweight = Sdweight^2
Pdat = as.data.frame(cbind(Pweight,Pnum,APweight,Sdweight,Varweight))
  
day = as.numeric(as.character(dclmdat$Air.temperature))
night  = as.numeric(as.character(nclmdat$Air.temperature))
trt = c(rep(1,16829),rep(2,17374))
value = cbind(c(day,night))


