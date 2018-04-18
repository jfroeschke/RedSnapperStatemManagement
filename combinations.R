
library(tidyverse)
library(reshape2)

### Import private angling data
PA <- read.csv("C:/Users/johnf/Downloads/RedSnapperDecisionSupportTool3-master/RedSnapperDecisionSupportTool3-master/PrivateAngling2.csv")



Start <- 1986:2016
End <- 1986:2016

### Create a dataframe with all combinations of start and end years
df <- expand.grid(a = Start, b = End)
df2 <- data.frame(df[order(df$a), ])

### Remove non-sensical combinations (i.e., end year < start year)
df2$try <- "ns"
df2$try <- ifelse(df2$b<df2$a, "no", 'yes')
df3 <- subset(df2, try=='yes')
df3$index <- 1:nrow(df3) ## add index for simulations 


out <- c()
for(i in 1: nrow(df3)){
  tmp <- df3[i,]

  PA.sub <- subset(PA, YEAR >= tmp$a & YEAR <= tmp$b  & YEAR !=2010)

    #if(nrow(PA.sub)>0){
      if(nrow(PA.sub)>=5){ #5 year minimum
        x <- melt(PA.sub, id="YEAR")
        colnames(x) <- c("Year", "State", "Landings")
        x2 <- group_by(x, State) %>%
              summarise(Landings=mean(Landings) )
        x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
        x2$State <- c("FL", "AL", "MS", "LA", "TX")
        x3 <-  select(x2, -Landings)
        x4 <- spread(x3, State, Allocation)
        x4$Start <- tmp$a
        x4$End <- tmp$b
        x5 <- as.data.frame(x4)
        out <- rbind(out, x5)
        } 
}


out2 <- subset(out, Start!=2010)
out3 <- subset(out2, End!=2010)
###head(out)
hist(out$AL)

### remove some rows that become duplicative

##min years 5
### select 10



FLout <- quantile(out3$FL, probs = c(.025, .25, .5, .75, .975))
ALout <- quantile(out3$AL, probs = c(.025, .25, .5, .75, .975))
MSout <- quantile(out3$MS, probs = c(.025, .25, .5, .75, .975))
LAout <- quantile(out3$LA, probs = c(.025, .25, .5, .75, .975))
TXout <- quantile(out3$TX, probs = c(.025, .25, .5, .75, .975))

quantileOut <- data.frame(FL=FLout, AL=ALout, MS=MSout, LA=LAout, TX=TXout)
write.csv(quantileOut, "quantileOut.csv", row.names=FALSE)


### Calculate quantile of a number
FLquantile <- ecdf(out3$FL)
FLquantile(28)


####################################### simulation test################### select 10 random years and compute 1000 times
### use df3 from above

PA.sim <- subset(PA, YEAR!=2010)
PA.sim$index <- 1:nrow(PA.sim)

out.sim <- c()

for(i in 1:1000){
  set.seed(i)
    INDEX <- sample(PA.sim$index, size=10, replace = FALSE)  #select rows for subsetting
    PA.sim2 <- subset(PA.sim, index %in% INDEX)
    PA.sim2 <- select(PA.sim2, -index)
    x <- melt(PA.sim2, id="YEAR")
    colnames(x) <- c("Year", "State", "Landings")
    x2 <- group_by(x, State) %>%
          summarise(Landings=mean(Landings) )
    x2$Allocation <- (x2$Landings/sum(x2$Landings))*100
    x2$State <- c("FL", "AL", "MS", "LA", "TX")
    x3 <-  select(x2, -Landings)
    x4 <- spread(x3, State, Allocation)
    x5 <- as.data.frame(x4)
out.sim <- rbind(out.sim, x5)
}

FLout.sim <- quantile(out.sim$FL, probs = c(.025, .25, .5, .75, .975))
ALout.sim <- quantile(out.sim$AL, probs = c(.025, .25, .5, .75, .975))
MSout.sim <- quantile(out.sim$MS, probs = c(.025, .25, .5, .75, .975))
LAout.sim <- quantile(out.sim$LA, probs = c(.025, .25, .5, .75, .975))
TXout.sim <- quantile(out.sim$TX, probs = c(.025, .25, .5, .75, .975))

quantileOut.sim <- data.frame(FL=FLout.sim, AL=ALout.sim, MS=MSout.sim, LA=LAout.sim, TX=TXout.sim)
write.csv(quantileOut.sim, "quantileOut.sim.csv", row.names=FALSE)


### Calculate quantile of a number
FLquantile <- ecdf(out3$FL)
FLquantile(28)

save.image("combinations.RData")