
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


out <- c()
for(i in 1: nrow(df3)){
  tmp <- df3[i,]

  PA.sub <- subset(PA, YEAR >= tmp$a & YEAR <= tmp$b  & YEAR !=2010)

    if(nrow(PA.sub)>0){
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
write.csv(quantileOut, "C:/dump/quantileOut.csv", row.names=FALSE)


### Calculate quantile of a number
FLquantile <- ecdf(out3$FL)
FLquantile(28)
