library(dplyr)
library(ggplot2)
data(diamonds)
head(diamonds)
str(diamonds)
names(diamonds)[2:4]
subset <- select(diamonds,carat,color:clarity)
diamondDrop <- select(diamonds, -c(table,price))
diamondDrop<- select(diamonds,-c(1,5))
head(diamondDrop)
sample_n(diamondDrop,size=2) # select random rows from a data frame


diamondC <-select(diamonds,starts_with("C"))
diamondR <- select(diamonds,ends_with("R"))
diamond1 <- filter(diamonds,carat>1)
diamondPremium <- filter(diamonds,cut=="premium")
diamondsHigh <- filter(diamonds,carat>0.75 & color=="D")
# sort
diamondSort <- arrange(diamonds,price)
head(diamondSort,3)
tail(diamondSort,3)
# rename column name
diamondRN <- rename(diamonds, Quality=cut)

# add new column
diamondDiff <-mutate(diamonds,PRICEdiff = price-mean(price,na.rm=TRUE))

# get random sample
sample_n(diamondDiff,size=4)

CLARITY.group <- group_by(diamonds,clarity)
summarize(CLARITY.group, PRICE.Average=mean(price,na.rm=TRUE), 
          PRICE.Max=min(price,na.rm=TRUE),
          PRICE.Median=median(price,na.rm=TRUE))


# create dataframe
df <- data.frame(player=c('A','B','C','D','E','F','G','H','I'),
    points=c(4,7,8,12,14,16,20,26,36)
)


df$category <- cut(df$points,breaks=4)

df$category <- cut(df$points,breaks=c(0,10,15,20,40))
df$category <- cut(df$points,breaks=c(0,10,15,20,40),
labels=c('Bad','OK','Good','Great')
)

# find the mean price for each of the following weight categories:
# 0,0.25,0.5,0.75,1,100
diamonds$category3 <- cut(diamonds$carat,breaks=c(0,0.25,0.5,0.75,1,100))
# create frequency table
freq_table <- table(diamonds$category3)
print(freq_table)
category.group <- group_by(diamonds,category3)
summarize(category.group, PRICE.Average=mean(price,na.rm=TRUE))

test1 <- read_csv("/home/tekraj/MSc-Mathematics-Github/test1.csv")
test2 <- read_csv("/home/tekraj/MSc-Mathematics-Github/test2.csv")

inner_join(test1,test2,by="id")
