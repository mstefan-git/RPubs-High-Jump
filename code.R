# Replication file for: "High Jump World Records"
# RPubs-link: https://rpubs.com/mstefan-rpubs/jump
# (c) Martin Stefan, August 2020

rm(list = ls())

df <- read.csv("data.csv")
df$Technique <- as.factor(df$Style)
df$Day <- as.character(df$Day)
df$Month <- as.character(df$Month)
for(i in 1:nrow(df)) if(nchar(df$Day[i])==1) df$Day[i] <- paste(0,df$Day[i],sep="")
for(i in 1:nrow(df)) if(nchar(df$Month[i])==1) df$Month[i] <- paste(0,df$Month[i],sep="")
df$Date <- paste(df$Day,df$Month,df$Year,sep="-")
df$Date <- as.Date(df$Date,"%d-%m-%Y")


library(ggplot2)

# world record over time
ggplot(df,aes(x=Date,y=Record)) + 
  geom_step() +
  geom_point()

# colors for techniques
ggplot(df,aes(x=Date,y=Record)) + 
  geom_step() +
  geom_point(aes(col=Technique))

# adjusted world records
df$AdjRec <- df$Record/df$Height
df <- df[!is.na(df$AdjRec),]

# remove "unfair" world records
keepwhile = T
while(keepwhile) {
  for(i in 2:(nrow(df)-1)) {
    if(i == nrow(df)-1) keepwhile = F
    if(df$AdjRec[i] < df$AdjRec[i-1]) {
      df <- df[-i,]
      break
    }
  }
}

# plot
ggplot(df,aes(x=Date,y=AdjRec)) + 
  geom_step() +
  geom_point(aes(col=Technique))
