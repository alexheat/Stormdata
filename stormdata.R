library(dplyr)
library(stringr)
library(choroplethr)
library(ggplot2)

stormdata <- read.csv("repdata-data-StormData.csv.bz2")
stormdata <- select(stormdata, REFNUM, BGN_DATE, STATE, EVTYPE, FATALITIES,INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

ApplyExponents <- function (DMGCol, EXPCol) {
  #This will multiply the damages by the right factor to get the total damages
  
  for (i in 1:length(EXPCol)) {
    if (is.na(EXPCol[i])) { next()
    } else if (EXPCol[i] == "M" | EXPCol[i] == "m") {
      DMGCol[i] <- DMGCol[i] * 1000000
    } else if (EXPCol[i] == "B") {
      DMGCol[i] <- DMGCol[i] * 1000000000
    } else if (DMGCol[i] > 0) {
      # I assume the default scale for all other values (K,k, 2,0,) is thousands 
      # Since K by far the median value
      DMGCol[i] <- DMGCol[i] * 1000
    }
  }
  return(DMGCol)
}

stormdata$PROPDMG <- ApplyExponents (stormdata$PROPDMG, stormdata$PROPDMGEXP)
stormdata$CROPDMG <- ApplyExponents (stormdata$CROPDMG, stormdata$CROPDMGEXP)

#According to this report the damage from the 1/1/2006 flood in california was $300M USD
maxdamage <- max(stormdata$PROPDMG)
stormdata[stormdata$PROPDMG==maxdamage,]$TOTALDMG <- 300000000
stormdata <- mutate(stormdata, TOTALDMG = PROPDMG+CROPDMG)

stormdata.agg <- aggregate(stormdata[c("TOTALDMG", "INJURIES")], by=list(EVENT=stormdata$EVTYPE, STATE=stormdata$STATE), FUN="sum")
stormdata.agg <- filter(stormdata.agg, stormdata.agg$TOTALDMG > 0 | stormdata.agg$INJURIES > 0)
stormdata.agg$EVENT <- str_trim(toupper(as.character(stormdata.agg$EVENT)))

for (i in 1:nrow(stormdata.agg)) {
  #clean up the event names
  if (stormdata.agg[i,]$EVENT=="?") {stormdata.agg[i,]$EVENT <- "OTHER"}
  if (grepl("HURR", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "HURRICANE"}
  if (grepl("HURRICANE/TYPHOON", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "HURRICANE"}
  if (grepl("THUNDER", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "THUNDERSTORM"}
  if (grepl("TSTM", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "THUNDERSTORM"}
  if (grepl("TORN", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "TORNADO"}
  if (grepl("FLOOD", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "FLOOD"}
  if (grepl("HEAT", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "HEAT"}
  if (grepl("HAIL", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "HAIL"}
  if (grepl("FREEZE", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "FREEZE"}
  if (grepl("FIRE", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "FIRE"}
  if (grepl("STORM SURGE", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "STORM SURGE"}
  if (grepl("WIND", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "STRONG WIND"}
  if (grepl("HEAVY RAIN", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "HEAVY RAIN"}
  if (grepl("HEAVY SNOW", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "HEAVY SNOW"}
  if (grepl("COLD", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "EXTREME COLD"}
  if (grepl("BLIZZARD", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "WINTER STORM"}
  if (grepl("FLD", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "FLOOD"}
  if (grepl("FROST", stormdata.agg[i,]$EVENT)) {stormdata.agg[i,]$EVENT <- "FROST"}
}

#aggregate one more time to flatten the new duplicate event names
stormdata.agg <- aggregate(stormdata.agg[c("TOTALDMG", "INJURIES")], by=list(EVENT=stormdata.agg$EVENT, STATE=stormdata.agg$STATE), FUN="sum")

#chart the top weather events by total damage
stormdata.top <- group_by(stormdata.agg, EVENT) %>% 
  summarize(DAMAGE=round(sum(TOTALDMG)/1000000)) %>% 
  arrange(desc(DAMAGE)) %>% top_n(10) 

stormdata.top$DAMAGE <- as.factor(stormdata.top$DAMAGE)
stormdata.top$EVENT <- as.factor(stormdata.top$EVENT)

ggplot(data=stormdata.top, aes(x=EVENT, y=DAMAGE, fill=DAMAGE)) +
  geom_bar(colour="black", stat="identity")  + coord_flip() + 
  theme(legend.position="none") + 
  ggtitle("Most Dangerous Weather Event Types To Property & Crops") +
  ylab("DAMAGE (Millions of USD)")
 
#chart the top weather events by injuries
stormdata.top <- group_by(stormdata.agg, EVENT) %>% 
  summarize(INJURIES=sum(INJURIES)) %>% 
  arrange(desc(INJURIES)) %>% top_n(10) 

top10 <- stormdata.top 

stormdata.top$INJURIES <- as.factor(stormdata.top$INJURIES)
stormdata.top$EVENT <- as.factor(stormdata.top$EVENT)

ggplot(data=stormdata.top, aes(x=EVENT, y=INJURIES, fill=INJURIES)) +
  geom_bar(colour="black", stat="identity")  + coord_flip() + 
  theme(legend.position="none") + 
  ggtitle("Most Dangerous Weather Event Types Based on Injuries to People") 

#filter by the event with the max damage in each state
agg.gb <- group_by(stormdata.agg, STATE) %>% 
  filter(TOTALDMG == max(TOTALDMG)) %>% 
  filter(TOTALDMG > 0)  %>%  filter(TOTALDMG > 0)  %>% 
  filter(STATE %in% state.abb ) %>% select(region=STATE, value=EVENT)

#plot the map with most dangerous event in each state by total damages
agg.gb$region <- as.character(agg.gb$region)
agg.gb$region <- tolower(state.name[match(agg.gb$region,state.abb)])

choro = StateChoropleth$new(agg.gb)
choro$title = "Most Damaging Weather In Each State"
choro$ggplot_scale = scale_fill_manual(name="Weather Event",values=c("#d73027", "#f46d43", "#fdae61","#4575b4", "#e0f3f8", "#74add1","#abd9e9", "#ffffbf", "#fee090" ), drop=FALSE)
choro$render()
