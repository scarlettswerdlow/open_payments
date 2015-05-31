########################################                                                                            #
#  MAPS                                #
#  Coded by Sarah Sajewski             #
#  May 31, 2015                        #                                                           
########################################

#TOC:
#{1}READ IN DATA
library(maps)
map('state')
setwd("/Users/sarahsajewski/Dropbox/Harris/BigData2/Final Project/Openpayments data/")
zip <-read.csv("zipdata.csv")

zip$zip <- substr(zip$coef.rzip,5,9)
zipval <-aggregate(zip$val~zip$zip, FUN=max )
head(zipval)
names(zipval)<- c("zip","val")
head(zipval)
#get zip lat and long
#downloaded from: https://www.census.gov/geo/maps-data/data/gazetteer2010.html
latlong <-read.table("zcta.txt", header=TRUE, colClasses=c("character", rep("NULL",6), rep("numeric",2)))
names(latlong)[1]<-"zip"
head(latlong)
ziplatlong <- merge(zipval,latlong,by="zip")
zippos <-subset(ziplatlong, ziplatlong$val>0)
zipneg <-subset(ziplatlong, ziplatlong$val<0)
library(maps)
map("state",)
points(x=zippos[,4], y=zippos[,3], pch=20, cex=3, col="hotpink")
points(x=zipneg[,4], y=zipneg[,3], pch=20, cex=3, col="lightblue")

#check why so many didn't merge?
zipcheck <- merge(zipval,latlong,by="zip", all.x=TRUE)
notmerge <- subset(zipcheck,is.na(zipcheck$INTPTLONG)&zipcheck$val!=0)
head(zipcheck)
head(notmerge)
notmerge
# some problems: example one of the zipcodes that doesn't merge is 21287- the zip code for
#johns hopkins, presumably because the census does not consider that a ZTCA. 
#also problematic- 44195 which is the clevland clinic. 

#chorplethr stuff
library(choroplethr)
library(choroplethrMaps)
# zip.sig<- names(coef.rzip)
# zipval <- coef.rzip
# zip.data <- data.frame(zip, zipval)
# region <-substr(names(coef.rstate[coef.rstate!=0]),7,8)
region <- c("california", "district of columbia", "florida", "indiana", "kansas", "kentucky",
            "massachusetts", "maryland", "minnesota", "missouri", "north carolina", "new jersey",
            "nevada", "new york", "ohio", "puerto rico", "south carolina", "west virginia")
value <-(coef.rstate[coef.rstate!=0])
state.data<-data.frame(region, value)
state_choropleth(state.data, num_colors = 2)
