#Date I need to start with: "Allthetags" and ""
# I will continue working on the data I made in the first script - 
#data I need to start with:
cat("\014")
#rm(AllDates)

#ListOfStart
#Allthetags

#Order it:

Allthetags<-Allthetags[order(Allthetags$TAG,Allthetags$TIME),]

#Now I need to remove all the data before the lapwings were tagged (driving in the car),
# I need to subset ListOfStart to just tag, date_capture and start_hour:

TagStart <- ListOfStart[c("TAG", "date_capture", "start_hour")]

head(Allthetags)
str(Allthetags$DateTime)
# I need to combine the date_capture and start_hour to one row and make it as time (DateTime POSIXct, format: "2020-03-31 06:05:10")

# I will create a list of the first time 

TagStart$DateTime <- paste(TagStart$date_capture, TagStart$start_hour, sep = " ")
TagStart$DateTime<- as.POSIXct(TagStart$DateTime, format="%d/%m/%Y %H:%M:%S", tz="UTC")
str(TagStart) #Good


# Now i need to create a if function, that if the TAG column are the same, remove the data from
# that is smaller them DateTime

Lisoftags
FirstCleaning <- list()

for (i in Lisoftags){
  d <- subset(TagStart , TAG == i)
  tag <- subset(Allthetags, TAG == i)
  DF <- tag[!(as.POSIXlt(tag$DateTime)< as.POSIXlt(d$DateTime)),]
  FirstCleaning[[i]] <- DF
  print(head(DF,1))}
rm(tag,d,DF)


FirstCleaning2 <- do.call(rbind.data.frame, FirstCleaning)
rm(FirstCleaning,Allthetags)

FirstCleaning3<-addLocAttribute(FirstCleaning2, locAttributs=c("distanceSpeed", "locQuality")) # This will take long time and will use a lot of CPU!!! close what you dont need
head(FirstCleaning3)
rm(FirstCleaning2)

FirstCleaning3 <-FirstCleaning3[order(FirstCleaning3$TAG,FirstCleaning3$TIME),]
unique(FirstCleaning3$TAG)

# Now I will remove the starvar that is bigger then 20

hist(FirstCleaning3$stdVarXY,xlim =c(0,100), breaks=5000000, main= "Stdev Distribution") # Now the distribution is more informative.
summary(FirstCleaning3$stdVarXY)
#I can clean the stVar of all lapwings the same way - but the speed is different to each one...

FirstCleaning4<-as.data.frame(FirstCleaning3 %>%  filter(stdVarXY<30)) 
hist(FirstCleaning4$stdVarXY,breaks=50, main= "Stdev Distribution") # Now the distribution is more informative.


TagList<-unique(FirstCleaning4$TAG) # choose a radom TAG. 
TAG_ex<-sample(TagList, 1)

raw_tracks<-FirstCleaning3[which(FirstCleaning3$TAG==TAG_ex),] # selecting night number 3 of randomly selected tags. Naturally you can control which tag and night you want to examine too. 

filtered_tracks<-FirstCleaning4[which(FirstCleaning4$TAG==TAG_ex),]

track_p<-convertSpatial.ITM2WGS84(raw_tracks, xyColNames=c("X","Y"))
track_f<-convertSpatial.ITM2WGS84(filtered_tracks, xyColNames=c("X","Y"))  


# In this example we plotted the unfiltered data in black and the filtered data in red. You can zoom in-an-out and when you hover over a point the stdev value pops-up. This way, you can change these parameters and observe thier influence. 
ll <- leaflet() %>% addTiles() %>% 
  
  addCircleMarkers(data=track_p, radius=2,color = "black",fillOpacity = 1,
                   popup = ~htmlEscape(as.character(round(track_p$stdVarXY)))) %>%
  
  addCircleMarkers(data=track_f, radius=3,color = "red",
                   popup = ~htmlEscape(as.character(round(track_f$stdVarXY)))) 

ll
