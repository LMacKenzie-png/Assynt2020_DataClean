#Laura Mackenzie 
#23/09/2020
#trapping data merge and clean 

#libaries necessary
library(openxlsx)
library(lubridate)
#load data
setwd("~/Assynt2020_DataClean")

George<-read.xlsx("Assynt_point_entry_2020_GP.xlsx","Data")
George<-George[!is.na(George$Date),]
George$Column1<-NULL
George$Column2<-NULL
George$Column3<-NULL
George$Column4<-NULL

Laura <-read.xlsx("Assynt_point_entry_2020_lm.xlsx","Data")
Laura <-Laura[!is.na(Laura$Date)=="",]

Holly<-read.xlsx("Assynt_point_entry_2020_hb.xlsx")
Holly<-Holly[!is.na(Holly$Date),]


Victoria<-read.xlsx("Assynt_point_entry_2020_vc.xlsx")
Victoria<-Victoria[!is.na(Victoria$Name),]

Ellen<-read.xlsx("Assynt_point_entry_2020_elb.xlsx")
Ellen<-Ellen[!is.na(Ellen$Name),]

#merge data
Points<-rbind(George, Laura, Holly, Victoria, Ellen)

#get data reference
Assynt_Data_Reference <- read_excel("Assynt_Data_Reference.xlsx")
###check patch names 
unique(Points$Patch)
Points$Patch<-gsub(" ","",Points$Patch)

Mpatch<-read.csv("Patch_names.csv")
Patches<- data.frame(Patches=unique(Points$Patch)) 
Patches$Match<-Patches$Patches%in% Mpatch$Patch
Patches[Patches$Match==FALSE,]

Points$Mpatch<-Points$Patch
Points$Mpatch<-ifelse(Points$Patch=="lei04","lei0304",Points$Mpatch) #lei04 was merged into lei0304

###change dates
#dates provided as 5 digit number but we want them as yyyy-mm-dd to fit in wiht the rest of points
# For Excel on Windows, the origin date is 30 December 1899 for dates after 1900. 
Points$Date1<-as.Date(NA)
Points$Date1<-as.Date.numeric(Points$Date,origin = "1899-12-30") #changes the date according to 30/12/1899 starts date
unique(Points$Date1) #check it worked
Points$Date<-Points$Date1
Points$Date1<-NULL

###check name
unique(Points$Name)
  #all in order

###check Survey
unique(Points$Survey)
Points$Survey<-gsub(" ","",Points$Survey)
Points$Survey<-tolower(Points$Survey)
  ##seems all in order, some surveys  have flag number in 

###Check Grid
unique(Points$Grid)

###check Waypoint
unique(Points$Waypoint)
Points$Waypoint[Points$Waypoint=="140 M"]<-140
Points$Waypoint<-as.numeric(Points$Waypoint)

###check Big_X 
str(Points$Big_X)
outliers.x  <- subset(Points,Points$Big_X<212070|Points$Big_X>231960) ## find x points that are outside area of assynt
#no outliers found

###check Big_y
str(Points$Big_Y)
outliers.y  <- subset(Points,Points$Big_Y<902605|Points$Big_Y>933357)
#no outliers found

###check elevation shouldn't be too high or low ~ 1~500
str(Points$Elev_m)

unique(Points$Elev_m)
min(Points$Elev_m,na.rm = TRUE) #51.45612
max(Points$Elev_m,na.rm = TRUE) #464.0514
  #no problems found

###Check Code
unique(Points$Code)%in%unique(Assynt_Data_Reference$Code) #compare codes 
unique(Points$Code) #eh/es is es/eh in Reference
Points$Code<-gsub(" ","",Points$Code)
Points$Code[Points$Code=="eh/es"]<-"es/eh"

unique(Points$Code)%in%unique(Assynt_Data_Reference$Code) #worked

###check Latrine
str(Points$Latrine)
unique(Points$Latrine)%in%unique(Assynt_Data_Reference$Latrine) # all correct

###check check_1
str(Points$check_1)
unique(Points$check_1)%in%unique(Assynt_Data_Reference$check_x)# all correct

###checl check_2
str(Points$check_2)
unique(Points$check_2)%in%unique(Assynt_Data_Reference$check_x)#all correct

###check check3 
str(Points$check_3)
unique(Points$check_3)%in%unique(Assynt_Data_Reference$check_x)#all correct

###check check_4
str(Points$check_4)
unique(Points$check_4)%in%unique(Assynt_Data_Reference$check_x)#all correct

###check check_5
str(Points$check_5)
unique(Points$check_5)%in%unique(Assynt_Data_Reference$check_x)#all correct

###check comments
unique(Points$Comment)

####so far I would say this is clean enough to now be merged to Assynt points
