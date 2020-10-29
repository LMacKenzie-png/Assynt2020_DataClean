#UPDATE 29/10/2020 found hb7 and hb8 missing changed hb5 and gp214 to new captures
### Load all data 
setwd("~/Assynt2020_DataClean")

George<-read.csv("Assynt_trapping_entry_2020_GP.csv")
George<-George[!George$Name=="",]

Laura <-read.csv("Assynt_trapping_entry_2020_lm.csv")
Laura <-Laura[!Laura$Name=="",]

Holly<-read.csv("Assynt_trapping_entry_2020_hb.csv")
Holly<-Holly[!Holly$Name=="",]
Holly$Vial<-paste0(Holly$Name,Holly$Vial)

Victoria<-read.csv("Assynt_trapping_entry_2020_vc.csv")
Victoria<-Victoria[!Victoria$Name=="",]

Ellen<-read.csv("Assynt_trapping_entry_2020_elb.csv")
Ellen<-Ellen[!Ellen$Name=="",]


###merge all data sheets
Data<-rbind(George,Laura,Holly,Victoria,Ellen)

###change Mark
unique(Data$Mark)
Data$Mark[Data$Mark=="l.s"|Data$Mark=="l.s "]<-"ls"
Data$Mark[Data$Mark=="l.h"]<-"lh"
Data$Mark[Data$Mark=="r.s"]<-"rs"
Data$Mark[Data$Mark=="r.h"]<-"rh"
Data$Mark[Data$Mark=="l.s,r.s"|Data$Mark=="ls, rs"|Data$Mark=="l.s, r.s"]<-"ls,rs"
Data$Mark[Data$Mark=="l.h,r.s"|Data$Mark=="l.h, r.s"]<-"lh,rs"
Data$Mark[Data$Mark=="ls, lh "]<-"ls,lh"

###check patch names 
Mpatch<-read.csv("Patch_names.csv")
Patches<- data.frame(Patches=unique(Data$Patch)) 
Patches$Match<-Patches$Patches%in% Mpatch$Patch
Patches[Patches$Match==FALSE,]

Data$Patch<-tolower(Data$Patch)
unique(Data$Patch)
rm(Patches)
rm(Mpatch)

###check flags
unique(Data$Flag)

###check tag_start

unique(Data$Tag_start)
Data$Tag_start<-ifelse(is.na(Data$Tag_start),Data$Mark,Data$Tag_start)
Marks<-c("lh","ls,rs","lh,rh","rh","rs","ls","ls,lh","ls,rh","",NA)
unique(Data$Species)
Data$Tag_start<-ifelse(Data$Species=="wv",Data$Tag_start,NA)
unique(Data$Tag_start)

Data$Mark<-ifelse(is.na(Data$Tag_start)|Data$Tag_start=="",Data$Mark,NA)
unique(Data$Mark)

Data$Mark[Data$Species=="wv"]

### check species
unique(Data$Species) #no out of order data!

####check Capt
unique(Data$Capt)
Data[Data$Capt=="",]
Data$Capt[Data$Vial=="gp206"]<-"n" #was marked as "" for Capt
Data$Capt[Data$Capt==""]<-NA   #most have escaped or are different species which are not recorded as n or recptures

##check sex
unique(Data$Sex) #nothing out of order

## check weight
unique(Data$Weight) #nothign out of order

##check colour
unique(Data$Colour) # no problems

## check whitespot
unique(Data$White.spot) # no problems

##check ws position
unique(Data$WS.position) # problem

##Check testes
unique(Data$Testes) # no problem
Data$Testes<-tolower(Data$Testes)
females<- Data[Data$Sex=="f",]
unique(females$Testes) # no females with testes measured which is good
rm(females)

## check vagina &pubic bone &  pregnant
unique(Data$Vagina) # n and np
unique(Data$Pubic.bone)# open and closed
unique(Data$Pregnant) # N,NA,Y
unique(Data$Nipples)

unique(Data$Lactating)
Data$Lactating[Data$Lactating=="N "]<-"N"

males<-Data[Data$Sex=="m",]
unique(males$Vagina) 
unique(males$Pubic.bone)
unique(males$Pregnant) 
unique(males$Nipples)# no female specific data in male column
unique(males$Lactating)# no female specific data in male column
rm(males)

##check mites 1 to 10
unique(Data$Mites) # no problem

# check tick
unique(Data$Ticks)# no problem

#check fleas
unique(Data$Fleas)# no problem

#check fur loss
unique(Data$Fur.loss)# no problem

#check Biopsy (1 or 0)
unique(Data$Biopsy)
not.wv<- Data[!Data$Species=="wv",]
unique(not.wv$Biopsy) #only wv should have biopsy taken probably just a mistake 
Data$Biopsy[Data$Species!="wv"]<-NA

not.wv<- Data[!Data$Species=="wv",]
unique(not.wv$Biopsy) #problem solved
rm(not.wv)

#check blood
unique(Data$Blood)

#check isolate
unique(Data$Isolate)

#check severety 
unique(Data$Severity)
Data$Severity<-tolower(Data$Severity)
bloods<-Data[Data$Blood==1|Data$Isolate==1,]
unique(bloods$Severity)

Data$Severity[Data$Blood==1|Data$Isolate==1]<-"m"
rm(bloods)

#Check Bleeder
unique(Data$Bleeder)
Bled<- Data[Data$Blood==1,]
Bled<-Bled[!is.na(Bled$Vial),]
unique(Bled$Bleeder) 
rm(Bled)

##check vial
unique(Data$Vial)



# check docility scores
unique(Data$Urinate)
unique(Data$Deficate)
unique(Data$Resists)
unique(Data$Pushing)
unique(Data$Bites)

unique(Data$Squeaks)
Data$Squeaks[Data$Squeaks==3]<-2

unique(Data$Handling)
Data$Handling[Data$Handling==3]<-2

Data$Docility<-rowSums(Data[,c(34:40)],na.rm=T)

#### matching up IDs following Richards way of doing it
Data$Capt[Data$Vial=="gp215"]<-"n" #clearly new not recapture
Data$Tag_start[Data$Vial=="elb02"]<-"5667730" #elb02 elb03 have same tag according to match ups this is the other one 
Data$Tag_start[Data$Vial=="lm241"]<-"NA" #duplicated number for two voles, likely that later one was written down from a wrapper
Data$Capt[Data$Vial=="gp214"]<-"n" 
Data$Capt[Data$Vial=="hb5"]<-"n" 

traps_wv<-Data[Data$Species=="wv",] #only wv
traps_wv_new<-traps_wv[traps_wv$Capt=="n"|traps_wv$Capt=="nr",] 
traps_wv_new$Tag_start<-as.numeric(traps_wv_new$Tag_start) # introduces NA for any ls lh etc. 
traps_wv_new<-traps_wv_new[!is.na(traps_wv_new$Tag_start),] # made so that any NA or ls etc aren't unnecessarily duplicated

traps_wv_new$Vial<-as.character(traps_wv_new$Vial)
traps_wv_new<-traps_wv_new[!traps_wv_new$Vial=="elb21",]

traps_wv_recap <- subset(traps_wv,Capt=="r")
traps_wv_recap$Tag_start<-as.numeric(traps_wv_recap$Tag_start)
traps_wv_recap <- traps_wv_recap[complete.cases(traps_wv_recap$Tag_start),]# same as !is.na above!

is.numeric(traps_wv_new$Tag_start)
is.numeric(traps_wv_recap$Tag_start)

# Assign recaptures vial names based on new capture information 


for (i in 1:length(traps_wv_recap$Capt))
{
  
  PIT <- traps_wv_recap[i,"Tag_start"]
  PIT
  vialID <- traps_wv_new[traps_wv_new$Tag_start==as.character(PIT),"Vial"]
  vialID <- vialID[complete.cases(vialID)]
  vialID
  if(length(vialID)>0){
    traps_wv_recap[i,"Vial_new"] <- vialID
  } else {
    traps_wv_recap[i,"Vial_new"] <- "Tag_not_Found" ### when tags don't match they are given tag_not_found
  }
}


#check that this has worked and that they were correctly assigned
###Merge data and after checking use Vial new as Vial

traps_wv_new$Vial_new<-NA #to have the columns match up
traps_wv_recap$Vial<-traps_wv_recap$Vial_new
named_recaps <- rbind(traps_wv_new,traps_wv_recap)

#### look for all the not found tags and check if some may be only slightly mismatched (using Richards code)
not_found <- subset(named_recaps,Vial=="Tag_not_Found") ## subset samples where tags were not found

missing_tags <- unique(not_found[,"Tag_start"]) ## list of tags that were not found

pos_matches <- data.frame(Vial=NA,Patch=NA,Tag_start=NA,Iter=NA)

for(i in 1:length(missing_tags))
{
  
  tag_info <- subset(not_found,Tag_start==missing_tags[i], select=c("Vial","Patch","Tag_start"))
  
  match <- agrep(missing_tags[i],traps_wv_new$Tag_start, max.distance=1)
  
  match_res <- traps_wv_new[match,c("Vial","Patch","Tag_start")]
  
  match_df <- rbind(tag_info,match_res)
  
  match_df$Iter <- i
  
  pos_matches <- rbind(pos_matches,match_df)
  
}

View(pos_matches)


## first tag with vial name always taken as correct tag ID
Data$Corrected.Mark[Data$Tag_start=="517254"]<-"10517254"
Data$Corrected.Mark[Data$Tag_start=="5601277"]<-"5601272"
Data$Corrected.Mark[Data$Tag_start=="5662705"]<-"5663705"
Data$Corrected.Mark[Data$Tag_start=="5674869"]<-"5674861"
Data$Corrected.Mark[Data$Tag_start=="6470023"]<-"6470025"
Data$Corrected.Mark[Data$Tag_start=="6498828"]<-"6498878"
Data$Vial[Data$Tag_start=="5673892"]<-"vc32" #tag got lost and replaced
Data$Corrected.Mark[Data$Tag_start=="517254"]<-"10517254"
Data$Corrected.Mark[Data$Tag_start=="6470023"]<-"6470025"
Data$Corrected.Mark[Data$Tag_start=="8673949"]<-"5673949"
Data$Corrected.Mark[Data$Tag_start=="6487359"]<-"5673892"
Data$Vial[Data$Tag_start=="5806504"]<-"elb37" #pit tag lost
Data$Corrected.Mark[Data$Tag_start=="6502085"]<-"10502085" 
Data$Corrected.Mark[Data$Vial=="elb37"]<-"5806504" 
Data$Corrected.Mark[Data$Tag_start=="6484273"]<-"5674301" #process of elimination
Data$Comments[Data$Tag_start=="5674301" ]<-"pit tag lost and replaced with 6484273" 
Data$Corrected.Mark<-ifelse(is.na(Data$Corrected.Mark),Data$Tag_start,Data$Corrected.Mark)# any correct are put into 

write.csv(Data,"Data_corrected.csv")

traps <- read.csv("Data_corrected.csv",header=T)
traps_wv <- subset(traps,Species=="wv")

traps_wv_new <- subset(traps_wv,Capt=="n"|Capt=="nr")
traps_wv_new$Corrected.Mark <- as.numeric(as.character(traps_wv_new$Corrected.Mark))
traps_wv_new<-traps_wv_new[!is.na(traps_wv_new$Corrected.Mark),]
traps_wv_new$Vial<-as.character(traps_wv_new$Vial)

traps_wv_recap <- subset(traps_wv,Capt=="r")
traps_wv_recap$Corrected.Mark <- as.numeric(as.character(traps_wv_recap$Corrected.Mark))
traps_wv_recap <- traps_wv_recap[complete.cases(traps_wv_recap$Corrected.Mark),]
traps_wv_recap$Vial<-as.character(traps_wv_recap$Vial)

for (i in 1:length(traps_wv_recap$Capt))
{
  
  PIT <- traps_wv_recap[i,"Corrected.Mark"]
  PIT
  vialID <- traps_wv_new[traps_wv_new$Corrected.Mark==as.character(PIT),"Vial"]
  vialID <- vialID[complete.cases(vialID)]
  vialID
  if(length(vialID)>0){
    traps_wv_recap[i,"Vial"] <- vialID
  } else {
    traps_wv_recap[i,"Vial"] <- "Tag_not_Found"
  }
}

named_recaps_updated <- rbind(traps_wv_new,traps_wv_recap) # check that this has actually helped!
not_found <- subset(named_recaps_updated,Vial=="Tag_not_Found") ## subset samples where tags were not found

missing_tags <- unique(not_found[,"Tag_start"]) ## list of tags that were not found

pos_matches <- data.frame(Vial=NA,Patch=NA,Tag_start=NA,Iter=NA,Name=NA,Date=NA)

for(i in 1:length(missing_tags))
{
  
  tag_info <- subset(not_found,Tag_start==missing_tags[i], select=c("Vial","Patch","Tag_start","Name","Date"))
  
  match <- agrep(missing_tags[i],traps_wv_new$Tag_start, max.distance=1)
  
  match_res <- traps_wv_new[match,c("Vial","Patch","Tag_start","Name","Date")]
  
  match_df <- rbind(tag_info,match_res)
  
  match_df$Iter <- i
  
  pos_matches <- rbind(pos_matches,match_df)
  
}

All.other<-traps[!traps$Species=="wv",,]
wv.other<-traps_wv[traps_wv$Tag_start==""|is.na(traps_wv$Tag_start)|traps_wv$Tag_start=="ls"|traps_wv$Tag_start=="rs",]

Data_cleaned<-rbind(named_recaps_updated,All.other,wv.other)

#check that all r have a n or nr
new.capt<-Data_cleaned[Data_cleaned$Capt=="n"|Data_cleaned$Capt=="nr",]
re.capt<-Data_cleaned[Data_cleaned$Capt=="r",]

Name_comparison<- data.frame(re.capt=unique(re.capt$Vial))
Name_comparison$matches <- Name_comparison$re.capt %in% new.capt$Vial
False.recapts<- Name_comparison[Name_comparison$matches==FALSE,]
#one where Tag_notfound or hbNA


write.csv(Data_cleaned,"Data_cleaned_14.09.2020.csv")

####fv renaming 
Data<-read.csv("Data_cleaned_14.09.2020.csv")#noticed that fv had not been renamed yet 
fv<-Data[Data$Species=="fv",] #only look at fv data
fv_unnamed<-fv[fv$Vial==""&fv$Capt=="r",] 
fv_new<-fv[fv$Capt=="n"&!fv$Vial=="",] #there are some fv which did not reciave a name so I excluded those

#comparing the unnamed and named individuals by looking at fv_unnamed,check that only one vial name comes up
#clippings should be unique within patches 
fv_new[fv_new$Patch=="cro1015"&fv_new$Mark=="rh","Vial"] #lm233
fv_new[fv_new$Patch=="cro1015"&fv_new$Mark=="ls","Vial"] #elb22
fv_new[fv_new$Patch=="cro1015"&fv_new$Mark=="lh","Vial"] #elb28
fv_new[fv_new$Patch=="cro1015"&fv_new$Mark=="ls,rs","Vial"] #gp216
fv_new[fv_new$Patch=="cro1015"&fv_new$Mark=="rs","Vial"] #elb26

#in fv_unnamed fill in th correct vial name based on the query run 
fv_unnamed[fv_unnamed$Patch=="cro1015"&fv_unnamed$Mark=="rh","Vial"]<-"lm233"
fv_unnamed[fv_unnamed$Patch=="cro1015"&fv_unnamed$Mark=="ls","Vial"]<-"elb22"
fv_unnamed[fv_unnamed$Patch=="cro1015"&fv_unnamed$Mark=="lh","Vial"]<-"elb28"
fv_unnamed[fv_unnamed$Patch=="cro1015"&fv_unnamed$Mark=="ls,rs","Vial"]<-"gp216"
fv_unnamed[fv_unnamed$Patch=="cro1015"&fv_unnamed$Mark=="rs","Vial"]<-"elb26"

#remerge fv data 
fv_renamed<-rbind(fv[fv$Capt=="n"|fv$Vial=="hbNA"|fv$Vial=="elb33",],fv_unnamed) # merge fv data recpt and new capt together, include HBNA and elb33 as they are not in the unnamed fiel
Data_fv_rename<-rbind(fv_renamed,Data[!Data$Species=="fv",])

write.csv(Data_fv_rename,"20200923Data_cleaned.csv")

