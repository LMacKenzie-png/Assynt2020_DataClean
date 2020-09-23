# ASSYNT TRAPPING DATA CORRECTION CODE ####
#edited 23/09/2020
# This script is used to check that all trapping data relating to water vole surveys in Assynt
# has the correct georeferencing information and that the recorded block and patch names 
# correspond with blocks and patches defined in the survey design 


# SESSION SETUP AND LOADING OF FILES ####

# Load required packages ####
library(openxlsx) # used for reading .xlsx files
library(lubridate)

# Set Working Directory ####
setwd("~/Assynt2020_DataClean")

# Load required files ####
traps <- read.xlsx("20200923Assynt master trapping.xlsx") #10788

# Add a unique identifier to each entry in the dataset
traps$ID <- c(1:nrow(traps))

# Add a Comment column to hold all Comments
traps$Comment <- as.character(NA)

# Add a Corrections column to hold all correction comments
traps$Corrections <- as.character(NA)

# Add a newCorrections column to hold all new correction comments
traps$newCorrections <- as.character(NA)

# Add a newComments column to insert new comments
traps$newComment <- as.character(NA)

# Create update.Comments() function ####
# This will update Corrections and Comments columns in specified df #
# The function will be used after corrections are made to each variable
update.Comments <- function(data) {
  for (i in 1 : nrow(data)) {
    
    if (is.na(data[i, "Comment"]) == FALSE & is.na(data[i, "newComment"]) == FALSE) {
      data[i, "Comment"] <- paste(data[i, "Comment"], data[i, "newComment"])
      data[i, "newComment"] <- NA
    }
    if (is.na(data[i, "Comment"]) == TRUE & is.na(data[i, "newComment"]) == FALSE) {
      data[i, "Comment"] <- data[i, "newComment"]
      data[i, "newComment"] <- NA
    }
    if (is.na(data[i, "newComment"]) == FALSE) {
      next
    }
    
    if (is.na(data[i, "Corrections"]) == FALSE & is.na(data[i, "newCorrections"]) == FALSE) {
      data[i, "Corrections"] <- paste(data[i, "Corrections"], data[i, "newCorrections"])
      data[i, "newCorrections"] <- NA
    }
    if (is.na(data[i, "Corrections"]) == TRUE & is.na(data[i, "newCorrections"]) == FALSE) {
      data[i, "Corrections"] <- data[i, "newCorrections"]
      data[i, "newCorrections"] <- NA
    }
    if (is.na(data[i, "newCorrections"]) == FALSE) {
      next
    }
  }
  data
}





# Read in reference files for correct codes and data values, stored as Microsoft Excel spreadsheet
refList <- read.xlsx("Assynt_Data_Reference.xlsx")

str(traps)

# CORRECTING 'DATE' AND 'YEAR' DATA ####

# Create a new variable called "Date". We will populated this from a corrected version of the existing "Date2".
traps$Date <- as.Date(NA)

unique(traps$Date2)
# There are a few dates that have been entered in a different format (ie dd/mm/yyy)

# Some Dates were recorded in "dd/mm/yy" format
ind <- grep(pattern = "^([012]?\\d|3[01])(\\/)(0?\\d|1[012])(\\/)(\\d{4})$", x = traps$Date2)
traps[ind, "Date2"]
traps[ind, "newCorrections"] <- "Date format corrected."
traps[ind, "Date"] <- as.Date(traps[ind, "Date2"], format = "%d/%m/%Y")

# There are date values converted to 5 digit values by microsoft excel's date functions based in an 'origin date'
# For Excel on Windows, the origin date is 30 December 1899 for dates after 1900. 
# For Excel on Mac, the origin date is 1 January 1904.
ind <- grep(pattern = "\\d{5}", x = (traps$Date2)) # pattern contains 5 digits in a row
traps[ind, "Date2"]
traps[ind, "newCorrections"] <- "Date format corrected."
traps[ind, "Date"] <- as.Date(as.numeric(traps[ind, "Date2"]), origin = "1899-12-30")


# Check that new dates all make sense
unique(traps$Date)

# The follwing dates need to have days and month values switched: 
#   "2017-01-08"  "2017-01-08" "2017-01-08" "2017-05-08"
#"2017-06-08" "2017-06-08" "2017-06-08"
#"2017-06-08" "2017-04-08" "2017-03-08"

ind <- which(traps$Date == "2017-01-08" | 
               traps$Date == "2017-01-08" | 
               traps$Date == "2017-01-08" | 
               traps$Date == "2017-05-08" | 
               traps$Date == "2017-06-08" | 
               traps$Date == "2017-09-08" | 
               traps$Date == "2017-10-08" |
               traps$Date == "2017-03-08" |
               traps$Date == "2017-04-08" |
               traps$Date == "2017-12-07")

traps[ind, "Date"]
traps[ind, "newCorrections"] <- "Date format corrected m/d swopped."
traps[ind, "Date"] <- ydm(traps[ind, "Date"]) # ydm() function from lubridate package

unique(traps$Date)
traps <- update.Comments(traps)

# All dates are in an acceptable format

## CORRECTING 'MPATCH' ENTRIES IN DATA USING REFERENCE LIST ####

# Read in reference list of correct Mpatch names
MpatchRef <- unique(refList$Mpatch)

# Identify which Mpatch entries do not appear in the reference list and need correction
traps.patch <- data.frame(patches = unique(traps$Mpatch))
traps.patch$matches <-  traps.patch$patches %in% MpatchRef
no_match <- traps.patch[traps.patch$matches == FALSE, ]
no_match <- no_match[order(no_match$patches), ]
no_match$patches
len.match <- length(no_match$patches)
paste(len.match, "patches don't match the reference list of merged patches names")


# Replace incorrect entries with correct entries as per reference list
{## run all lines of code for patch correction and addition of correction comments
  
  # cro10 / cro1115 / cro12 / cro13
  rowCorrection <- which(traps$Mpatch == "cro10" | traps$Mpatch == "cro1115" | traps$Mpatch == "cro12" | traps$Mpatch == "cro13")
  traps[rowCorrection, ]
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "cro1015"
  
  # lei05 / lei04 / lei0203
  rowCorrection <- which(traps$Mpatch == "lei05" | traps$Mpatch == "lei04" | traps$Mpatch == "lei0203")
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "lei0205"
  
  # sgi02
  rowCorrection <- which(traps$Mpatch == "sgi02")
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "sgi0103"
  
  # sginp4
  rowCorrection <- which(traps$Mpatch == "sginp4")
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "sgi39"
  
  # sgi45
  rowCorrection <- which(traps$Mpatch == "sgi45")
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "sgi45"   # !!!!!! CHECK WHAT THIS SHOULD BE CHANGED TO
  
  
  # led30
  rowCorrection <- which(traps$Mpatch == "led30")
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "led30"   # !!!!!! CHECK WHAT THIS SHOULD BE CHANGED TO
  
  
  
  # cro20 / cro21 / cro2223
  rowCorrection <- which(traps$Mpatch == "cro20" | traps$Mpatch == "cro21" | traps$Mpatch == "cro2223")
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "cro2023"
  
  
  # sgi1013
  rowCorrection <- which(traps$Mpatch == "sgi1013")
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "sgi1213"
  
  # cro0708b
  rowCorrection <- which(traps$Mpatch == "cro0708b")
  traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  traps[rowCorrection, "Mpatch"] <- "cro0708"
  
  
  # MERGES TO CHECK WITH LAURA:
  # # sgi02
  # rowCorrection <- which(traps$Mpatch == "sgi02")
  # traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  # traps[rowCorrection, "Mpatch"] <- "sgi0103"
  
  # # cro12 / cro13
  # rowCorrection <- which(traps$Mpatch == "cro12" | traps$Mpatch == "cro13")
  # traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  # traps[rowCorrection, "Mpatch"] <- "cro1015"
  
  
  # # lei04
  # rowCorrection <- which(traps$Mpatch == "lei04")
  # traps[rowCorrection,"newCorrections"] <- paste0("Mpatch corrected from ", traps[rowCorrection, "Mpatch"], " using reference list.")
  # traps[rowCorrection, "Mpatch"] <- "lei0304"
  
  
  
  
  
}

traps.patch <- data.frame(patches = unique(traps$Mpatch))
patch <- data.frame(patch = unique(traps$Mpatch)) ## get corrected data
traps.patch$matches <-  traps.patch$patches %in% MpatchRef
no_match <- traps.patch[traps.patch$matches == FALSE, ]
no_match <- no_match[order(no_match$patches), ]
no_match$patches
len.match <- length(no_match$patches)
paste(len.match, "patches don't match the reference list of merged patches names") ## if not 0 then find errors

# Update the Comments and Corrections columns using the update.Comments() function
traps <- update.Comments(traps)


# CORRECTING 'SEX' COLUMN ##### 

unique(traps$Sex) ## should only contain 3 levels "m", "f"  & NA

{## run all lines of code for 'patch'Sex' correction and addition of correction comments
  
  # f_ / F
  rowCorrection <- which(traps$Sex == "f " | traps$Sex == "F")
  traps[rowCorrection,"newCorrections"] <- paste0("Sex corrected from ", traps[rowCorrection, "Sex"], " using reference list.")
  traps[rowCorrection, "Sex"] <- "f"
  
  # ? / m? / na
  rowCorrection <- which(traps$Sex == "?" | traps$Sex == "m?" | traps$Sex == "na")
  traps[rowCorrection,"newCorrections"] <- paste0("Sex corrected from ", traps[rowCorrection, "Sex"], " using reference list.")
  traps[rowCorrection, "Sex"] <- NA
  
  # m_ / M
  rowCorrection <- which(traps$Sex == "m " | traps$Sex == "M")
  traps[rowCorrection,"newCorrections"] <- paste0("Sex corrected from ", traps[rowCorrection, "Sex"], " using reference list.")
  traps[rowCorrection, "Sex"] <- "m"
  
  # jv / juv
  rowCorrection <- which(traps$Sex == "jv" | traps$Sex == "juv")
  traps[rowCorrection,"newCorrections"] <- paste0("Sex corrected from ", traps[rowCorrection, "Sex"], " using reference list.")
  traps[rowCorrection, "Sex"] <- NA
  
}

unique(traps$Sex) ## check if corrections have worked

# Update the Comments and Corrections columns using the update.Comments() function
traps <- update.Comments(traps)

# TRAP SESSION ####
unique(traps$Trap.session)


# AREA ####
unique(traps$Area)
traps[is.na(traps$Area) == TRUE, ]
traps$Area[is.na(traps$Area)]<-"Assynt"

# NETWORK ####
unique(traps$Network)

# convert all 'Network' entries to lower case
traps$Network <- tolower(traps$Network)
unique(traps$Network)

# Identify if any 'Network' entries do not appear in the reference list and need correction
NetworkRef <- unique(refList$Block)
traps.network <- data.frame(network = unique(traps$Network))
traps.network$matches <-  traps.network$network %in% NetworkRef
no_match <- traps.network[traps.network$matches == FALSE, ]
no_match <- no_match[order(no_match$network), ]
no_match$network



# Correct incorrect 'Network' entries
{ 
  # cro_
  rowCorrection <- which(traps$Network == "cro ")
  traps[rowCorrection,"newCorrections"] <- paste0("Network corrected from ", traps[rowCorrection, "Network"], " using reference list.")
  traps[rowCorrection, "Network"] <- "cro"
}

unique(traps$Network)
# Update Comments and Corrections
traps <- update.Comments(traps)


# TRAP ####
unique(traps$Trap)
traps$Trap<-tolower(traps$Trap)
# Note: There is no reference list for trap nomenclture. At best, character strings could be standardised to upper or lower case.

#FLAG###
unique(traps$Flag)
traps$Flag<-tolower(traps$Flag)
# Note: There is no reference list for flag nomenclture. At best, character strings could be standardised to upper or lower case.

# CHECK ####
unique(traps$Check)
# All seems in order

# SPECIES ####
# Some observations have been recorded using incorrect references for species
# We can compare the data with a reference file and make neccessary corrections
unique(traps$Species)


# Read in the reference file for correct data values and codes
speciesRef <- unique(refList$check_x) # unique values for 'Code' in the reference list.
species <- data.frame(species = unique(traps$Species))  # create a new data frame using the unique 'Species' values that appear in the data set
species$matches <- species$species %in% speciesRef # add a new TRUE/FALSE column indicating whether the 'Species' values in the data set appear in the reference list
no.match <- species[species$matches == FALSE, "species"] #  identify the values which do not appear in the reference list
no.match

# Make corrections and add correction comment to data

{
  # w.vole
  rowCorrection <- which(traps$Species == "w.vole"|traps$Species == "w.vole "|traps$Species == "wv ")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "wv"
  
  # f.vole
  rowCorrection <- which(traps$Species == "f.vole")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "fv"
  
  # w.mouse
  rowCorrection <- which(traps$Species == "w.mouse")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "wm"
  
  # weasel
  rowCorrection <- which(traps$Species == "weasel")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "wl"
  
  # p.shrew
  rowCorrection <- which(traps$Species == "p.shrew")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "ps"
  
  # toad
  rowCorrection <- which(traps$Species == "toad")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection,"newComment"] <- paste0("Species was ", traps[rowCorrection, "Species"], ".")
  traps[rowCorrection, "Species"] <- "other"
  
  # frog
  rowCorrection <- which(traps$Species == "frog")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "fg"
  
  # w.shrew
  rowCorrection <- which(traps$Species == "w.shrew")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "ws"
  
  # c.shrew
  rowCorrection <- which(traps$Species == "c.shrew")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "cs"
  
  # stoat
  rowCorrection <- which(traps$Species == "stoat")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection,"newComment"] <- paste0("Species was ", traps[rowCorrection, "Species"], ".")
  traps[rowCorrection, "Species"] <- "other"
  
  # wea
  rowCorrection <- which(traps$Species == "wea")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "wl"
  
  # f.trigger
  rowCorrection <- which(traps$Species == "f.trigger")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "ft"
  
  # st
  rowCorrection <- which(traps$Species == "st")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "other"
  
  #rabbit
  rowCorrection <- which(traps$Species == "rabbit")
  traps[rowCorrection,"newCorrections"] <- paste0("Species corrected from ", traps[rowCorrection, "Species"], " using reference list.")
  traps[rowCorrection, "Species"] <- "ra"
  
}

# Check for any further mismatches
species <- data.frame(species = unique(traps$Species))  # create a new data frame using the unique 'Species' values that appear in the data set
species$matches <- species$species %in% speciesRef # add a new TRUE/FALSE column indicating whether the 'Species' values in the data set appear in the reference list
no.match <- species[species$matches == FALSE, "species"] #  identify the values which do not appear in the reference list
no.match # The only remaining mismatch should be 'wv' which is acceptable.

unique(traps$Species)
# Update Comments and Corrections
traps <- update.Comments(traps)


# ALIVE ####
unique(traps$Alive)

# Make corrections for uppercase characters
rowCorrection <- which(traps$Alive == "Alive")
traps[rowCorrection,"newCorrections"] <- paste0("Alive corrected from ", traps[rowCorrection, "Alive"], " .")
traps[rowCorrection, "Alive"] <- "alive"

unique(traps$Alive) # recheck

# Update Comments and Corrections
traps <- update.Comments(traps)


# ID1 ####
unique(traps$ID1)
traps$ID1
#ID2#
unique(traps$ID2)

# CAPT ####
# Whether the vole is a new (n), a re-capture (r) or a vole from the previous year (nr)
unique(traps$Capt) # check levels in capture (should have 4 levels "n", "nr", "r", NA)

# Make corrections for uppercase characters
rowCorrection <- which(traps$Capt == "N"|traps$Capt == "n ")
traps[rowCorrection,"newCorrections"] <- paste0("Capt corrected from ", traps[rowCorrection, "Capt"], " .")
traps[rowCorrection, "Capt"] <- "n"

rowCorrection <- which(traps$Capt == "R"|traps$Capt == "r ")
traps[rowCorrection,"newCorrections"] <- paste0("Capt corrected from ", traps[rowCorrection, "Capt"], " .")
traps[rowCorrection, "Capt"] <- "r"

unique(traps$Capt)

traps <- update.Comments(traps)

# If the species caught in the trap is not a water vole or a field vole, then the value of 'Capt' should be NA
traps[traps$Species != "wv" & traps$Species != "fv" & is.na(traps$Capt) == FALSE, ] # these observations have incorrect 'Capt' values

# Correct 'Capt' values for Species not equal to 'wv' or 'fv'. These values should be NA.
rowCorrection <- which(traps$Species != "wv" & traps$Species != "fv" & is.na(traps$Capt) == FALSE)
traps[rowCorrection,"newCorrections"] <- paste0("Capt corrected from ", traps[rowCorrection, "Capt"], " based on value of 'Species'.")
traps[rowCorrection, "Capt"] <- NA

traps[which(is.na(traps$Species) == TRUE & (traps$Capt == "n" | traps$Capt == "nr")), ] # there are some entries where
# the values of 'Species' = NA and 'Capt' has a value of "n" or "nr". These 'Capt' values need to be corrected to NA

# Make corrections for these incorrect 'Capt' values
rowCorrection <- which(is.na(traps$Species) == TRUE & (traps$Capt == "n" | traps$Capt == "nr"))
traps[rowCorrection,"newCorrections"] <- paste0("Capt corrected from ", traps[rowCorrection, "Capt"], " based on value of 'Species'.")
traps[rowCorrection, "Capt"] <- NA

# Update Comments and Corrections
traps <- update.Comments(traps)


# CAPT.IND ####
# CAPT.ind should have a "1" if Capt value is "n" or "nr" and
# CAPT.ind should have a "0" if Capt value is anything other than "n" or "nr".
# This column will be removed as redundant. In the future, any such index will be easy to generate based on the value of 'Capt'.

# There are a few 
unique(traps$CAPT.ind)

# Check if 'Capt' values of n or nr have a 'CAPT.ind' value of 1
traps[which((traps$Capt == "n" | traps$Capt == "nr") & traps$CAPT.ind != 1), ]
unique(traps[which(traps$Capt == "n" | traps$Capt == "nr"), "CAPT.ind" ])
unique(traps[(traps$Capt == "n" | traps$Capt == "nr"), "CAPT.ind" ])

# Correct these 'CAPT.ind' values to 1
rowCorrection <- which((traps$Capt == "n" | traps$Capt == "nr") & traps$CAPT.ind != 1)
traps[rowCorrection, "newCorrections"] <- paste0("CAPT.ind corrected from ", traps$CAPT.ind[rowCorrection], " based on value of 'Capt'.")
traps[rowCorrection, "CAPT.ind"] <- 1

rowCorrection <- which((traps$Capt == "n" | traps$Capt == "nr") & (is.na(traps$CAPT.ind) == TRUE))
traps[rowCorrection, "newCorrections"] <- paste0("CAPT.ind corrected from ", traps$CAPT.ind[rowCorrection], " based on value of 'Capt'.")
traps[rowCorrection, "CAPT.ind"] <- 1

# Check if 'Capt' values not equal to n or nr have a 'CAPT.ind' value of 0
traps[which((traps$Capt != "n" & traps$Capt != "nr") & traps$CAPT.ind != 0), ] # all seems in order

# Check if 'Capt' values = NA have a CAPT.ind value of 0
traps[which(is.na(traps$Capt) == TRUE & traps$CAPT.ind != 0), ] # there are corrections required


# Correct these 'CAPT.ind' values to 0
rowCorrection <- which(is.na(traps$Capt) == TRUE & traps$CAPT.ind != 0)
traps[rowCorrection,"newCorrections"] <- paste0("CAPT.ind corrected from ", traps[rowCorrection, "CAPT.ind"], " based on value of 'Capt'.")
traps[rowCorrection, "CAPT.ind"] <- 0

# Update Comments and Corrections
traps <- update.Comments(traps)

traps[is.na(traps$Capt) == TRUE, "CAPT.ind"]

unique(traps[traps$CAPT.ind == 1, "Capt"])
unique(traps[traps$Capt == "n" | traps$Capt == "nr", "Species"])

unique(traps[traps$CAPT.ind == 0, "Capt"])


traps[which(traps$CAPT.ind == 0 & traps$Capt == "n"), ] 


# Update Comments and Corrections
traps <- update.Comments(traps)


# PAST.LOCATION ####
unique(traps$Past.location)

# Check that all Patch names correspond with reference list

# Identify which 'Past.location' entries do not appear in the reference list and need correction
traps.past.loc <- data.frame(past.loc = unique(traps$Past.location))
traps.past.loc$matches <-  traps.past.loc$past.loc %in% MpatchRef
no_match <- traps.past.loc[traps.past.loc$matches == FALSE, ]
no_match <- no_match[order(no_match$past.loc), ]
no_match$past.loc


# Correct past.location names that have incorrect names

{
  #cro12
  rowCorrection <- which(traps$Past.location == "cro12" )
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "cro1015"
  
  # cro10
  rowCorrection <- which(traps$Past.location == "cro10" )
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "cro1015"
  
  # cro01_03
  rowCorrection <- which(traps$Past.location == "cro01_03" )
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "cro0103"
  
  # cro10_15
  rowCorrection <- which(traps$Past.location == "cro10_15" )
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "cro1015"
  
  # lei02 / lei03 / lei05
  rowCorrection <- which(traps$Past.location == "lei02" | traps$Past.location == "lei03" | traps$Past.location == "lei05")
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "lei0205"
  
  # sgi01
  rowCorrection <- which(traps$Past.location == "sgi01" )
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "sgi0103"
  
  # sgi07
  rowCorrection <- which(traps$Past.location == "sgi07" )
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "sgi0407"
  
  
  # sgi08 / sgi09
  rowCorrection <- which(traps$Past.location == "sgi08" | traps$Past.location == "sgi09")
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "sgi0810"
  
  # sgi12
  rowCorrection <- which(traps$Past.location == "sgi12")
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "sgi1213"
  
  # sgi37
  rowCorrection <- which(traps$Past.location == "sgi37")
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "sgi3637"
  
  
  # sgi40
  rowCorrection <- which(traps$Past.location == "sgi40")
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "sgi4043"
  
  # led27
  rowCorrection <- which(traps$Past.location == "led27")
  traps[rowCorrection,"newCorrections"] <- paste0("Past.location corrected from ", traps[rowCorrection, "Past.location"], " using reference list.")
  traps[rowCorrection, "Past.location"] <- "led2728"
  
}

# Update Comments and Corrections
traps <- update.Comments(traps)

#Network
unique(traps$Network)
traps$Network<-as.factor(substr(traps$Mpatch,1,3))
unique(traps$Network)
#only entries with odd or no patches ="NA" 

# AV.ID ####
unique(traps$AV.id)
# All seems in order


# EAR.1 ####
unique(traps$Ear.1)

# This shows a combination of different types of data collected. Ear tags usually consist of numbers (eg. "2386")
# Whereas fur clippings from different areas of the body are have been recorded using letters (eg "rh" meaning 'right hind')

# First, we can standardise the data to lower case characters
traps$Ear.1 <- tolower(traps$Ear.1)


# Duplicate 'Ear.1' as a temporary vector. Clear Ear.1 and re-insert only numeric entries. Remaining entries saved in a new 'Fur_clip' vector.
traps$Ear.1.Temp <- traps$Ear.1 # Duplicate Ear.1 to temporary vector.
traps$Ear.1 <- NA # Clear 'Ear.1'


# ind <- grep(pattern = "^[0-9]+$", traps$Ear.1.Temp) # identify rows that contain only numbers for 'Ear.1'
# traps$Ear.1[ind] <- traps$Ear.1.Temp[ind]

# identify rows that contain at least two digits
ind <- grep(pattern = "\\d", traps$Ear.1.Temp) # identify rows that contain at least one
unique(traps$Ear.1.Temp[ind])
traps$Ear.1[ind] <- traps$Ear.1.Temp[ind] # insert these numeric values back into 'Ear.1'

# Remaining entries can be stored in a new vector called 'Fur_clip'
traps$Fur_clip <- NA
traps$Fur_clip[-ind] <- traps$Ear.1.Temp[-ind]
unique(traps$Fur_clip)


# There are some Ear.1 values that can be cleaned up:

# The word 'dead' can be removed from 'Ear.1'. This information was captured in the 'Alive' column.
traps$Ear.1 <- gsub(pattern = "dead", replacement = "", x = traps$Ear.1)
unique(traps$Ear.1)

# There is a recording of "RHB+t8528:t8945?" which can be moved to the 'Fur_clip' column
ind <- grep(pattern = "+t8528:t8945?", traps$Ear.1) # identify rows that contain the sequence
traps$Ear.1[ind] <- NA
traps$Fur_clip[ind] <- traps$Ear.1.Temp[ind]

# There are some Fur_clip values that can be cleaned up:
unique(traps$Fur_clip)

# There are Fur_clip values entered as "Field vole". This information can be removed and was captured in the 'Species' column.
ind <- grep(pattern = "field vole", traps$Fur_clip)
traps$Fur_clip[ind] <- NA

# There are Fur_clip values entered as "fv". This information can be removed and was captured in the 'Species' column.
# One entry had Species entered as'wv' with a 'fv' recorded under Fur-clip.
rowCorrection <- which(traps$Species == "wv" & traps$Fur_clip == "fv")
traps[rowCorrection, "newComment"] <- "Species recorded as 'wv' but 'fv' orginally recorded in Ear.1/Fur_clip - recommend delete entry"
traps[rowCorrection, "Fur_clip"] <- NA

ind <- grep(pattern = "fv", traps$Fur_clip)
traps$Fur_clip[ind] <- NA

# There are some 'Fur_clip values entered as 'last day'. These can be moved to the 'newComment' column.
ind <- grep(pattern = "last day", traps$Fur_clip)
traps$newComment[ind] <- "last day."
traps$Fur_clip[ind] <- NA

# There are 'Fur_clip' values entered as 'N/A'. These need to be corrected to NA.
traps$Fur_clip <- gsub(pattern = "n/a", replacement = NA, x = traps$Fur_clip)

# There are 'Fur_clip' values entered as 'na. These need to be corrected to NA.
traps$Fur_clip <- gsub(pattern = "na", replacement = NA, x = traps$Fur_clip)
unique(traps$Fur_clip)
# Remove spaces in strings
traps$Fur_clip <- gsub(pattern = " ", replacement = "", x = traps$Fur_clip)

# Remove commas in strings
traps$Fur_clip <- gsub(pattern = ",", replacement = "", x = traps$Fur_clip)

# Remove forward slashes '/' in strings
traps$Fur_clip <- gsub(pattern = "/", replacement = "", x = traps$Fur_clip)


#Remove ? 
traps$Fur_clip <- gsub(pattern = "\\?", replacement = "", x = traps$Fur_clip)
unique(traps$Fur_clip)


# Remove temporary columns created above
traps$Ear.1.Temp <- NULL
str(traps)


# NOTE: 'Fur_clip' data could be further standardised and cleaned up but will not be prioritised in this excercise.

traps <-  update.Comments(traps)

# EAR.2 ####
unique(traps$Ear.2)
# All seems in order 


# NEW.EAR.3 ####
unique(traps$New.ear.3)
# All seems in order

# PIT.1 ####
unique(traps$Pit.1)
# There are 'Pit.1' values entered as 'Na. These need to be corrected to NA.
traps$Pit.1 <- gsub(pattern = "Na", replacement = NA, x = traps$Pit.1)


# PUNCH ####
unique(traps$Punch)

# Standardise to upper case
traps$Punch <- toupper(traps$Punch)

# Convert all "NA" strings to NA
traps[which(traps$Punch == "NA"), "Punch"] <- NA

# Standardise binomial to either Y/N or 1/0.
rowCorrection <- which(traps$Punch == "1")
traps[rowCorrection, "newCorrections"] <- paste0("'Punch' corrected from ", traps[rowCorrection, "Punch"], ".")
traps[rowCorrection, "Punch"] <- "Y"

rowCorrection <- which(traps$Punch == "0")
traps[rowCorrection, "newCorrections"] <- paste0("'Punch' corrected from ", traps[rowCorrection, "Punch"], ".")
traps[rowCorrection, "Punch"] <- "N"
unique(traps$Punch)

traps <- update.Comments(traps)


# VIAL ####
# There are two columns named 'Vial' in this data set. We can rename them Vial.a and Vial temporarily:
#names(traps)[1] <- "Vial.a"


# The two Vial columns have similar data, although the second one contains data in a standardised format.
# This standardised format includes two capital letters (initials of person taking the sample) followed by a 4-digit
# number which should be sequential and not repeated with the two intials during the survey. eg: "JD0023"

traps[ ,c("Vial1", "Vial")]
length(unique(traps[ , "Vial1"]))
length(unique(traps[ , "Vial"])) # This column contains more entries

rowCheckVial <- which(is.na(traps[ ,"Vial1"]) == FALSE & is.na(traps[ ,"Vial"]) == TRUE)
traps[rowCheckVial, c("Vial1", "Vial")]


rowCheck <- which(is.na(traps[, "Vial1"]) == FALSE & is.na(traps[, "Vial"]) == TRUE)
length(rowCheck)

rowCheck <- which(is.na(traps[, "Vial1"]) == TRUE & is.na(traps[, "Vial"]) == FALSE)
length(rowCheck)
# There are instances of an entry occuring in 'Vial1' but not in 'Vial'. 
# So relatively safe to assume that 'Vial contains all of the required data, although it has not been standardised.

# Standardise 'Vial' data
traps$Vial <- tolower(traps$Vial) # all letters to lower case
unique(traps$Vial)
# Identify entries with less than 4 digits after initials and correct with adding 0's.

# Vial values containing only 1 digit
#ind <- grep(pattern = "^[A-Z]+\\d$", x = traps$Vial) # look for a pattern starting with one or more letters followed by and ending with one digit only
#traps$Vial[ind]
#traps$newCorrections[ind] <- paste0("Vial standardised from ", traps$Vial[ind], ".") # Add comment about the correction.
#VialNo <- substr(traps$Vial[ind], nchar(traps$Vial[ind]), nchar(traps$Vial[ind])) # Extract the digits
#VialNo
#VialChars <- substr(traps$Vial[ind], 1, (nchar(traps$Vial[ind]) -1)) # Extract the characters before the digits
#VialChars

# Combine characters, missing 0's and original digits
#for (i in 1 : length(traps$Vial[ind])) {
#  traps$Vial[ind][i] <- paste0(VialChars[i], "000", VialNo[i])
#}


# Vial values containing only 2 digits
#ind <- grep(pattern = "^[A-Z]+\\d\\d$", x = traps$Vial) # look for a pattern starting with one or more letters followed by and ending with two digits only
#traps$Vial[ind]
##traps$newCorrections[ind] <- paste0("Vial standardised from ", traps$Vial[ind], ".") # Add comment about the correction.
#VialNo <- substr(traps$Vial[ind], nchar(traps$Vial[ind]) -1, nchar(traps$Vial[ind])) # Extract the digits
#VialNo
#VialChars <- substr(traps$Vial[ind], 1, (nchar(traps$Vial[ind]) -2)) # Extract the characters before the digits
#VialChars

# Combine characters, missing 0's and original digits
#for (i in 1 : length(traps$Vial[ind])) {
#  traps$Vial[ind][i] <- paste0(VialChars[i], "00", VialNo[i])
#}


# Vial values containing only 3 digits
#ind <- grep(pattern = "^[A-Z]+\\d\\d\\d$", x = traps$Vial) # look for a pattern starting with one or more letters followed by and ending with three digits only
#traps$Vial[ind]
#traps$newCorrections[ind] <- paste0("Vial standardised from ", traps$Vial[ind], ".") # Add comment about the correction.
#VialNo <- substr(traps$Vial[ind], nchar(traps$Vial[ind]) -2, nchar(traps$Vial[ind])) # Extract the digits
#VialNo
#VialChars <- substr(traps$Vial[ind], 1, (nchar(traps$Vial[ind]) -3)) # Extract the characters before the digits
#VialChars

# Combine characters, missing 0's and original digits
#for (i in 1 : length(traps$Vial[ind])) {
#  traps$Vial[ind][i] <- paste0(VialChars[i], "0", VialNo[i])
#}

# NOTE: There are still a few entries which migh not conform to the formatting and these will need to be dealt with case by case.
# for example:
#ind <- grep(pattern = "[A-Z]+$", x = traps$Vial) # look for a pattern starting and ending with characters
#traps$Vial[ind]

#traps <- update.Comments(traps)



# GENE.SAMP ####
# Gene.samp data was collected between the years of 2001, 2002, and 2003 only. It is no longer maintained.
# This data will therefore be kept in a seperate data set in order to keep the master trapping file more stream-lined.

unique(traps[is.na(traps$Gene.samp) == FALSE, "Year" ])

Assynt_Trapping_GeneSamp_2001_2003 <- traps[is.na(traps$Gene.samp) == FALSE ,c("Mpatch", "Date", "Gene.samp", "Unique.ID")]

# TESTES ####
# 'Testes' should only contain the following values:
# ab	abdominal testes
# ms	midscrotal testes
# ns	non-scrotal testes
# scr	scrotal testes

unique(traps$Testes) # There are some entries that need correction

traps$Testes <- tolower(traps$Testes) # standardise to lower case letters

# Make neccessary corrections:
{
  # scr_ / s
  rowCorrection <- which(traps$Testes == "scr "| traps$Testes == "s")
  traps[rowCorrection, "newCorrections"] <- paste0("'Testes' corrected from ", traps[rowCorrection, "Testes"], ".")
  traps[rowCorrection, "Testes"] <- "scr"
  
  # ns 
  rowCorrection <- which(traps$Testes == "ns ")
  traps[rowCorrection, "newCorrections"] <- paste0("'Testes' corrected from ", traps[rowCorrection, "Testes"], ".")
  traps[rowCorrection, "Testes"] <- "ns"
  
  # ns 
  rowCorrection <- which(traps$Testes == "ns ")
  traps[rowCorrection, "newCorrections"] <- paste0("'Testes' corrected from ", traps[rowCorrection, "Testes"], ".")
  traps[rowCorrection, "Testes"] <- "ns"
  
  # a / as 
  rowCorrection <- which(traps$Testes == "a" | traps$Testes == "as")
  traps[rowCorrection, "newCorrections"] <- paste0("'Testes' corrected from ", traps[rowCorrection, "Testes"], ".")
  traps[rowCorrection, "Testes"] <- "ab"
  
  # s? / na 
  rowCorrection <- which(traps$Testes == "s?" | traps$Testes == "na")
  traps[rowCorrection, "newCorrections"] <- paste0("'Testes' corrected from ", traps[rowCorrection, "Testes"], ".")
  traps[rowCorrection, "Testes"] <- NA
  
}

unique(traps$Testes) # all seems in order now

traps <- update.Comments(traps)  


# VAGINA ####

# 'Vagina' should only contain the following values:
# np	      Not perforated
# perf	    Perforated
# sp.plug	  Sperm Plug

unique(traps$Vagina)

traps$Vagina <- tolower(traps$Vagina) # standardise to lower case letters

# Make neccessary corrections:
{
  # perf_ / p / y
  rowCorrection <- which(traps$Vagina == "perf " | traps$Vagina == "p"| traps$Vagina == "y")
  traps[rowCorrection, "newCorrections"] <- paste0("'Vagina' corrected from ", traps[rowCorrection, "Vagina"], ".")
  traps[rowCorrection, "Vagina"] <- "perf"
  
  # np_ / not perf
  rowCorrection <- which(traps$Vagina == "np " | traps$Vagina == "n"| traps$Vagina == "not perf")
  traps[rowCorrection, "newCorrections"] <- paste0("'Vagina' corrected from ", traps[rowCorrection, "Vagina"], ".")
  traps[rowCorrection, "Vagina"] <- "np"
  
  # o / ? / na
  rowCorrection <- which(traps$Vagina == "o" | traps$Vagina == "?"| traps$Vagina == "na")
  traps[rowCorrection, "newCorrections"] <- paste0("'Vagina' corrected from ", traps[rowCorrection, "Vagina"], ".")
  traps[rowCorrection, "Vagina"] <- NA
  
}

unique(traps$Vagina) # all seems in order now.

traps <- update.Comments(traps)

# PUBIC.BONE ####

# 'Pubic.bone' should only contain the following values:
# closed	  Pubic bone fused
# open	    Pubic bone seperated and determined via palpitation
# NA

unique(traps$Pubic.bone)

traps$Pubic.bone <- tolower(traps$Pubic.bone) # standardise to lower case letters

# Make neccessary corrections:
{
  # na
  rowCorrection <- which(traps$Pubic.bone == "na")
  traps[rowCorrection, "newCorrections"] <- paste0("'Pubic.bone' corrected from ", traps[rowCorrection, "Pubic.bone"], ".")
  traps[rowCorrection, "Pubic.bone"] <- NA
  
  # o
  rowCorrection <- which(traps$Pubic.bone == "o")
  traps[rowCorrection, "newCorrections"] <- paste0("'Pubic.bone' corrected from ", traps[rowCorrection, "Pubic.bone"], ".")
  traps[rowCorrection, "Pubic.bone"] <- "open"
  
  # c / closed_
  rowCorrection <- which(traps$Pubic.bone == "c" | traps$Pubic.bone == "closed ")
  traps[rowCorrection, "newCorrections"] <- paste0("'Pubic.bone' corrected from ", traps[rowCorrection, "Pubic.bone"], ".")
  traps[rowCorrection, "Pubic.bone"] <- "closed"
  
}

traps <- update.Comments(traps)



# REPRUDUCTIVE ####

unique(traps$Reproductive)

unique(traps[is.na(traps$Reproductive) == FALSE, "Year"])

# The 'Reproductive' data was only recorded between 2001 and 2003. We can therefore store it in a seperate data set and simplify the master trapping data.
Assynt_Trapping_Reproductive_2001_2003 <- traps[is.na(traps$Reproductive) == FALSE ,c("Mpatch", "Date", "Reproductive", "Unique.ID")]


# PREGNANT ####
# Rename variable from 'PregNAnt' to 'Pregnant'
unique(traps$Pregnant)

# Standardise all entries to upper case
traps$Pregnant <- toupper(traps$Pregnant)

# Convert all "NA" strings to NA
traps[which(traps$Pregnant == "NA"), "Pregnant"] <- NA

# Convert unclear data to NA
rowCorrection <- which(traps$Pregnant == "NO?" | traps$Pregnant == "N?")
traps[rowCorrection, "newCorrections"] <- paste0("'Pregnant' corrected from ", traps[rowCorrection, "Pregnant"], ".")
traps[rowCorrection, "Pregnant"] <- NA

# Standardise binomial classification to Y/N or 1/0.
rowCorrection <- which(traps$Pregnant == "1" | traps$Pregnant == "YES")
traps[rowCorrection, "newCorrections"] <- paste0("'Pregnant' corrected from ", traps[rowCorrection, "Pregnant"], ".")
traps[rowCorrection, "Pregnant"] <- "Y"

rowCorrection <- which(traps$Pregnant == "0" | traps$Pregnant == "O" | traps$Pregnant == "NO" | traps$Pregnant == "NO ")
traps[rowCorrection, "newCorrections"] <- paste0("'Pregnant' corrected from ", traps[rowCorrection, "Pregnant"], ".")
traps[rowCorrection, "Pregnant"] <- "N"
unique(traps$Pregnant)

traps <- update.Comments(traps)



# NIPPLES ####

# 'Nipples' data should only have the following numerical values:
# 0	    no
# 1	    small
# 2	    med
# 3	    large
# NA

unique(traps$Nipples)

# Make corrections to data

# ? / 4 / n/a / na / Na
rowCorrection <- which(traps$Nipples == "?" | traps$Nipples == "4" | traps$Nipples == "n/a" | traps$Nipples == "na" | traps$Nipples == "Na")
traps[rowCorrection, "newCorrections"] <- paste0("'Nipples' corrected from ", traps[rowCorrection, "Nipples"], ".")
traps[rowCorrection, "Nipples"] <- NA

# no / no_ / N / n
rowCorrection <- which(traps$Nipples == "no" | traps$Nipples == "no " | traps$Nipples == "N" | traps$Nipples == "n" | traps$Nipples == "o")
traps[rowCorrection, "newCorrections"] <- paste0("'Nipples' corrected from ", traps[rowCorrection, "Nipples"], ".")
traps[rowCorrection, "Nipples"] <- "0"

# med
rowCorrection <- which(traps$Nipples == "med")
traps[rowCorrection, "newCorrections"] <- paste0("'Nipples' corrected from ", traps[rowCorrection, "Nipples"], ".")
traps[rowCorrection, "Nipples"] <- "2"

# large / L
rowCorrection <- which(traps$Nipples == "large" | traps$Nipples == "L")
traps[rowCorrection, "newCorrections"] <- paste0("'Nipples' corrected from ", traps[rowCorrection, "Nipples"], ".")
traps[rowCorrection, "Nipples"] <- "3"

# small
rowCorrection <- which(traps$Nipples == "small")
traps[rowCorrection, "newCorrections"] <- paste0("'Nipples' corrected from ", traps[rowCorrection, "Nipples"], ".")
traps[rowCorrection, "Nipples"] <- "1"

traps$Nipples <- as.numeric(traps$Nipples)
unique(traps$Nipples)
traps <- update.Comments(traps)



# LACTATING ####

unique(traps$Lactating)

# Standardise all entries to upper case
traps$Lactating <- toupper(traps$Lactating)

# Convert all "NA" strings to NA
traps[which(traps$Lactating == "NA"), "Lactating"] <- NA

# Standardise binomial classification to Y/N or 1/0.
rowCorrection <- which(traps$Lactating == "1"|traps$Lactating == "YES")
traps[rowCorrection, "newCorrections"] <- paste0("'Lactating' corrected from ", traps[rowCorrection, "Lactating"], ".")
traps[rowCorrection, "Lactating"] <- "Y"

rowCorrection <- which(traps$Lactating == "0" | traps$Lactating == "O" | traps$Lactating == "NO")
traps[rowCorrection, "newCorrections"] <- paste0("'Lactating' corrected from ", traps[rowCorrection, "Lactating"], ".")
traps[rowCorrection, "Lactating"] <- "N"


unique(traps$Lactating)
traps <- update.Comments(traps)


# Some cross checks between 'Sex' and related fields ####
# of 'Testes', 'Vagina', 'Pubic.bone', 'Reproductive', 'Pregnant', 'Nipples', 'Lactating'.


# Females should not have any data in 'Testes'
ind <- which(traps$Sex == "f" & is.na(traps$Testes) == FALSE)
traps[ind, ]

# there is one entry with conflicting data that can't help with properly assigning 'Sex' or related attributes. 
rowCorrection <- which(traps$Sex == "f" & traps$Testes == "ab" & traps$Vagina == "np" & traps$Pubic.bone == "closed")
traps[rowCorrection, "newCorrections"] <- paste("'Sex', 'Testes', 'Vagina', Pubic.bone', 'Pregnant', 'Nipples', 'Lactating' corrected from ", 
                                                traps[rowCorrection, "Sex"], 
                                                traps[rowCorrection, "Testes"],
                                                traps[rowCorrection, "Vagina"],
                                                traps[rowCorrection, "Pubic.bone"],
                                                traps[rowCorrection, "Pregnant"],
                                                traps[rowCorrection, "Nipples"],
                                                traps[rowCorrection, "Lactating"],
                                                "due to conflicting sex-related data.")
traps[rowCorrection, c("Sex", "Testes", "Vagina", "Pubic.bone", "Pregnant", "Nipples", "Lactating")] <- NA

ind <- which(traps$Sex == "f" & is.na(traps$Testes) == FALSE)
traps[ind, ]

# The remaining entries containing conflicts all contain male-related data. 'Sex' therefore can be changed to male.
rowCorrection <- ind
traps[rowCorrection, "newCorrections"] <- paste0("'Sex' corrected from ", traps[rowCorrection, "Sex"], " based on sex-related data.")
traps[rowCorrection, "Pregnant"] <- NA
traps[rowCorrection, "Lactating"] <- NA
traps[rowCorrection, "Sex"] <- "m"

# Males should not have any data in Vagina', 'Pubic.bone', 'Reproductive', 'Pregnant', 'Nipples', or 'Lactating'.

# Males with 'Vagina' not equal to NA
ind <- which(traps$Sex == "m" & is.na(traps$Vagina) == FALSE)
traps[ind, ] # all of these conflicting entries have "Vagina" recorded as perforated. Therefore able to overwrite 'Sex' to female.

rowCorrection <- ind
traps[rowCorrection, "newCorrections"] <- paste0("'Sex' corrected from ", traps[rowCorrection, "Sex"], " based on value of 'Vagina'.")
traps[rowCorrection, "Sex"] <- "f"


# Males with 'Pubic.bone' not equal to NA
ind <- which(traps$Sex == "m" & is.na(traps$Pubic.bone) == FALSE)
traps[ind, ]

rowCorrection <- ind
traps[rowCorrection, "newCorrections"] <- paste0("'Sex' corrected from ", traps[rowCorrection, "Sex"], " based on sex-related data.")
traps[rowCorrection, "Pubic.bone"] <- NA
traps[rowCorrection, "Sex"] <- "m"

ind <- which(traps$Sex == "m" & is.na(traps$Pubic.bone) == FALSE)
traps[ind, ]


# all seems in order

# Males with 'Reproductive' not equal to NA
ind <- which(traps$Sex == "m" & is.na(traps$Reproductive) == FALSE)
traps[ind, ]
 # all males have other male attricutes and no female ones to reproductive chnaged to NA
rowCorrection <- ind
traps[rowCorrection, "newCorrections"] <- paste0("'Reproductive' corrected from ", traps[rowCorrection, "Reproductive"], " based on sex-related data.")
traps[rowCorrection, "Reproductive"] <- NA

ind <- which(traps$Sex == "m" & is.na(traps$Reproductive) == FALSE)
traps[ind, ]
# all seems in order

# Males with 'Pregnant' not equal to NA
ind <- which(traps$Sex == "m" & is.na(traps$Pregnant) == FALSE)
traps[ind, ]

# Re-evaluate Males with 'Pregnant' not equal to NA

traps[ind, c("Sex", "Testes", "Vagina", "Pubic.bone", "Pregnant", "Nipples", "Lactating", "Corrections")]

# There are no conflicts remaining
# If 'Sex' = m and 'Testes' = scr / ms then we will over-wite 'Nipples' and 'Pregnant' (currently 'N') to NA
#rowCorrection <- which(traps$Sex == "m" & is.na(traps$Pregnant) == FALSE & (traps$Testes == "scr" | traps$Testes == "ms"))
#traps[rowCorrection, "NewCorrections"] <- paste0("'Nipples' corrected from ", traps[rowCorrection, "Nipples"],
                                               #  " and 'Pregnant corrected from ", traps[rowCorrection, "Pregnant"], 
                                                # " based on 'Sex' and 'Testes' values.")
#traps[rowCorrection, "Nipples"] <- NA
#traps[rowCorrection, "Pregnant"] <- NA

# Re-evaluate Males with 'Pregnant' not equal to NA
#ind <- which(traps$Sex == "m" & is.na(traps$Pregnant) == FALSE)
#traps[ind, c("Sex", "Testes", "Vagina", "Pubic.bone", "Pregnant", "Nipples", "Lactating", "Corrections")]

# None-scrotal males with 'Nipples' = 0 can have 'Nipples' corrected to NA.
#rowCorrection <- which(traps$Sex == "m" & is.na(traps$Pregnant) == FALSE & traps$Testes == "ns" & traps$Nipples == "0")
#traps[rowCorrection, "NewCorrections"] <- paste0("'Nipples' corrected from ", traps[rowCorrection, "Nipples"],
                                               #  " based on 'Sex' value.")
#traps[rowCorrection, "Nipples"] <- NA

# None-scrotal males with 'Nipples' > 0 cannot be correctly assigned sex-related data. Remove this sex-realted data.
#rowCorrection <- which(traps$Sex == "m" & is.na(traps$Pregnant) == FALSE & traps$Testes == "ns" & traps$Nipples > "0")
#traps[rowCorrection, "newCorrections"] <- paste0("Sex-related data removed due to unresolvable conflicts:",
                                           #      " 'Sex' = ", traps[rowCorrection, "Sex"],
                                           #      " 'Testes' = ", traps[rowCorrection, "Testes"],
                                           ##      " 'Pregnant' = ", traps[rowCorrection, "Pregnant"],
                                           #      " 'Nipples' = ", traps[rowCorrection, "Nipples"] )

#traps[rowCorrection, c("Sex", "Testes", "Pregnant", "Nipples")] <- NA

#traps[rowCorrection, ]

# Re-evaluate Males with 'Pregnant' not equal to NA
#ind <- which(traps$Sex == "m" & is.na(traps$Pregnant) == FALSE)
#traps[ind, ]

# These remaining values have no conficts in sex-realted data. 'Pregnant' can be corrected to NA.
#rowCorrection <- which(traps$Sex == "m" & is.na(traps$Pregnant) == FALSE)
#traps[rowCorrection, "NewCorrections"] <- paste0("'Pregnant' corrected from ", traps[rowCorrection, "Pregnant"],
                                               #  " based on 'Sex' value.")

#traps[rowCorrection, "Pregnant"] <- NA

#traps <- update.Comments(traps)

# Males with 'Nipples' not equal to NA
#ind <- which(traps$Sex == "m" & is.na(traps$Nipples) == FALSE)
#traps[ind, ]

# No conflicting data and 'Nipples' values of 0 can be corrected to NA
#rowCorrection <- which(traps$Sex == "m" & traps$Nipples == 0)
#traps[rowCorrection, "newCorrections"] <- paste0("'Nipples' corrected from ", traps[rowCorrection, "Nipples"],
                                             #    " based on 'Sex' value.")

#traps[rowCorrection, "Nipples"] <- NA


traps <- update.Comments(traps)

# Males with 'Lactating' not equal to NA
ind <- which(traps$Sex == "m" & is.na(traps$Lactating) == FALSE)
traps[ind, ]

# No conflicting data and 'Lactating' values can be corrected to NA
rowCorrection <- which(traps$Sex == "m" & is.na(traps$Lactating) == FALSE)
traps[rowCorrection, "newCorrections"] <- paste0("'Lactating' corrected from ", traps[rowCorrection, "Lactating"],
                                                 " based on 'Sex' value.")

traps[rowCorrection, "Lactating"] <- NA

traps <- update.Comments(traps)

# AGE ####
unique(traps$Age) # All seems in order

# YR.BIRTH ####
unique(traps$Yr.birth)#All seems in order


# WEIGHT ####
unique(traps$Weight)
# Some weight values entered as 'na'
rowCorrection <- which(traps$Weight == "na" | traps$Weight == "other") # Correction comment not neccessary to include.
traps[rowCorrection, "Weight"] <- NA 

# Convert to numeric vector
traps$Weight <- as.numeric(traps$Weight)

traps <- update.Comments(traps)

# AGE_170 ####
# If a water vole weighs >170 grams, it is assumed to be born the year before and therefore and adult
# This field has not been maintained past the year of 2010
unique(traps$Age_170)
# First check that this applies to water voles only
ind <- which(is.na(traps$Age_170) == FALSE & traps$Species != "wv")
traps[ind, ] 

traps <- update.Comments(traps)

# COLOUR ####
#bl
#br
#bl/br
#NA
unique(traps$Colour)
traps$Colour<-tolower(traps$Colour)
unique(traps$Colour)

rowCorrection <- which(traps$Colour == "blbr"|traps$Colour == "bl br"|traps$Colour == "br/bl")
traps[rowCorrection, "newCorrections"] <- paste0("Colour corrected from ", traps[rowCorrection, "Colour"], ".")
traps[rowCorrection, "Colour"] <- "bl/br"

rowCorrection <- which(traps$Colour == "bl ")
traps[rowCorrection, "newCorrections"] <- paste0("Colour corrected from ", traps[rowCorrection, "Colour"], ".")
traps[rowCorrection, "Colour"] <- "bl"

rowCorrection <- which(traps$Colour == "na")
traps[rowCorrection, "newCorrections"] <- paste0("Colour corrected from ", traps[rowCorrection, "Colour"], ".")
traps[rowCorrection, "Colour"] <- NA
unique(traps$Colour)
traps <- update.Comments(traps)

# WHITE.SPOT ####
#should be 0,1,2,3,4 depending on strength
unique(traps$White.spot)
rowCorrection <- which(traps$White.spot == "na")
traps[rowCorrection, "newCorrections"] <- paste0("White.spot corrected from ", traps[rowCorrection, "White.spot"], ".")
traps[rowCorrection, "White.spot"] <- NA

rowCorrection <- which(traps$White.spot == "N"|traps$White.spot == "n")
traps[rowCorrection, "newCorrections"] <- paste0("White.spot corrected from ", traps[rowCorrection, "White.spot"], ".")
traps[rowCorrection, "White.spot"] <- 0

rowCorrection <- which(traps$White.spot == "Y"|traps$White.spot == "y")
traps[rowCorrection, "newCorrections"] <- paste0("White.spot corrected from ", traps[rowCorrection, "White.spot"], ".")
traps[rowCorrection, "White.spot"] <- 1
unique(traps$White.spot)
traps <- update.Comments(traps)

# WS.POSITION ####
unique(traps$WS.position)# not standardised

# BRUSHED ####
unique(traps$Brushed)
traps$Brushed<-toupper(traps$Brushed)
unique(traps$Brushed)

# MITES ####
#supposed to be 0-10
unique(traps$Mites)
{
rowCorrection <- which(traps$Mites == "na"|traps$Mites == "0.5"|traps$Mites == "wet")
traps[rowCorrection, "newCorrections"] <- paste0("Mites corrected from ", traps[rowCorrection, "Mites"], ".")
traps[rowCorrection, "Mites"] <- NA

rowCorrection <- which(traps$Mites == "12"|traps$Mites == "11"|traps$Mites == "14")
traps[rowCorrection, "newCorrections"] <- paste0("Mites corrected from ", traps[rowCorrection, "Mites"], ".")
traps[rowCorrection, "Mites"] <- 1

rowCorrection <- which(traps$Mites == "50")
traps[rowCorrection, "newCorrections"] <- paste0("Mites corrected from ", traps[rowCorrection, "Mites"], ".")
traps[rowCorrection, "Mites"] <- 5

rowCorrection <- which(traps$Mites == "100")
traps[rowCorrection, "newCorrections"] <- paste0("Mites corrected from ", traps[rowCorrection, "Mites"], ".")
traps[rowCorrection, "Mites"] <- 10

rowCorrection <- which(traps$Mites == "20")
traps[rowCorrection, "newCorrections"] <- paste0("Mites corrected from ", traps[rowCorrection, "Mites"], ".")
traps[rowCorrection, "Mites"] <- 2

rowCorrection <- which(traps$Mites == "30")
traps[rowCorrection, "newCorrections"] <- paste0("Mites corrected from ", traps[rowCorrection, "Mites"], ".")
traps[rowCorrection, "Mites"] <- 3

rowCorrection <- which(traps$Mites == "40")
traps[rowCorrection, "newCorrections"] <- paste0("Mites corrected from ", traps[rowCorrection, "Mites"], ".")
traps[rowCorrection, "Mites"] <- 4

rowCorrection <- which(traps$Mites == "70")
traps[rowCorrection, "newCorrections"] <- paste0("Mites corrected from ", traps[rowCorrection, "Mites"], ".")
traps[rowCorrection, "Mites"] <- 7
}
unique(traps$Mites)
traps <- update.Comments(traps)

# TICKS ####
unique(traps$Ticks)
traps$Ticks <- gsub(pattern = "\\+", replacement = "", x = traps$Ticks)
traps$Ticks <- gsub(pattern = "na", replacement = NA, x = traps$Ticks)
traps$Ticks<-as.numeric(traps$Ticks)
unique(traps$Ticks)

# FLEAS ####
unique(traps$Fleas)
rowCorrection <- which(traps$Fleas == ">20")
traps[rowCorrection, "newCorrections"] <- paste0("Fleas corrected from ", traps[rowCorrection, "Fleas"], ".")
traps[rowCorrection, "Fleas"] <- 20

rowCorrection <- which(traps$Fleas == "na")
traps[rowCorrection, "newCorrections"] <- paste0("Fleas corrected from ", traps[rowCorrection, "Fleas"], ".")
traps[rowCorrection, "Fleas"] <- NA
unique(traps$Fleas)
traps <- update.Comments(traps)

# FUR.LOSS ####
unique(traps$Fur.loss)
rowCorrection <- which(traps$Fur.loss == "na")
traps[rowCorrection, "newCorrections"] <- paste0("Fur.loss corrected from ", traps[rowCorrection, "Fur.loss"], ".")
traps[rowCorrection, "Fur.loss"] <- NA

rowCorrection <- which(traps$Fur.loss == ">10")
traps[rowCorrection, "newCorrections"] <- paste0("Fur.loss corrected from ", traps[rowCorrection, "Fur.loss"], ".")
traps[rowCorrection, "Fur.loss"] <- 10

rowCorrection <- which(traps$Fur.loss == "N")
traps[rowCorrection, "newCorrections"] <- paste0("Fur.loss corrected from ", traps[rowCorrection, "Fur.loss"], ".")
traps[rowCorrection, "Fur.loss"] <- 0

rowCorrection <- which(traps$Fur.loss == "Y")
traps[rowCorrection, "newCorrections"] <- paste0("Fur.loss corrected from ", traps[rowCorrection, "Fur.loss"], ".")
traps[rowCorrection, "Fur.loss"] <- NA

unique(traps$Fur.loss)
traps <- update.Comments(traps)

# FAECES ####
#refers to if it was collecetd 
#binary :Y N
unique(traps$Faeces)
traps$Faeces<-toupper(traps$Faeces)

rowCorrection <- which(traps$Faeces == "YES"|traps$Faeces == "1"|traps$Faeces == "2"|traps$Faeces == "0.05")
traps[rowCorrection, "newCorrections"] <- paste0("Faeces corrected from ", traps[rowCorrection, "Faeces"], ".")
traps[rowCorrection, "Faeces"] <- "Y"

rowCorrection <- which(traps$Faeces == "YES"|traps$Faeces == "1"|traps$Faeces == "2"|traps$Faeces == "0.5")
traps[rowCorrection, "newCorrections"] <- paste0("Faeces corrected from ", traps[rowCorrection, "Faeces"], ".")
traps[rowCorrection, "Faeces"] <- "Y"

rowCorrection <- which(traps$Faeces == "NO"|traps$Faeces == "0"|traps$Faeces == "0?")
traps[rowCorrection, "newCorrections"] <- paste0("Faeces corrected from ", traps[rowCorrection, "Faeces"], ".")
traps[rowCorrection, "Faeces"] <- "N"

traps <- update.Comments(traps)

# BLOOD ####
#binary 1 and 0
unique(traps$Blood)
rowCorrection <- which(traps$Blood == "no"|traps$Blood == "N"|traps$Blood == "n")
traps[rowCorrection, "newCorrections"] <- paste0("Blood corrected from ", traps[rowCorrection, "Blood"], ".")
traps[rowCorrection, "Blood"] <- 0

rowCorrection <- which(traps$Blood == "na"|traps$Blood == "Na")
traps[rowCorrection, "newCorrections"] <- paste0("Blood corrected from ", traps[rowCorrection, "Blood"], ".")
traps[rowCorrection, "Blood"] <- NA
traps <- update.Comments(traps)
traps$Blood<-as.factor(traps$Blood)
unique(traps$Blood)

# UNIQUE.ID ####
unique(traps$Unique.ID)

# POSSIBLE.NR ####
unique(traps$Possible.nr)#looks okay
# NO.INITIAL.CAPT ####
unique(traps$No.initial.capt)#looks okay

# COMMENTS ####
unique(traps$Comments)#looks fine

# DOCILITY ####
unique(traps$Docility)

# Duplicate 'Docility' as a temporary vector. Clear Docility and re-insert only numeric entries. Remaining entries saved in a new 'Fur.clip.2' vector.
traps$Docility.Temp <- traps$Docility # Duplicate Docility to temporary vector.
traps$Docility <- NA # Clear 'Docility'
unique(traps$Docility.Temp)

ind <- grep(pattern = "^[0-9]+$", traps$Docility.Temp) # identify rows that contain only numbers for 'Docility'
traps$Docility[ind] <- traps$Docility.Temp[ind]
unique(traps$Docility)


# Remaining entries can be stored in a new vector called 'Fur.clip.2'
traps$Comment2 <- NA
traps$Comment2[-ind] <- traps$Docility.Temp[-ind]
unique(traps$Comment2)

unique(traps$Comments)
traps$Comments<-ifelse(is.na(traps$Comments),traps$Comment2,traps$Comments)
traps$Docility.Temp<-NULL
traps$Comment2<-NULL
traps$Docility<-as.numeric(traps$Docility)
unique(traps$Docility)

# ISSUE ####
unique(traps$Issue)#looks fine

# ISSUE NOTE ####
unique(traps$Issue.Note)#looks fine 
#### Duplicated rows
# by visual inspection some rows are duplicated (esp. dr in 2015) 
#as some have certain columns missing i will compare them using Vial, Flag, Mpatch, Check < species, date and Unique ID
#any duplicated rows based on all these columns are considered true duplicates and can be removed

traps$sample.id<-NA
traps$sample.id<-paste0(traps$Vial,sep="_",traps$Year)
unique(traps$sample.id)
dupl<-traps[duplicated(traps[,c("sample.id","Mpatch","Flag","Check","Species","Date","Unique.ID","Capt")]),]
traps.no.dpl<-traps[!duplicated(traps[traps$Year==2015,c("sample.id","Mpatch","Flag","Check","Species","Date","Unique.ID","Capt")]),]

traps$sample.id<-NULL
# CLEAR OUT REDUNDANT COLUMNS ####

# convert Date column to text format to try and preserve structure when opening in other applications
traps$Date <- as.character(traps$Date)

str(traps)
traps <- update.Comments(traps)


redundant <- c("Area",
               "Date2",
               "Gene.samp",
               "Reproductive",
               "newCorrections",
               "newComment") 

trapsClean <- traps[ ,!(names(traps) %in% redundant)]
str(trapsClean)

summary(traps$Network)
unique(traps$Fur_clip)
# WRITE NEW FILES


# 2001 to 2003 have data that does not appear in other years:
#   Gene.samp
#   Reproductive
write.xlsx(Assynt_Trapping_GeneSamp_2001_2003,"Assynt_Trapping_GeneSamp_2001_2003.xlsx")
write.xlsx(Assynt_Trapping_Reproductive_2001_2003,"Assynt_Trapping_Reproductive_2001_2003.xlsx")


write.xlsx(trapsClean,"Assynt_master_traps_corrected20200923.xlsx")
