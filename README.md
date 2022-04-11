# pipo_thesis

setwd("G:/My Drive/2020_Cores_Gonzalez/Scripts and data")

## working library
library(tidyverse)
library(readxl)
library(dplyr)
#library(adehabitat)
#library(raster)
#library(rgdal)
#library(foreign)
library(data.table)


## field data in xlsx. Reading in excel and choosing first sheet
fielddata <- read_excel("FieldData_2020.xlsx", sheet= "Field Data", col_names = TRUE)
fielddata <- subset(fielddata, select= -c(Notes))
cwd <- read.csv("site_locations.csv")
ringwidths12 <-read.csv("ringwidths_12mm.csv", header=T)
ringwidths5 <- read.csv("ringwidths_5mm.csv", header=T)
conescars <- read.csv("conescars_new.csv", header=T)
conescars <- subset(conescars, select=c("site", "tree", "year", "cones"))

### reformatting ring width data frame to mimic cone scars
### using melt function to remove everything but Year. 
ringwidthsmelt12 <- melt(ringwidths12, id.vars=c("Year"))

### separating site and tree is giving me an error.... deleting rows? NAS? 
annualgrowth12 <- ringwidthsmelt12 %>% separate(variable, 
                c("site", "tree"))
annualgrowth12 <- annualgrowth12 %>% rename(year=Year, growth12=value) ## matching other dataset
annualgrowth12 <- annualgrowth12 %>% subset(year<=2020 & year>=2000)
#### doing this again with 5mm growth info to add to master
ringwidthsmelt5 <- melt(ringwidths5, id.vars=c("Year"))
annualgrowth5 <- ringwidthsmelt5 %>% separate(variable,c("site", "tree"))
annualgrowth5 <- annualgrowth5 %>% rename(year=Year, growth5=value) ## matching other dataset
annualgrowth5 <- annualgrowth5 %>% subset(year<=2020 & year>=2000)


timeseries <- merge(annualgrowth12,conescars,by=c("year","tree","site"))
timeseries <- merge(timeseries, annualgrowth5, by=c("year","tree","site"))
all_data <- merge(timeseries, fielddata, by=c("tree", "site"), all.x=TRUE)
all_data <- merge(all_data, cwd, by=c("site"))

#### creating column with cone abundance per tree per year

all_data$conematur <- cbind(all_data$cones*all_data$Leadershoots)

#### merging and compiling all resin scripts. Script is redundant.
##I generally work with a single line and change file name. Much faster.
### changing directory to merge files into master directory

setwd("G:/My Drive/2020_Cores_Gonzalez/BD/BD_resin")

## pulling all length dataset to compile into master
BD168 <- read.csv("BD_168_length.csv")

## only two can merge at a time
BD_length <- merge(BD159, BD160,by=c("Label","Length","Year", "Scar"),all.x=TRUE, all.y=TRUE)

BD_length <- merge(BD_length, BD168,by=c("Label","Length","Year", "Scar"),all.x=TRUE, all.y=TRUE)

### dropping unneeded columns
BD_length <- subset(BD_length, select= c("Label","Length","Year", "Scar")) ## only needed columns

### RESULTS/duct info ###########################################

BD168 <- read.csv("BD_168_Results.csv")

BD_ducts <- merge(BD159, BD160,by=c("Label","Year","Area"),all.x=TRUE, all.y=TRUE)

BD_ducts <- merge(BD_ducts, BD168,by=c("Label","Year","Area"),all.x=TRUE, all.y=TRUE)
BD_ducts <- subset(BD_ducts, select= c("Label","Area","Year"))

## checking 0s, NAs, and other large oddities. Verifying i have all data points

#HM1_ducts <- na.omit(HM1_ducts)
#BH_ducts <- subset(BH_ducts, BH_ducts$Year !="0")

### wrote these as separate csvs for extras. Not necessary
write.csv(BD_ducts, file="BD_ducts.csv")
write.csv(BD_length, file="BD_length.csv")

### now to compile all of it into single master of length and ducts

master_length <- merge(FC_length,BH_length,by=c("Label","Year","Length","Scar"), all.x=TRUE, all.y=TRUE)
master_length <- merge(master_length,BD_length, by=c("site", "tree", "Year","Length","Scar"),all.x=TRUE, all.y=TRUE)
#in case there are extra columns: master_length <- subset(master_length, select = c("Label","Year","Scar","Length"))

BD_length <- BD_length %>% separate(Label, 
                                            c("site","tree"))
#master_length <- master_length %>% na.omit()
write.csv(master_length, "master_length.csv")

master_ducts <- merge(BH_ducts, FC_ducts, by=c("site", "tree", "Year","Area"), all.x=TRUE, all.y = TRUE)
master_ducts <- merge(master_ducts, BD_ducts,by=c("site", "tree", "Year","Area"), all.x=TRUE, all.y = TRUE)
#master_ducts <- subset(master_ducts, select = c("Label","Year","Area"))
BD_ducts<-BD_ducts%>%separate(Label,
                                      c("site","tree"))
#master_ducts <- master_ducts%>% na.omit()
## no na's but used it to double check.
write.csv(master_ducts, "master_ducts.csv")


######################
# adding additional stuff to csv
master_length <- read.csv("master_length.csv")
master_ducts <- read.csv("master_ducts.csv")

### creating two metrics for resin ducts
### none of these metrics account for ring area. Length will not be used yet
### 1st is the unstandardized total resin area. sum area per annual ring

totalresinarea <- master_ducts %>% group_by(tree, Year, site) %>% 
  summarise(totalresinarea =sum(Area))

totalresinarea <- totalresinarea %>% rename(year=Year)

all_data <- merge(all_data, totalresinarea, by=c('tree', 'year', 'site'))

## 2nd is mean Resin duct size (mm2 of cross sectional area): mean size of all ducts per annual ring

meanductsize <- master_ducts %>% group_by(tree, Year, site) %>% summarise(meanductsize = mean(Area))
meanductsize <- meanductsize %>% rename(year=Year)

all_data <- merge(all_data, meanductsize, by=c('tree', 'year', 'site'))

### creating standardized relative resin area metric
# did not add this in the end, but keeping in case i change my mind
master_length<- rename(master_length, year=Year)
all_data <- merge(all_data, master_length ,by=c('tree', 'year', 'site') )
all_data <- all_data %>% subset(select=-c(X.x, X.y))
all_data$ringarea <- (all_data$growth12*all_data$Length)-all_data$Scar
all_data$relativeductarea <- (all_data$totalresinarea/(all_data$ringarea))*100

### removing unnecessary columns. renaming columns. 

all_data <- all_data %>% subset(select=-c(PlotID, slope.y, aspect.y))
all_data <- rename(all_data, slope=slope.x, aspect=aspect.x) 
all_data <- rename(all_data, leadershoots=Leadershoots)

### verifying I have all my data points after merge

all_data$site <-as.factor(all_data$site)
levels(all_data$site)
all_data$tree <- as.factor(all_data$tree)
levels(all_data$tree)
list(all_data$year)

### writing CSV with all data needed. I put this on top instead of the bottom of the script


write.csv(all_data, "allfield_data.csv")


### adding in climate sum 4k data

climate_all <-read.csv('climate_monthly_4k.csv')

## dropping temp columns
drops<-c("tmax","tmin","tmean")
climate<-climate_all[,!(names(climate_all) %in% drops)]

## creating water year by seperating and summing water year months

## decided not to use wateryear for analysis. Moved to FDSI
#octdec <- climate %>% group_by(Year, Site) %>% filter(Month >= 10)
#octdec_sum <- octdec %>% summarise(sum(ppt))
#octdec_sum$Year <- octdec_sum$Year+1
#jansep <- climate %>% group_by(Year, Site) %>% filter(Month < 10)
#jansep_sum <- jansep %>% summarise(sum(ppt))
#jansep_sum

#jansep_sum$ppt <- jansep_sum$`sum(ppt)`
#octdec_sum$ppt <- octdec_sum$`sum(ppt)`

#jansep_sum <- merge(jansep_sum, octdec_sum, by=c("Year", "Site"))

#wateryear <- jansep_sum %>% subset(select=c("Year", "Site", "ppt.x", "ppt.y"))

#wateryear$wateryear <- wateryear$ppt.x+wateryear$ppt.y

#wateryear <- wateryear %>% subset(select=c("Year", "Site", "wateryear"))
#wateryear <- wateryear %>% rename(year=Year, site=Site)


#all_data <- merge(all_data, wateryear, by=c("year", "site"))

#all_data <- all_data %>% subset(select=-c(X))

#write.csv(all_data, "allfield_data.csv")


### PULLING WUE DATA TO ADD TO ALL_DATA FILE
wueavg <- read_excel("PIPO_WUE_20.xlsx", sheet="Samples", col_names = TRUE ) 
all_data <- read.csv("allfield_data.csv")

wueavg <- rename(wueavg, id=SampleID)
wueavg <- wueavg %>% subset(select=c(id, d13CVPDB, TotalC))
wueavg <- wueavg %>% separate(id,c("site", "tree"))
wueavg$site <- tolower(wueavg$site)
wueavg <- as.data.frame(wueavg)
all_data <- merge(all_data, wueavg, by=c("site", "tree"))

write.csv(all_data, "allfield_data.csv")
all_data$d13CVPDB <- all_data$d13CVPDB
all_data$dAtmo <- -8

all_data$wue <- (all_data$dAtmo-all_data$d13CVPDB)/(1+all_data$d13CVPDB/1000)

## agreed with new equation based on book Bill provided. Will move 
#foward after reading more. 

all_data <- all_data %>% subset(select=-c(TotalC, d13CVPDB, dAtmo))
all_data$site <- tolower(all_data$site)

### basal area calc:

all_data$BA20 <- all_data$BA20*20
all_data$BA10 <- all_data$BA10*10
all_data$BA5 <- all_data$BA5*5

#
write.csv(all_data, "allfield_data.csv")

                

tree_avgs <- read.csv("all_averages.csv")

avgs_data <- as.data.table(avgs_data)
all_data <- as.data.table(all_data)

newdf <- all_data %>% group_by(tree, site) %>% summarise(wue=mean(wue))

tree_avgs <- merge(tree_avgs, newdf, by=c("site", "tree"))

aet <- all_data %>% group_by(site) %>% summarise(aet=mean(aet))
tree_avgs <- merge(tree_avgs, aet, by=c("site"))

remove(aet)
write.csv(tree_avgs, "all_averages.csv")

wtryravg <- all_data %>% group_by(site) %>% summarise(wateryear=mean(wateryear))
head(wtryravg)

remove(wtryravg)
tree_avgs <- merge(tree_avgs, wtryravg, by=c("site"))
site_avgs <- all_data %>% group_by(site) %>% summarise(wue=mean(wue), growth=mean(growth12), slope=mean(slope), aspect=mean(aspect), cwd=mean(cwd), elev=mean(elev), coneabundance=mean(coneabundance), totalresinarea=mean(totalresinarea), meanductsize=mean(meanductsize), C13=mean(C13), aet=mean(aet), lat=mean(lat))
write.csv(site_avgs, "site_avgs.csv")
tree_avgs$wue <- tree_avgs$wue*1000

tree_avgs <- tree_avgs %>% subset(select=-c(X.1, X))


all_data <- read.csv("allfield_data.csv")


#### FDSI CALCULATIONS ####

### ADG working directory
setwd("G:/My Drive/2020_Cores_Gonzalez/Scripts and data")

# LOAD Packages

library(adehabitat)
#library(raster)
library(rgdal)
library(foreign)
library(data.table)

### pulling in compiled climate data. All sites included in csv. 

climate_all <-read.csv('Climate_PRISM.csv')

head(climate_all)

## dropping temp columns
drops<-c("tmax","tmin","tmean", "vpdmax", "vpdmin")
climate_all<-climate_all[,!(names(climate_all) %in% drops)]

#### STACK ALL CLIMATE DATA

climate_stacked<-melt(climate_all, id.vars=c("year","site","month"))
## melt function note: melt stacks the other variables not listed in order to streamline coding applications

head(climate_stacked)
### stacked has twice as many rows because it melted and stacked the ppt and vpd variables
levels(climate_stacked$variable)
#combine climate variable and month together
climate_stacked$monthvar<-paste(climate_stacked$variable,climate_stacked$month,sep="")
head(climate_stacked)
### doing this because we want to look at cone initiation
climate_minus1<-climate_stacked
climate_minus1$year<-climate_minus1$year+1

## clarifying yr1 to avoid confusion later on. 
climate_minus1$monthvar<-paste("yr1",climate_minus1$monthvar,sep="")
### the data collected is already subset so i dont believe the code below is necessary
#climate_minus1<-subset(climate_minus1,year>1998)

## technically year 2000 is 1999. now it says 2000. year1 reminds me that its this way
## +2? Not sure why. Think that one threw. Maturation?

### fdsi calculation won't work for some reason w/out three dfs when i dcast. 
#keep for that reason
climate_minus2<-climate_stacked
climate_minus2$year<-climate_stacked$year+2
climate_minus2$monthvar<-paste("yr2",climate_minus2$monthvar,sep="")
#climate_minus2<-subset(climate_minus2,year>1999)
head(climate_minus2)

climate_stacked_allyrs<-rbind(climate_stacked,climate_minus1,climate_minus2)
#climate_stacked_allyrs<-subset(climate_stacked_allyrs,year<2017 & year>2000)
head(climate_stacked_allyrs)

### FDSI CALC
#read it
#DT it
### site and year, i want to stay long ways. I want monthvar to be columns.
### i want value to be the actual value beneath the columns

FDSI_C <- dcast(climate_stacked_allyrs,site+year~monthvar,measure.var=c("value"))

head(FDSI_C)
View(FDSI_C)


#calc summer VPD and winter PPT, fertilization and initiation VPD
#S_VPD:VPD during the summer of cone initiation (yr-1)
#w_PPT:PPT during the winter preceding cone initiation (yr-1)
#W_PPT.IF:PPT during the winter between initiation and fertilization
#F_VPD:VPD during year of fertilization (yr0)
#I_VPD:VPD during year of initiation (yr-1)
#MON: July and Aug PPT during year of cone initation (yr-1)
#logPPT: log scale PPT for FDSI calc

## ALL OF THIS IS FROM THE WILLIAMS PAPER ABOUT FDSI. YOU MOST LIKELY WONT USE THIS.

FDSI_C<-as.data.table(FDSI_C)
# pulling yr1 previous winter/fall precip and vpd data to calculate current year vpd and precip
## vpd and ppt w/out yr1 is current year. yr1=previous year

# YOU NEED 3 THINGS TO CALCULATE FDSI. RE-READ PAPER TO UNDERSTAND WHY SHE CHOSE THESE THINGS
FDSI_raw<-FDSI_C[,.(FDSIvpd=((((vpdmean5+vpdmean6+vpdmean7)/3)+(yr1vpdmean8+yr1vpdmean9+yr1vpdmean10)/3)/2), 
                 FDSIppt=log(yr1ppt11+yr1ppt12+ppt1+ppt2+ppt3)), by=c("site","year")]

head(FDSI_raw)
view(FDSI_raw)

## Create stacked climate data with all monthly variables as z scores
climatetable<- FDSI_raw
climatetable <- as.data.table(climatetable) #needs to be a DT
head(climatetable)
climatetable <- na.omit(climatetable)
meanVPD <- mean(climatetable$FDSIvpd)
meanppt <- mean(climatetable$FDSIppt)
SDVPD <- sd(climatetable$FDSIvpd)
SDppt <- sd(climatetable$FDSIppt)
SDppt
FDSI_raw$zscoreppt <- ((FDSI_raw$FDSIppt-meanppt)/SDppt)
FDSI_raw$zscoreVPD <- ((FDSI_raw$FDSIvpd-meanVPD)/SDVPD)

FDSI_raw$FDSI <- (0.44*(FDSI_raw$zscoreppt)-0.56*(FDSI_raw$zscoreVPD))

view(FDSI_raw)

FDSI_raw <- FDSI_raw %>% subset(year<=2020 & year>=2000)
FDSI_Final <- FDSI_raw %>% subset(select=c("site", "year", "FDSI"))
FDSI_Final$site <- tolower(FDSI_Final$site)

all_data <- merge(all_data, FDSI_Final, by=c("site", "year"))

write.csv(all_data, "allfield_data.csv")

### FDSI calc looks good. Check w/ Miranda to ensure it looks OK in R. 


### Subject centering WUE data

wuecentered1 <- tree_avgs %>% group_by(site) %>% summarise(wuesite=mean(wue))

tree_avgs <- merge(wuecentered1, tree_avgs, by=c("site"))

tree_avgs$wuecentered <- (tree_avgs$wue-tree_avgs$wuesite)

tree_avgs <- tree_avgs %>% subset(select=-c(wuesite))

wuecentered2 <- tree_avgs %>% subset(select=c("site", "tree", "wuecentered"))

all_data <- merge(all_data, wuecentered2, by=c("site", "tree"))

all_data <- all_data %>% subset(select=-c(X.2,X, X.1, X.x, X.y))

##### CWD CALCULATIONS #############

library(raster)
library(foreign)
library(reshape2)
library(ggplot2)

##read in PRISM data for all sites
site_climate <- read.csv("G:/My Drive/2020_Cores_Gonzalez/Scripts and data/30yrnorm_sites.csv")
site_climate$month <- recode(site_climate$month, January = 1, February=2, March=3, April=4, May=5, June=6, July=7, August=8, September=9, October=10, November=11, December=12)
site_locations <- read.csv("G:/My Drive/2020_Cores_Gonzalez/Scripts and data/site_locations.csv")
site_locations <- merge(site_locations, site_climate, by=c("site"))


### open Miranda's function, RUN function. 
#Then run the function using her example code.

cwd_normal <- cwd_function(site=site_locations$site,slope=site_locations$slope,latitude=site_locations$lat,foldedaspect=site_locations$aspect,ppt=site_locations$ppt,tmean=site_locations$tmean,month=site_locations$month, soilawc=site_locations$awc..mm.,type="normal")

cwd_final <- cwd_normal %>% group_by(cwd_normal$site) %>% summarise(cwd=sum(cwd))
site_locations <- read.csv("G:/My Drive/2020_Cores_Gonzalez/Scripts and data/site_locations.csv")

cwd_final <- cwd_final %>% rename(site="cwd_normal$site")

site_locations <- merge(site_locations, cwd_final, by=c("site"))

write.csv(site_locations, file="site_locations.csv")

site_locations <- site_locations %>% subset(select=c("site", "cwd"))
site_locations$site <- tolower(site_locations$site)

all_data <- merge(all_data, site_locations, by=c("site"))
tree_avgs <- merge(tree_avgs, site_locations, by=c("site"))

write.csv(all_data, file="allfield_data.csv")
write.csv(tree_avgs, file="all_averages.csv")
