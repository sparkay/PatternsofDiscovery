### functions for processing patent files ###
### got tired of selecting segments of individual scripts ###
### KSF April 20, 2012 ###

###################################################################
### reads and cleans the TechMatch file and returns a dataframe ###

### Original shortening, no NVESD ###
shortenLab <- function(Labnamevec) {
  #shortening long names
  returnvec <- 
    ifelse(Labnamevec == "Army Armament Research, Development & Engineering Center", "ARDEC",
           ifelse(Labnamevec =="U.S. Army Armament Research, Development and Engineering Center", "ARDEC",
                  ifelse(Labnamevec =="Army Aviation & Missile Research, Development & Engineering Center", "AMRDEC",
                         ifelse(Labnamevec =="Night Vision & Electronic Sensors Directorate", "CERDEC",
                                ifelse(Labnamevec =="Tank-Automotive Research, Development and Engineering Center", "TARDEC",
                                       ifelse(Labnamevec =="RDEC Edgewood Chemical Biological Center", "ECBC",
                                              ifelse(grepl("^ERDC ",Labnamevec), "ERDC",
                                                     ifelse(Labnamevec =="Directed Energy Directorate", "AFRL RD",
                                                            ifelse(Labnamevec =="Air Vehicles Directorate", "AFRL RB",
                                                                   ifelse(Labnamevec =="Propulsion Directorate", "AFRL RZ",
                                                                          ifelse(Labnamevec =="AFRL - Munitions Directorate", "AFRL RW",
                                                                                 ifelse(Labnamevec =="Human Effectiveness Directorate", "AFRL 711th",
                                                                                        ifelse(Labnamevec =="Space Vehicles Directorate", "AFRL RV", 
                                                                                               ifelse(Labnamevec =="Sensors Directorate", "AFRL RY", 
                                                                                                      ifelse(Labnamevec =="Materials & Manufacturing Directorate", "AFRL RX",
                                                                                                             ifelse(Labnamevec =="Information Directorate", "AFRL RI", 
                                                                                                                    ifelse(Labnamevec =="Army CERDEC - Fort Monmouth", "CERDEC",
                                                                                                                           ifelse(Labnamevec =="Army ERDC", "ERDC",
                                                                                                                                  ifelse(Labnamevec =="Natick Soldier Systems Center", "NSRDEC", 
                                                                                                                                         ifelse(grepl("Army Research Laboratory", Labnamevec), "ARL", 
                                                                                                                                                ifelse(grepl("Army Medical Research and Materiel Command", Labnamevec), "USAMRMC",  ifelse(Labnamevec =="Naval Research Laboratory", "NRL", as.character(Labnamevec)))))))))))))))))))))))
  return(returnvec)
}

#### Second shortening adds NVESD ####
shortenLab2 <- function(Lablong, Labshort) {
  returnvec <- ifelse(Lablong =="Night Vision & Electronic Sensors Directorate", "NVESD",as.character(Labshort))
  return(returnvec)
}

#### label services based on short lab ####
labelservice <- function(Labnamevec) {
  returnvec <- ifelse(grepl("AFRL|AFOSR|Air Force|311th|Electronics System Center",Labnamevec), "Air Force", ifelse(grepl("NAV|Naval|NRL|SPAWAR|Atlantic|Strategic Systems",Labnamevec),"Navy","Army"))
  return(returnvec)
}


### MAIN ####
procTechMatch <- function(filename) {
  Patents.by.Lab <- read.delim(filename)
  
  #drop the silly X column
  Patents.by.Lab <- subset(Patents.by.Lab, select=-c(X))
  
  #rename Patents.. into pat_num
  names(Patents.by.Lab)[names(Patents.by.Lab) == "Patent.."] <- "pat_num"
  
  #clean pat nums
  Patents.by.Lab$pat_num <- tolower(gsub(" ","",Patents.by.Lab$pat_num))
  
  #fix short patent numbers
  Patents.by.Lab$pat_num <- paste(substr(Patents.by.Lab$pat_num,1,1),gsub(" ","0",sprintf("%06s",substr(Patents.by.Lab$pat_num,2,nchar(Patents.by.Lab$pat_num)))),sep="")
  
  #remove duplicate entries  
  Patents.by.Lab <- ddply(Patents.by.Lab, .(pat_num,Patent.Title, Lab, Issue.Date), 'nrow')
  #rename 'nrow' to something more appropriate
  names(Patents.by.Lab)[names(Patents.by.Lab) == "nrow"] <- 'nentries'

  #get shorter lab names
  Patents.by.Lab$Lab.Short <- shortenLab(Patents.by.Lab$Lab)
  Patents.by.Lab$lab.short2 <- shortenLab2(Patents.by.Lab$Lab, Patents.by.Lab$Lab.Short)
  Patents.by.Lab$service <- labelservice(Patents.by.Lab$lab.short2)
  
  #only show each year not by month or day
  #library('lubridate')
  Patents.by.Lab$Issue.Year <- (year(mdy(Patents.by.Lab$Issue.Date)))
  
  return(Patents.by.Lab)
}

###################################################################


########################################################
### MERGE A STATA FILE WITH A TECHMATCH DF AND CLEAN ###
## More general version
prepStata <- function(statadf, techmatchdf) {
  #assumes original stata names, techmatch names per above
  ##R takes too long to condense the sub classes, better to let STATA condense and load the file you want
  ####merge keeping matches with TechMatch only
  ## first, clean patent number for both dfs
  statadf$patentnum <- tolower(gsub(" ","",statadf$patentnum))
  techmatchdf$pat_num <- tolower(gsub(" ","",techmatchdf$pat_num))
  ## second, merge
  PatswClass <- merge(techmatchdf, statadf, by.x="pat_num", by.y="patentnum", all.x=TRUE)
  #clean up column names
  names(PatswClass)[names(PatswClass) == "class_primary"] <- "class"
  names(PatswClass)[names(PatswClass) == "appyear"] <- "app_year"
  
  ## finally, double check that we only have one entry per patent number per class
  tmp <- ddply(PatswClass,.(pat_num,class,app_year,lab.short2),'nrow')
  print("patents with more than one entry per class:")
  print(tmp[which(tmp$nrow > 1), c('pat_num','class','app_year','lab.short2','nrow')])
  
  return(PatswClass)
}
## Looking at primary classes only
prepStata.primary <- function(statadf, techmatchdf) {
  #assumes original stata names, techmatch names per above
  #slim down to primary only first cause it's faster
  pclasses <- ddply(statadf,.(patentnum, class_primary), summarise, subcnt = length(class_sub))
  ##double check length of results to make sure we don't get duplicates in the merge
  print(paste("number of entries after removing sub class:", nrow(pclasses), sep=" "))
  #add the demographic info back in
  pclasses.demo <- merge(pclasses, subset(statadf, select=c(patentnum, appyear, grantyear)))
  ##double check length of results to make sure we don't get duplicates in the merge
  print(paste("number of entries after merging demo info:", nrow(pclasses.demo), sep=" "))
  ####merge keeping matches with TechMatch only
  ## first, clean patent number for both dfs
  pclasses.demo$patentnum <- tolower(gsub(" ","",pclasses.demo$patentnum))
  techmatchdf$pat_num <- tolower(gsub(" ","",techmatchdf$pat_num))
  ## second, merge
  PatswClass <- merge(techmatchdf, pclasses.demo, by.x="pat_num", by.y="patentnum", all.x=TRUE)
  #clean up column names
  names(PatswClass)[names(PatswClass) == "class_primary"] <- "class"
  names(PatswClass)[names(PatswClass) == "appyear"] <- "app_year"
  
  ## finally, double check that we only have one entry per patent number per class
  tmp <- ddply(PatswClass,.(pat_num,class),'nrow')
  print("patents with more than one entry per class:")
  print(tmp[which(tmp$nrow > 1),c('pat_num','class','app_year','lab.short2')])
  
  return(PatswClass)
}
  
########################################################

###############################################
## COUNT UP FRACTION OF PATENTS EARLY BY LAB ##
getregioncounts <- function(patsdf, ilab="lab.short2", iregion="region", iclass="class") {
  #rename the input df to make our life easier
  tmpsub <- data.frame(lab = patsdf[,ilab], region = patsdf[,iregion], class = patsdf[,iclass])

  #count up number of pats in each region
  classct <- count(tmpsub,c("lab","region","class"))
  names(classct)[length(names(classct))] <- "count"
  #get total patents per lab per class
  classtotal <- count(tmpsub,c("lab","class"))
  names(classtotal)[length(names(classtotal))] <- "total"
  classgreaterthan <- merge(classct,classtotal)
  #this should give me the % of patents in each region for each lab
  classgreaterthan$freq <- classgreaterthan$count/classgreaterthan$total
  ##need to add the mean as well
  classgreaterthan <- ddply(classgreaterthan,.(class,region),transform,clsmean=mean(freq))
  #add reattach the service
  classgreaterthan$service <- labelservice(classgreaterthan$lab)
  return(classgreaterthan)  
}


####################################################
## ADD CLASS TITLE AND GROUP INFO TO A DATA FRAME ##
## subject thing isn't really working, need to account for multiples ##
addclasstxt <- function(mydf) {
  #listing participating patent classes by names
  primary_classes_index <- read.delim("~/Patents/primary_classes_index2.txt", stringsAsFactors=F)
  #add class groups
  patent.class.groups <- read.delim("~/Patents/patent class groups.txt", stringsAsFactors=F)
  classinfo <- merge(primary_classes_index, patent.class.groups, by.x="class", by.y="class", all.x=TRUE)

  #reconcile class number format
  classinfo$tmp <- gsub(" ", "0", sprintf("%03s", as.character(classinfo$class)))
  names(classinfo)[names(classinfo) == "title"] <- "class.title"
  
  #shorten some titles
  classinfo$class.title <- gsub(" -- PART OF THE CLASS.+ SERIES","",classinfo$class.title)

  #merge class titles and group names on to the input df
  outdf <- merge(mydf,subset(classinfo,select=-class), by.x="class", by.y="tmp", all.x=TRUE)
  print(names(outdf))
  
  ###now turn everything into factors ordered by Kgroup similarity value
  Kgroupcolors <- read.delim("~/Patents/Kgroup colors.txt",sep="\t",header=T, stringsAsFactors=F)
  classinfo <- merge(classinfo,Kgroupcolors[,c('Kgroup','Index')], by="Kgroup", all.x=T)
  classinfo <- classinfo[order(classinfo$Index,classinfo$tmp),]
  outdf$class <- factor(outdf$class, levels = classinfo$tmp)
  outdf$class.title <- factor(outdf$class.title, levels = unique(classinfo$class.title))
  outdf$Kgroup <- factor(outdf$Kgroup, levels = unique(classinfo$Kgroup))
  
  return(outdf)
}

##################################################################
### CALCULATE THE CUMSUM FOR A SUBSET OF THE ENTIRE POPULATION ###
get_pmcf_cumsum <- function(appyearstart=1900, appyearend=2100, issueyearstart=1900, issueyearend=2100) {
  ##look at lab patents by count of primary class per the mcf
  
  #open counts by class from STATA
  pmcf_counts <- read.dta("~/Patents/USPTO/pmcf_cumsum.dta")
  
  #get primary counts by appyear, restricted by dates
  tmp_counts <- subset(pmcf_counts, appyear >= appyearstart & appyear<= appyearend & grantyear >= issueyearstart & grantyear <= issueyearend)

  #get the cumsum by appyear and class
  pmcf_cumsum <- ddply(tmp_counts[order(tmp_counts$appyear,tmp_counts$class_primary),],.(class_primary),transform,CUMSUM=cumsum(patentnum))
  
  return(pmcf_cumsum)
}

#########################################################################
### SHORT FUNCTION TO LABEL THE GROWTH REGION FOR A LIST OF PATENTS  ####
### ALOWS US TO SELECT WHICH LIST OF REGION DATES WE WANT TO USE     ####
labelpatregions <- function(dfpats, dfregions, iyear='appyear', iclass='class') {
  #1 - run the region labeler on each row of dfpats
  dfout <- adply(dfpats,1, function(xrow)  data.frame(region=markregion_single(xrow[,iyear],as.character(xrow[,iclass]),dfregions)))
  #2 - make sure the regions are ordered
  dfout$region <- factor(dfout$region,levels=c("Emerging","Growth","Maturity","Saturation","NA"))
  
  return(dfout)
}

#########################################################################
### Create the lab3 cleaned lab names
fixlab3 <- function(mydf){
  mydf$lab3 <- with(mydf, ifelse(service=="Air Force" & grepl("AFRL|Research Lab",lab.short2),"AFRL",as.character(lab.short2)))
  mydf$lab3 <- gsub("NAVAIR", "NAWC", mydf$lab3)
  mydf$lab3 <- gsub("NAVSEA", "NSWC", mydf$lab3)
  mydf$lab3 <- gsub("NSWC Newport", "NUWC Newport", mydf$lab3) #fix NUWCs
  mydf$lab3 <- gsub("NAWC Training Systems Division","NAVAIR Training Systems Division", mydf$lab3) #fix NAVAIR Training
  mydf$lab3 <- gsub("SSC", "SPAWAR Systems Center", mydf$lab3) #fix SSC
  mydf$lab3 <- gsub(",", "", mydf$lab3) #remove comma
  
  #show new list of labs
  print(unique(mydf$lab3))
  
  return(mydf)
}