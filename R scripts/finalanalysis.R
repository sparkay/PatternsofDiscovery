########################################################
# Loads your libraries, data, and supporting functions #
# All graphing moved to another file - KSF 20161217    #
########################################################

##libraries
library('plyr')
library('ggplot2')
library('lubridate')
library('stringr')
library('foreign')
library('reshape2')

#Set working directory and paths
#-----------------------------------------------------------------------------
  #setwd("Patents") #change to match your filestructure
  path.grpfile <- "Key datafiles/class groupings"
  path.rscripts <- "R scripts"
  path.datafiles <- "Key datafiles"
  path.images <- "images"
  path.datafiles.stattest <- "Key datafiles/MWW leadership tests"
#Load supporting scripts
#------------------------------------------------------------------------------
source(paste(path.rscripts, "Scurve_funs.r", sep="/"))
source(paste(path.rscripts, "patproc_funs.r", sep="/"))
source(paste(path.rscripts, "patent graphing functions2.r", sep="/"))

#Load essential files
#------------------------------------------------------------------------------
  #Load raw bibliographic files
  #---------------------------------------------------------------------------
  loadraw <- F
    #patent numbers with all bib by primary class
    if(loadraw) { #use older version
      bib_plus_primary_class <- read.dta(paste(path.datafiles,"STATA/bib_plus_primary_class2.dta", sep="/")) 
      names(bib_plus_primary_class)[names(bib_plus_primary_class) == "class_primary"] <- "class"
      #need to add Kgroups to cumsum db to get classes in right order
      bib_plus_primary_class <- addclasstxt(bib_plus_primary_class)
      #label the service with all assignee names - see USPTO curves.r
      #write text
      write.table(bib_plus_primary_class, paste(path.datafiles,"bib_plus_primary_class2.txt", sep="/"), row.names=F, sep="\t", quote=F)
      #write R object
      save(bib_plus_primary_class, file=paste(path.datafiles,"bib_plus_primary_class2.RData", sep="/"))
    } else { #jump to processed version
      load(paste(path.datafiles,"R/bib_plus_primary_class2.RData", sep="/"))
    }

    if(loadraw) { #use older version
      #patent numbers with all bib info, primary and sub class records
      mcfdta <- read.dta(paste(path.datafiles,"STATA/bib_plus_mcfcls.dta", sep="/")) 
      names(mcfdta)[names(mcfdta) == "class_primary"] <- "class"
      #need to add Kgroups to cumsum db to get classes in right order
      mcfdta <- addclasstxt(mcfdta)
      
      #write text
      write.table(mcfdta, paste(path.datafiles,"bib_plus_mcfcls.txt", sep="/"), row.names=F, sep="\t", quote=F)
      #write R object
      save(mcfdta, file=paste(path.datafiles,"bib_plus_mcfcls.RData", sep="/"))
    } else { #jump to processed version
      load(paste(path.datafiles,"R/bib_plus_mcfcls.RData", sep="/"))
    }
  
  #get counts and cumsum by sub class
  #-----------------------------------------------------------
    if(loadraw) { #use older version
      mcf_counts <- read.dta(paste(path.datafiles,"STATA/mcf_cumsum.dta", sep="/"))
      #designate main class variable
      mcf_counts$class_combo <- with(mcf_counts, paste(class_primary, class_sub,sep=""))
      names(mcf_counts)[which(names(mcf_counts)=="class_primary")] <- "class"
      #add Kgroup
      mcf_counts <- addclasstxt(mcf_counts)

      #write text
      write.table(mcf_counts, paste(path.datafiles,"mcf_cumsum.txt", sep="/"), row.names=F, sep="\t", quote=F)
      #write R object
      save(mcf_counts, file=paste(path.datafiles,"mcf_cumsum.RData",sep="/"))
    } else { #jump to processed version
      load(paste(path.datafiles,"R/mcf_cumsum.RData", sep="/"))
    }
      
  #get counts and cumsum by primary class
  #------------------------------------------------------------
  if(loadraw) { #use older version
    pmcf_cumsum <- read.dta(paste(path.datafiles,"STATA/pmcf_cumsum.dta", sep="/"))
    names(pmcf_cumsum)[which(names(pmcf_cumsum)=="class_primary")] <- "class"
    #add Kgroup
    pmcf_cumsum <- addclasstxt(pmcf_cumsum)
    
    #write text
    write.table(pmcf_cumsum, paste(path.datafiles,"pmcf_cumsum.txt", sep="/"), row.names=F, sep="\t", quote=F)
    #write R object
    save(pmcf_cumsum, file=paste(path.datafiles,"pmcf_cumsum.RData", sep="/"))
  } else { #jump to processed version
    load(paste(path.datafiles,"R/pmcf_cumsum.RData", sep="/"))
  }
  
  #############################################################
  #1990s+ versions of cumsums
  #-------------------------------------------------------------
    #get counts and cumsum by sub class
    #-----------------------------------------------------------
    if(loadraw) { #use older version
      mcf_counts <- read.dta(paste(path.datafiles,"STATA/mcf_cumsum_1990.dta", sep="/"))
      #designate main class variable
      mcf_counts$class_combo <- with(mcf_counts, paste(class_primary, class_sub,sep=""))
      names(mcf_counts)[which(names(mcf_counts)=="class_primary")] <- "class"
      #add Kgroup
      mcf_counts <- addclasstxt(mcf_counts)
      
      #write text
      write.table(mcf_counts, paste(path.datafiles,"mcf_cumsum_1990.txt", sep="/"), row.names=F, sep="\t", quote=F)
      #write R object
      save(mcf_counts, file=paste(path.datafiles,"mcf_cumsum_1990.RData", sep="/"))
    } else { #jump to processed version
      load(paste(path.datafiles,"R/mcf_cumsum_1990.RData", sep="/"))
    }
    
    #get counts and cumsum by primary class
    #------------------------------------------------------------
    if(loadraw) { #use older version
      pmcf_cumsum <- read.dta(paste(path.datafiles,"STATA/pmcf_cumsum_1990.dta", sep="/"))
      names(pmcf_cumsum)[which(names(pmcf_cumsum)=="class_primary")] <- "class"
      #add Kgroup
      pmcf_cumsum <- addclasstxt(pmcf_cumsum)
      
      #write text
      write.table(pmcf_cumsum, paste(path.datafiles,"pmcf_cumsum_1990.txt", sep="/"), row.names=F, sep="\t", quote=F)
      #write R object
      save(pmcf_cumsum, file=paste(path.datafiles,"pmcf_cumsum_1990.RData", sep="/"))
    } else { #jump to processed version
      load(paste(path.datafiles,"R/pmcf_cumsum_1990.RData", sep="/"))
    }   


  #latest processed mil patents
  #-------------------------------------------------------------
  if(0){ #old stuff, saving new version
    Pats_useme <- read.table("more data/latestdata.txt", sep="\t", stringsAsFactors=F, header=T)
    #crap - lost the factor ordering!
    Pats_useme <- addclasstxt(subset(Pats_useme, select=-c(Kgroup, class.title)))

    #bin all of AF together b/c of scarce data & other name cleaning
    Pats_useme <- fixlab3(Pats_useme)

    #bin the WCs together to match budget data
    Pats_useme$lab4 <- with(Pats_useme, ifelse(service=="Navy" & grepl("Carderock|Corona|Crane|Dahlgren|Dam Neck|Indian Head|Panama City|Hueneme",lab.short2),"NSWC",ifelse(service=="Navy" & grepl("Newport|Keyport", lab.short2), "NUWC", ifelse(service=="Navy" & grepl("Patuxent|China Lake|Point Mugu", lab.short2), "NAWC", ifelse(service=="Navy" & grepl("SPAWAR", lab.short2), "SPAWAR", as.character(lab3))))))
    
    #save text file
    write.table(Pats_useme, paste(path.datafiles,"Pats_useme.txt", sep="/"), sep="\t", row.names=F, quote=F)
    #save R object
    save(Pats_useme,file=paste(path.datafiles,"Pats_useme.RData", sep="/"))
  } else { #new version
    load(paste(path.datafiles,"R/Pats_useme.RData", sep="/"))
  }
  
  #original techmatch dataset - this may be obsolete
  #---------------------------------------------------------------
  Patents.by.Lab <- procTechMatch(paste(path.datafiles,"Tab/Patents by Lab.txt", sep="/"))

  #some additional data manipulation
  #-----------------------------------------------------------------
    
    
    #create exlude list - patents with fewer than 5 patents, techmatch labs, and the naval facilities engineering service center
    otherlst <- subset(count(Pats_useme, vars=c("lab3")), freq <= 5)$lab3
    excludelst <- c(otherlst, "Army TechMatch Patent Lab","Air Force TechMatch Patent Lab","Naval Facilities Engineering Service Center", "Naval Air Systems Command", "Office of Naval Research", "Redstone Technical Test Center","Strategic Systems Programs", "Naval Postgraduate School", "Army Yuma Proving Ground", "Army Space & Missile Defense Command")


