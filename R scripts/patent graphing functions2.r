###############################################################
##                                                           ##
##      R: Good practice - adding footnotes to graphics      ##
##                                                           ##
###############################################################

#constants
graphyears <- c(1958, 2010)
### load the class group color definitions
palette.Kgroup <- read.delim(paste(path.grpfile,"Kgroup colors.txt",sep="/"),sep="\t",header=T, stringsAsFactors=F)
mypal <- paste("#",palette.Kgroup$RRGGBB,sep="")
names(mypal) <- palette.Kgroup$Kgroup #name the color vector for ggplot

# basic information at the beginning of each script
scriptName <- "filename.R"
author <- "mh"
footnote <- paste(scriptName, format(Sys.time(), "%d %b %Y"),
                  author, sep=" / ")
#titlecase function borrowed from internet
source(paste(path.rscripts, "stringops.R", sep="/"))


# default footnote is today's date, cex=.7 (size) and color
# is a kind of grey

makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(1,"npc") - unit(2, "mm"),
             y= unit(2, "mm"),
             just=c("right", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}


#### Kay's graphical stuff ####

###Ryan's difference from mean proportion plot
plotMeanDif <- function(mydf, iclass = "class", ilab = "lab.short2", iregion = "region", iservice = "service", minct=5, servicestoinclude = c("Army", "Navy", "Air Force"), labstoinclude=unique(factor(mydf[,ilab])), classestoinclude=unique(factor(mydf[,iclass])), filepref="default") {
  
  #restrict included labs by service, labname, or class
  tmpdta <- mydf[which((mydf[,iclass] %in% classestoinclude) & !is.na(mydf[,iregion]) & (mydf[,ilab] %in% labstoinclude) & (mydf[,iservice] %in% servicestoinclude)),]
  
  #get counts in restricted dataframe
  classcounts <- getregioncounts(tmpdta, ilab, iregion, iclass)
  
  #exclude labs-class combos with very few patents, also we just want the Early list
  dtasub <- subset(classcounts,total > minct & region %in% c("Emerging", "Growth"))
  
  #plot the fraction early by lab per class
  meanplt_labclass <- ggplot(ddply(dtasub,.(lab,service),summarise,labmean=sum(count)/sum(total),labtotal=sum(total)))
  meanplt_labclass <- meanplt_labclass + geom_point(aes(x=factor(lab),y=labmean,color=service, size=labtotal)) + scale_size(name="Total Patents",trans="log10")
  meanplt_labclass <- meanplt_labclass+ theme(axis.text.x=element_text(angle=90, hjust=1),title="Fraction of Early Patents") + xlab("") + ylab("Fraction Early") + scale_y_continuous(limits = c(0,1.0))
  #then add a line showing the mean
  meanplt_labclass <- meanplt_labclass + geom_hline(data=dtasub, aes(yintercept=sum(count)/sum(total)))
  #faceted by class
  #meanplt_labclass <- meanplt_labclass + facet_wrap(~class_primary)
  print(meanplt_labclass)
  makeFootnote(paste("Excludes lab-class combinations with fewer than",minct,"patents",sep=" "))
  #save plot
  ggsave(paste("~/Patents/images/",filepref," mean early.pdf",sep=""), width=11, height=8.5, dpi=600)
}


################################################################################
### We often want to limit the dataset by type of lab and by count of class ###
### this function takes care of all that 
###############################################################################
prep_count_dta <- function(mydf, iclass, classestoinclude, ilab, labstoinclude, iservice, servicestoinclude, iregion, minct, ipat='pat_num',iyear="app_year") {
  #restrict included labs by service, labname, or class
 pltdf <- mydf[which(mydf[,iclass] %in% classestoinclude & !is.na(mydf[,iclass]) & (mydf[,ilab] %in% labstoinclude) & (mydf[,iservice] %in% servicestoinclude)),]
  
  #make sure all the column names are right in the pltdf
  names(pltdf)[names(pltdf) == ilab] <- "lab"
  names(pltdf)[names(pltdf) == iclass] <- "class"
  names(pltdf)[names(pltdf) == iservice] <- "service"
  #there may be more than one region
  if(iregion!="region") {
    #if iregion == "region" we're good to go, no change required
    if(length(which(names(pltdf)=="region")) > 0) {
      #then we need to change any columns currently named 'region'
      names(pltdf)[names(pltdf) == "region"] <- "region.tmp"
      #then change any regions not named 'region' to what our graphing progs are expecting
      names(pltdf)[names(pltdf) == iregion] <- "region"
    }
  }
  names(pltdf)[names(pltdf) == iregion] <- "region"
  #names(pltdf)[grep("[Tt]itle",names(pltdf))] <- "patent.title"
  names(pltdf)[names(pltdf) == ipat] <- "pat_num"
  names(pltdf)[names(pltdf) == iyear] <- "app_year"
  if(nrow(pltdf) > 0){
    ##EXCLUDE LAB-CLASS COMBINATIONS WITH FEWER THAN 5 (default) PATENTS
    tmpdta <- ddply(pltdf,.(lab,class),transform,total=length(pat_num))
    #exclude any lab-class combos that are too small
    pltdf <- subset(tmpdta, total > minct)
 }
  
  return(pltdf)
}

### tree map or mosaic polt for class coverage by lab ###
library('treemap')
plt.classmosaic <- function(mydf, iclass = "class", ilab = "lab.short2", iregion = "region", iservice = "service", ipat="pat_num", minct=5, servicestoinclude = c("Army", "Navy", "Air Force"), labstoinclude=unique(factor(mydf[,ilab])), classestoinclude=unique(factor(mydf[,iclass])), filepref="default", scalecolors='GnBu', scalemax=c(0,200)) {
  
  #restrict included labs by service, labname, or class & count
  # start of function content #
  pltdf <- prep_count_dta(mydf, iclass, classestoinclude, ilab, labstoinclude, iservice, servicestoinclude, iregion, minct, ipat)
  
  if(nrow(pltdf) > 0) {
    #reduce to just count per lab and class
    labclass_count <- ddply(pltdf, .(lab,class), summarise, count.all=length(pat_num))
    #add group labels
    labclass_count <- addclasstxt(labclass_count)
    labclass_count <- addgroupcolors(labclass_count)
  
    tmPlot(subset(labclass_count,!is.na(Kgroup)), index=c('Kgroup','class'), vSize='count.all', vColor='Index', title=filepref, vColorRange=scalemax, palette=scalecolors)
  
    #write to file
    dev.copy(pdf,paste("~//Patents//images//",filepref," mosaic.pdf",sep=""),width=7,height=6)
    dev.off()
  }
}

#this function creates the main bar count plus weighted cumsum graph we use everwhere, and returns a handle for the graph so you can modify it at will
#this version expects to have Kgroup to color by
fnMainGraph <- function(mydf, iclass = "class", ilab = "lab.short2", iregion = "region", iservice = "service", ipat="pat_num",iyear="app_year", minct=5, locmaxct=20, servicestoinclude = c("Army", "Navy", "Air Force"), labstoinclude=unique(factor(mydf[,ilab])), classestoinclude=unique(factor(mydf[,iclass])), filepref=paste(labstoinclude), vpalette = mypal, vfontsize=12, vpagesh=1, vpagesw=1, showlegend=T, vertical=F, saveopt=F, classwrap=NA, addline=T, mytitle=paste("Patent Counts for",filepref), fillswitch="default") {
  
  #restrict included labs by service, labname, or class & count
  # start of function content #
  pltdf <- prep_count_dta(mydf, iclass, classestoinclude, ilab, labstoinclude, iservice, servicestoinclude, iregion, minct, ipat, iyear)
  
  if(nrow(pltdf) > 0) { #just in case there are no qualifying rows remaining
    ####plots the count of patents by lab and class and overlays a normalized cumulative count of all patents in that class  
    
    #if classwrap index is set, then replace class code with short class name wrapped if necessary
    if(!is.na(classwrap)){ 
      myregex <- paste('(.{1,',classwrap,'})(\\s|$)',sep='')
      pltdf$class <- gsub('\n$','',gsub(myregex, '\\1\n', paste(pltdf$class, pltdf$shortname, sep=" ")))
      #remove anything in parens
      #pltdf$class <- gsub(' *\\(.+\\)','',pltdf$class)
      #factor ordering again
      pltdf$class <- factor(pltdf$class, levels=unique(pltdf$class[order(pltdf$Kgroup)]))
      
      #plt.cts <- plt.cts + geom_text(aes(label=class.title),x=graphyears[1]+2, y=locmaxct-5, vjust=1, color='grey50', alpha=0.1, size=3)
    }

    ##the first plot plus the full comparison plot, weighted to approx max count
    #check fill options
    myhists <- list(
              main = list(geom_histogram(aes(x=app_year, fill=factor(main)),position="stack", binwidth=1), scale_fill_manual(values=c("0"="black","1" ="red"), name="Main Class")),  #changed to color bars by main/not main
              cluster = list(geom_histogram(aes(x=app_year, fill=factor(cluster)),position="stack", binwidth=1), scale_fill_discrete(name="Inventor Cluster", guide=F)), #too many clusters to show guide
              patdetect = list(geom_histogram(aes(x=app_year, fill=factor(patdetect)),position="stack", binwidth=1), scale_fill_discrete(name="Emerging Patent Trend")), #too many clusters to show guide
              default = list(geom_histogram(aes(x=app_year, fill=Kgroup),position="dodge", binwidth=1), scale_fill_manual(values=vpalette)) #default is Kgroup
                                )
    plt.cts <- ggplot(pltdf) + myhists[[fillswitch]]

    
    if(!vertical) {
      plt.cts <- plt.cts + facet_wrap(~class) #add faceting
    }
    else {
      plt.cts <- plt.cts + facet_grid(class~.) #add faceting
    }

    
    if(addline){
      ###get cumulative activity by lab - make sure we keep Kgroup
      pltdf2 <- ddply(pltdf,.(lab,class,Kgroup, app_year),summarise,yearct=length(pat_num))
      pltdf2 <- ddply(pltdf2[order(pltdf2$lab, pltdf2$class, pltdf2$app_year),],.(lab,class),transform, CUMSUM=cumsum(yearct))
      pltdf2 <- ddply(pltdf2[order(pltdf2$lab, pltdf2$class, pltdf2$app_year),],.(lab,class),transform, normsum=CUMSUM/max(CUMSUM), com=sum(yearct*app_year)/sum(yearct))
      #have to scale normsum outside the ggplot call
      pltdf2$maxct <- locmaxct
      
      #add normalized s-curves - labs
      plt.cts <- plt.cts + geom_line(data=pltdf2, aes(x=app_year, y=normsum*maxct, color=Kgroup)) + scale_color_manual(values=vpalette)
      
      #change titel
      mytitle==paste("Patent Counts and Cumulatitive Sum (normalized) for",filepref)
    }
    
    #plot options go here
    plt.cts <- fnPltOps(plt.cts, mytitle, vfontsize=vfontsize, showlegend=showlegend, myymax=locmaxct)
    
    print(plt.cts)
    if(saveopt) ggsave(paste("~/Patents/images/",filepref,"-",fillswitch," counts.pdf",sep=""), width=11*vpagesw, height=8.5*vpagesh, dpi=600)
  }
  else {
    plt.cts <- NA
  }
  
  return(list(plt=plt.cts,classes=unique(pltdf$class), ymax=locmaxct, xmax=graphyears[2]))
}

#function to set all the axis properties and other options to a common standard
fnPltOps <- function(myplt, mytitle='default title', myxlab="Application Year", myylab="Count", showlegend=T, vfontsize=10, mygraphyears=graphyears, myymax=NA, strip.y.angle=-90, myrectx=2005){
  #text and axis options
  if(!is.na(myymax)) { #only limit yaxis if specified
    myplt <- myplt + coord_cartesian(xlim = mygraphyears, ylim=c(0,myymax+1))
  }
  else {
    myplt <- myplt + coord_cartesian(xlim = mygraphyears)
  }
  myplt <- myplt + xlab(myxlab) + ylab(myylab) + theme_bw() + theme(axis.text.x=element_text(size=vfontsize, angle=90, hjust=1), axis.text.y=element_text(size=vfontsize), axis.title.x=element_text(size=vfontsize*1.2), axis.title.y=element_text(size=vfontsize*1.2, angle=90), strip.text.y=element_text(size=vfontsize, angle=strip.y.angle), legend.text=element_text(size=vfontsize), legend.title=element_text(size=vfontsize))
  if(!showlegend) {
    myplt <- myplt + theme(legend.position="none")
  }
  else{
    myplt <- myplt + ggtitle(label=mytitle)
  }
  
  #add a rectangle blocking off 2005+
  rect <- data.frame(xmin=myrectx, xmax=Inf, ymin=0, ymax=Inf)
  myplt <- myplt + geom_rect(data=rect, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), fill='black', alpha=0.2)
  
  return(myplt)
}

#function to add a normalized scurve and vertical line showing growth and matruity points t0 a MainGraph
fnAddUniverseLine <- function(myplt, mycount, myclasses, maxy=10, myregions=NA, linecolor='black') {
  ##the normalized cumusum
  pmcf_cumsum2 <- ddply(subset(mycount,class %in% myclasses),.(class),transform,normsum=cumsum/max(cumsum), com=sum(patentnum*appyear)/sum(patentnum), yweight=max(patentnum))
  #rename pmcf columns so that both data sets have the same sorting name
  names(pmcf_cumsum2)[names(pmcf_cumsum2) == "class_primary"] <- "class"
  #need to add Kgroups to cumsum db
  #pmcf_cumsum2 <- addclasstxt(pmcf_cumsum2)
  pmcf_cumsum2$maxy <- maxy #need to make internal to df - can't remember what yweight or com were for
  
  #add normalized s-curves - universe
  myplt <- myplt + geom_point(data=pmcf_cumsum2, aes(x=appyear,y=normsum*maxy), colour=linecolor, alpha=0.2, size=1)
  #add vertical lines for regions
  if(!is.na(myregions)){
    myplt <- myplt + geom_vline(data=subset(myregions,class %in% myclasses), aes(xintercept=growth), colour=linecolor)
    myplt <- myplt + geom_vline(data=subset(myregions,class %in% myclasses), aes(xintercept=maturity), colour=linecolor, linetype="dashed")
  }
  print(myplt)
  return(myplt)
}

#this function shows the smoothed density of the given lab(s) and the universe
fnMainGraph.density <- function(mydf, univ_counts, iclass = "class", ilab = "lab.short2", iregion = "region", iservice = "service", ipat="pat_num", iyear="app_year", minct=5, servicestoinclude = c("Army", "Navy", "Air Force"), labstoinclude=unique(factor(mydf[,ilab])), classestoinclude=unique(factor(mydf[,iclass])), filepref=paste(labstoinclude), vpalette = mypal,vfontsize=12, vpagesh=1, vpagesw=1, saveopt=F, showlegend=T, classwrap=NA, facet.type='class', iKgrp='Kgroup', Kgroupstoinclude=unique(factor(mydf[,iKgrp]))) {
  #do Kgroup restriction outside prep_count_dat
  names(mydf)[names(mydf) == iKgrp] <- "Kgroup"
  #restrict included labs by service, labname, or class & count
  # start of function content #
  pltdf <- prep_count_dta(subset(mydf,Kgroup %in% Kgroupstoinclude), iclass, classestoinclude, ilab, labstoinclude, iservice, servicestoinclude, iregion, minct, ipat, iyear)
  
  if(nrow(pltdf) > 0) { #just in case there are no qualifying rows remaining
    
    #if classwrap index is set, then replace class code with short class name wrapped if necessary
    if(!is.na(classwrap)){ 
      myregex <- paste('(.{1,',classwrap,'})(\\s|$)',sep='')
      pltdf$class <- gsub('\n$','',gsub(myregex, '\\1\n', paste(pltdf$class, pltdf$shortname, sep=" ")))
      #remove anything in parens
      #pltdf$class <- gsub(' *\\(.+\\)','',pltdf$class)
      #factor ordering again
      pltdf$class <- factor(pltdf$class, levels=unique(pltdf$class[order(pltdf$Kgroup)]))   
      
      #have to do the same for univ_counts
      univ_counts$class <- gsub('\n$','',gsub(myregex, '\\1\n', paste(univ_counts$class, univ_counts$shortname, sep=" ")))
      univ_counts$class <- factor(univ_counts$class, levels=unique(univ_counts$class[order(univ_counts$Kgroup)]))
    }
    
    ####plots the count of patents by lab and class and overlays a normalized cumulative count of all patents in that class
    ##unless we want to import the origianl stata dataset, which we could, we need to do the universe density weighting by hand    
    univ_counts <- ddply(subset(univ_counts,class %in% pltdf$class),.(class),transform,patdens=patentnum/sum(patentnum))
    #make sure the class variable is named correctly so we facet correctly
        
    
    ##first plot the background, universe, density curve
    plt.dens <- ggplot() + geom_area(data=univ_counts, aes(x=appyear,y=patdens), fill="gray", color="black")
    #then add the lab's density curve with coloring
    plt.dens <- plt.dens + geom_density(data=pltdf, aes(x=app_year, color=Kgroup, fill=Kgroup, binwidth=1), alpha=0.4) + scale_fill_manual(values=vpalette, name="Class Group")+ scale_color_manual(values=vpalette,guide=F)
      
    #faceting options
    myfacets <- list(
        class = list(facet_wrap(~class, scales="free_y")),  
        lab = list(facet_grid(lab~class, scales="free_y")), 
        vertical = list(facet_grid(class~.))
        )
      
    plt.dens <- plt.dens + myfacets[[facet.type]] #add faceting
    
    #plot options go here
    if(facet.type == 'lab'){
      strip.y.angle = 0
    }
    else{
      strip.y.angle = -90
    }
    plt.dens <- fnPltOps(plt.dens, myylab="Patent Count Density", mytitle=paste("Patent Density for Universe and",filepref), vfontsize=vfontsize, showlegend=showlegend, strip.y.angle=strip.y.angle)
    
    print(plt.dens)
    
    if(saveopt) ggsave(paste("~/Patents/images/",filepref," densities.pdf",sep=""), width=11*vpagesw, height=8.5*vpagesh, dpi=600)
  }
  else {
    plt.dens <- NA
  }
  return(list(plt=plt.dens,classes=unique(pltdf$class)))
}

#this function shows the smoothed density of the given lab(s) and the universe
fnMainGraph.density2 <- function(mydf, univ_counts, iclass = "class", ilab = "lab.short2", iregion = "region", iservice = "service", minct=5, ipat='pat_num', servicestoinclude = c("Army", "Navy", "Air Force"), labstoinclude=unique(factor(mydf[,ilab])), classestoinclude=unique(factor(mydf[,iclass])), filepref=paste(labstoinclude), vpalette = mypal,vfontsize=12, vpagesh=1, vpagesw=1, saveopt=F, showlegend=T) {
  
  #restrict included labs by service, labname, or class & count
  # start of function content #
  pltdf <- prep_count_dta(mydf, iclass, classestoinclude, ilab, labstoinclude, iservice, servicestoinclude, iregion, minct, ipat)
  
  if(nrow(pltdf) > 0) { #just in case there are no qualifying rows remaining
    ####plots the count of patents by lab and class and overlays a normalized cumulative count of all patents in that class
    #universe should already have Kgroup by this point to save time
    univ_counts <- subset(univ_counts, class %in% pltdf$class)
    
    ##first plot the background, universe, density curve
    plt.dens <- ggplot() + geom_density(data=univ_counts, aes(x=appyear), fill="gray", color="black", alpha=0.4)
    #then add the lab's density curve with coloring
    plt.dens <- plt.dens + geom_density(data=pltdf, aes(x=app_year, color=Kgroup, fill=Kgroup, binwidth=1), alpha=0.4) + scale_fill_manual(values=vpalette, name="Class Group")+ scale_color_manual(values=vpalette,guide=F)
    plt.dens <- plt.dens + facet_wrap(~class, scales="free_y") #add faceting
    #text and axis options
    plt.dens <- plt.dens + scale_x_continuous(limits = graphyears) + xlab("Application Year") + ylab("Patent Count Density") + theme_bw() + theme(axis.text.x=element_text(size=vfontsize), axis.text.y=element_text(size=vfontsize), axis.title.x=element_text(size=vfontsize*1.2), axis.title.y=element_text(size=vfontsize*1.2, angle=90), strip.text.y=element_text(size=vfontsize, angle=-90), legend.text=element_text(size=vfontsize), legend.title=element_text(size=vfontsize))
    if(!showlegend) {
      plt.dens <- plt.dens + theme(legend.position="none")
    }
    else{
      plt.dens <- plt.dens + ggtitle(label=paste("Patent Density for Universe and",filepref))
    }
    print(plt.dens)
    if(saveopt) ggsave(paste("~/Patents/images/",filepref," desnities.pdf",sep=""), width=11*vpagesw, height=8.5*vpagesh, dpi=600)
  }
  else {
    plt.dens <- NA
  }
  return(list(plt=plt.dens,classes=unique(pltdf$class)))
}


#this function creates a heat map of inventor cluster activity
#scales_y = free cluster id and x-axis is time
#fill is the number of patents that year
fnClusterMap <- function(mydf, iclass = "class", ilab = "lab.short2", iregion = "region", iservice = "service",ipat="pat_num", minct=5, servicestoinclude = c("Army", "Navy", "Air Force"), labstoinclude=unique(factor(mydf[,ilab])), classestoinclude=unique(factor(mydf[,iclass])), filepref=paste(labstoinclude), vpalette = mypal,vfontsize=12, vpagesh=1, vpagesw=1,saveopt=F) {
  
  #restrict included labs by service, labname, or class & count
  # start of function content #
  pltdf <- prep_count_dta(mydf, iclass, classestoinclude, ilab, labstoinclude, iservice, servicestoinclude, iregion, minct, ipat)
  
  if(nrow(pltdf) > 0) { #just in case there are no qualifying rows remaining
    ####plots the count of patents by lab and class and overlays a normalized cumulative count of all patents in that class  
    #reduce to count per cluster per group
    pltdf <- count(pltdf, c("lab","cluster", "app_year", "Kgroup", "class"))
    #order clusters by time
    #tmp <- ddply(pltdf, .(lab, cluster), summarise, tot = sum(freq))
    pltdf$cluster <- factor(pltdf$cluster, levels=unique(pltdf$cluster[order(pltdf$app_year, pltdf$freq)]))
    
    plt.cts <- ggplot(pltdf) + geom_point(aes(x=app_year, y=cluster, color=Kgroup, size=freq), alpha=0.4) + scale_color_manual(values=vpalette, name="Class Group")
    
    #plt.cts <- plt.cts + facet_wrap(~class, scales="free_y") #add faceting
    
    #text and axis options
    plt.cts <- plt.cts + scale_x_continuous(limits = graphyears) + xlab("Application Year") + ylab("Cluster") + theme_bw() + theme(axis.text.x=element_text(size=vfontsize), axis.text.y=element_text(size=vfontsize), axis.title.x=element_text(size=vfontsize*1.2), axis.title.y=element_text(size=vfontsize, angle=90), strip.text.y=element_text(size=vfontsize, angle=-90), legend.text=element_text(size=vfontsize), legend.title=element_text(size=vfontsize), title=paste("Patent Counts by Inventor Cluster (normalized) for",filepref))
    print(plt.cts)
    if(saveopt) ggsave(paste("~/Patents/images/",filepref,"-clustermap",".pdf",sep=""), width=11*vpagesw, height=8.5*vpagesh, dpi=600)
  }
  else {
    plt.cts <- NA
  }
  return(list(plt=plt.cts,classes=unique(pltdf$class)))
}

#this version of the main graph function plots by lab
#and expects limitations by Kgroup
#vertical=T refers to orientation of classes
fnMainGraph.lab <- function(mydf, iclass = "class", ilab = "lab.short2", iregion = "region", iservice = "service",iKgrp="Kgroup",ipat="pat_num", minct=5, maxct=20, servicestoinclude = c("Army", "Navy", "Air Force"), labstoinclude=unique(factor(mydf[,ilab])), classestoinclude=unique(factor(mydf[,iclass])), Kgroupstoinclude=unique(factor(mydf[,iKgrp])), filepref=paste(labstoinclude), vpalette = mypal,vfontsize=10, vpagesh=1, vpagesw=1, showlegend=F, vertical=F, saveopt=F) {
  
  #do Kgroup restriction outside prep_count_dat
  names(mydf)[names(mydf) == iKgrp] <- "Kgroup"
  #restrict included labs by service, labname, or class & count
  # start of function content #
  pltdf <- prep_count_dta(subset(mydf,Kgroup %in% Kgroupstoinclude), iclass, classestoinclude, ilab, labstoinclude, iservice, servicestoinclude, iregion, minct, ipat)
  
  if(nrow(pltdf) > 0) { #just in case there are no qualifying rows remaining
    ####plots the count of patents by lab and class and overlays a normalized cumulative count of all patents in that class  
    
    ###get cumulative activity by lab - make sure we keep Kgroup
    pltdf2 <- ddply(pltdf,.(lab,class,Kgroup, app_year),summarise,yearct=length(pat_num))
    pltdf2 <- ddply(pltdf2[order(pltdf2$lab, pltdf2$class, pltdf2$app_year),],.(lab,class),transform, CUMSUM=cumsum(yearct))
    pltdf2 <- ddply(pltdf2[order(pltdf2$lab, pltdf2$class, pltdf2$app_year),],.(lab,class),transform, normsum=CUMSUM/max(CUMSUM), com=sum(yearct*app_year)/sum(yearct))
    
    ##the first plot plus the full comparison plot, weighted to approx max count
    plt.cts <- ggplot(pltdf) + geom_histogram(aes(x=app_year, fill=Kgroup),position="dodge", binwidth=1) + scale_fill_manual(values=vpalette)
    if(vertical) {
      plt.cts <- plt.cts + facet_grid(class~lab) #add faceting
    }
    else {
      plt.cts <- plt.cts + facet_grid(lab~class) #add faceting
    }
    
    #add normalized s-curves - labs
    plt.cts <- plt.cts + geom_line(data=pltdf2, aes(x=app_year, y=normsum*10, color=Kgroup)) + scale_color_manual(values=vpalette)
    #text and axis options
    plt.cts <- plt.cts + coord_cartesian(xlim = graphyears, ylim=c(0,maxct)) + xlab("Application Year") + ylab("Count") + theme_bw() + theme(axis.text.x=element_text(size=vfontsize, angle=90, hjust=0), axis.text.y=element_text(size=vfontsize), axis.title.x=element_text(size=vfontsize*1.2), axis.title.y=element_text(size=vfontsize*1.2, angle=90), strip.text.y=element_text(size=vfontsize, angle=0),
strip.text.x=element_text(size=vfontsize, angle=0), legend.text=element_text(size=vfontsize), legend.title=element_text(size=vfontsize),title=paste(paste(Kgroupstoinclude,sep=",",collapse=" & "),paste(servicestoinclude,sep=",",collapse=" & ")))
    if(!showlegend) {
      plt.cts <- plt.cts + theme(legend.position="none")
    }
    print(plt.cts)
    if(saveopt){ ggsave(paste("~/Patents/images/",paste(gsub("/","-",Kgroupstoinclude),sep="-",collapse="-"),paste(servicestoinclude,sep=",",collapse=" & ")," bars.pdf",sep=""), width=11*vpagesw, height=8.5*vpagesh, dpi=600)
    }
  }
  else {
    plt.cts <- NA
  }
  return(list(plt=plt.cts,classes=unique(pltdf$class), ymax=maxct, xmax=graphyears[2]))
}

#like the function above, this version of the density graph plots by lab
#and expects subsetting by Kgroup
fnMainGraph.density.lab <- function(mydf, univ_counts, iclass = "class", ilab = "lab.short2", iregion = "region", iservice = "service", iKgrp="Kgroup", ipat="pat_num", minct=5, servicestoinclude = c("Army", "Navy", "Air Force"), labstoinclude=unique(factor(mydf[,ilab])), classestoinclude=unique(factor(mydf[,iclass])),Kgroupstoinclude=unique(factor(mydf[,iKgrp])), filepref=paste(labstoinclude), vpalette = mypal,vfontsize=10, vpagesh=1, vpagesw=1, saveopt=F, showlegend=F) {
  
  #do Kgroup restriction outside prep_count_dat
  names(mydf)[names(mydf) == iKgrp] <- "Kgroup"
  #restrict included labs by service, labname, or class & count
  # start of function content #
  pltdf <- prep_count_dta(subset(mydf,Kgroup %in% Kgroupstoinclude), iclass, classestoinclude, ilab, labstoinclude, iservice, servicestoinclude, iregion, minct, ipat)
  
  if(nrow(pltdf) > 0) { #just in case there are no qualifying rows remaining
    ####plots the count of patents by lab and class and overlays a normalized cumulative count of all patents in that class
    ##unless we want to import the origianl stata dataset, which we could, we need to do the universe density weighting by hand    
    univ_counts <- ddply(subset(univ_counts,class %in% pltdf$class),.(class),transform,patdens=patentnum/sum(patentnum))
    #make sure the class variable is named correctly so we facet correctly
    
    
    ##first plot the background, universe, density curve
    plt.dens <- ggplot() + geom_area(data=univ_counts, aes(x=appyear,y=patdens), fill="gray", color="black", alph=0.4)
    #then add the lab's density curve with coloring
    plt.dens <- plt.dens + geom_density(data=pltdf, aes(x=app_year, color=Kgroup, fill=Kgroup, binwidth=1), alpha=0.4) + scale_fill_manual(values=vpalette, name="Class Group")+ scale_color_manual(values=vpalette,guide=F)
    plt.dens <- plt.dens + facet_grid(lab~class, scales="free_y") #add faceting
    #text and axis options
    plt.dens <- plt.dens + scale_x_continuous(limits = graphyears) + xlab("Application Year") + ylab("Patent Count Density") + theme_bw() + theme(axis.text.x=element_text(size=vfontsize, angle=90, hjust=0), axis.text.y=element_text(size=vfontsize), axis.title.x=element_text(size=vfontsize*1.2), axis.title.y=element_text(size=vfontsize*1.2, angle=90), strip.text.y=element_text(size=vfontsize, angle=0), strip.text.x=element_text(size=vfontsize, angle=0), legend.text=element_text(size=vfontsize), legend.title=element_text(size=vfontsize),title=paste(paste(Kgroupstoinclude,sep=",",collapse=" & "),paste(servicestoinclude,sep=",",collapse=" & ")))
    if(!showlegend) {
      plt.dens <- plt.dens + theme(legend.position="none")
    }
    print(plt.dens)
    if(saveopt) ggsave(paste("~/Patents/images/",paste(gsub("/","-",Kgroupstoinclude),sep="-",collapse="-"),paste(servicestoinclude,sep=",",collapse=" & ")," desnities.pdf",sep=""), width=11*vpagesw, height=8.5*vpagesh, dpi=600)
  }
  else {
    plt.dens <- NA
  }
  return(list(plt=plt.dens,classes=unique(pltdf$class)))
}
