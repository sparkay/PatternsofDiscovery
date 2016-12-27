#### Functions for S-curve analysis
get.Scurve2 <- function(mydata, x, t, nloglets) {
  #x and t specify the name of the columns in data containing the data and time respectively
  startvals <- get.startvals(mydata,x,t) #just double these
  #should return the initial params in order (K,B,a)
  #Koffset = -100 #number of counts to sift K
  #Boffset = -10 #number of years to shift the second loglet
  #dtoffset = 2 #multiplier for dt (a=ln(81)/dt)
  
  startvals2 <-param_gen(startvals,nloglets); #print(startvals2)
  
  fm <- term_gen(x,t,nloglets); #print(fm)
  
  #try nls
  est <- nls(fm,mydata,startvals2)
  return(est)
}

term_gen <- function(x,t,n) {
  #returns loglets formula for the nls function
  terms <- c()
  for (i in 1:n) {
    terms[i] <- paste("K",i,"/","(1+exp(","-a",i,"*","(",t,"-B",i,")))", sep="")
  }
  term_seq = paste(terms,collapse="+")
  return(paste(x,"~",term_seq,sep=""))
}

param_gen <- function(startvals,n) {
  #returns a list of starting parameters for the nls loglet estimator
  #params <- list(K1=startvals$K, a1=startvals$a, B1=startvals$B) #setup the first iteration without an offset
  params <- list()
  #use a random offset to tweak the startvals slightly?
  set.seed(1)
  namelst <- names(startvals)
  for (i in 1:n) {
    for (j in namelst) {
      params[paste(j,i,sep="")] <- startvals[[j]] - startvals[[j]]*(i-1)*runif(1,min=0,max=1) #ensure no modification if n=1
    }
  }
  return(params)
}

params_sequential <- function(mydata,x,t,nloglets=2) {
  #returns the loglet parameters with offsets for an n-parallel case
  #input params should not have the loglet number, e.b. K not K1
  #for now just works with n=2
  #based on dividing region into 2 halves, one for the first log and the second for the second
  #find 50% of the series value
  midpt <- median(mydata[,x])
  print(midpt)
  series1 <- data.frame(x=mydata[which(mydata[x]<midpt),x],
                        t=mydata[which(mydata[x]<midpt),x])
  series2 <- data.frame(x=mydata[which(mydata[x]>=midpt),x],
                        t=mydata[which(mydata[x]>=midpt),x])
  print(series1)
  print(series2)
  #recalculate params based on these sections
  output <- list()
  for (i in 1:nloglets) {
    tmpparams <- get.startvals(get(paste("series",i,sep="")),"x","t")
    for (letter in names(tmpparams)) {
      output[paste(letter,i,sep="")] <- tmpparams[letter]
    }
  }
  return(output)
}

get.startvals <- function(mydata, x, t) {
  #data is a matrix or dataframe with 2 columns
  #x and t specify the name (or number) of the columns in data containing the data and time respectively
  #sort by time order
  data <- mydata[order(mydata[,t]),]
  #set initial estimate for parameters
  K <- max(mydata[,x])*0.9 #K = growth limit, 90% of observed max as recommended by loglet guys?
  
  #B = time to reach K/2
  ind <- which(mydata[,x] <= K/2) #list of indices with data values <= K/2
  B <- mydata[max(ind),t] #year in which x = K/2 or the closest below it

  #a = ln(81)/dt, dt = time to go from 0.1 to 0.9 K
  #do same trick as with B
  nlow <- max(which(mydata[,x] <= 0.1*K))
  nhigh <- max(which(mydata[,x] <= 0.9*K))
  #dt is the difference between the two estimates
  dt <- (mydata[nhigh,t] - mydata[nlow,t])
  #a = ln(81)/dt
  a <- log(81/dt)
  return(list(K=K,B=B,a=a))
}

get.n.startvals <- function(mydata,x,t,n) {
  #returns a list with n sets of K,a,B parameters for loglet fitting
  #does this by dividing the dataset into n chunks
  datai <- seq(from=1, to=length(mydata[,x]),by=n)[1:n]
  #tack on last index to make the following loop work
  datai[n+1] <- length(mydata[,x])
  params <- list()
  for (iteri in (1:n)) {
    tmpvals <- get.startvals(mydata[datai[iteri]:datai[iteri+1],],x,t)
    for (parai in (1:length(tmpvals))) {
      params[paste(names(tmpvals)[parai],iteri,sep="")] <- tmpvals[parai]
    }
    
  }
  return(params)
}

my.coef <- function(nlsobj,default_coef=c(K1=NA,B1=NA,a1=NA)) {
  a <- tryCatch(coef(nlsobj),error=function(c) default_coef)
  return(a)
}

pretty.nls.output <- function(nlsobj) {
  #takes a nls object and returns the coefficients and rss as a vector
  output <- as.list(coef(nlsobj))
  output$class <- as.character(summary(nlsobj)$formula[2][1])
  #output <- data.frame()
  #for (elm in 1:length(tmp)) {
    #output[names(tmp)[elm]] <- tmp[elm]
  #}
  #output <- tapply(tmp,function(x) data.frame(name(x)=x))
  output$rss <- summary(nlsobj)$sigma
  
  return(as.data.frame(output))
}

run.loglets <- function(mydf,classnames,nloglets) {
  #get the nls objects
  tstrun <- lapply(classnames, 
                   function(x,mydata,t,nloglets) try(get.Scurve2(mydata,x,t,nloglets), TRUE), mydata=mydf,t="year",nloglets=nloglets)
  
  #put coefs, rss, class in a df, include try to ignore (for now) runs with errors
  tstout <- lapply(tstrun,function(x) try(pretty.nls.output(x),TRUE))
  tstoutdf <- as.data.frame(do.call(rbind, tstout))
  
  tstoutdf[which(is.na(tstoutdf$class)),] <- NA #fix error results
  
  #find out which classes we're missing, and fill in
  errclasses <- classnames[which(!(classnames %in% tstoutdf$class))]
  
  ###!!!OMG, the parameter values are stored as characters!!! use as.numeric to fix
  criteria <- names(tstoutdf)[which(names(tstoutdf) != "class")]
  tstoutdf[criteria] <- data.frame(mapply(as, tstoutdf[criteria], rep("numeric", length(criteria)), SIMPLIFY=FALSE), stringsAsFactors=FALSE)
  
  ####finally, make sure all of the class names are represented
  #remove any rows with NAs
  missing_classes <- classnames[which(!(classnames %in% tstoutdf$class))]
  #have to first fix the factoring
  tstoutdf$class <- factor(tstoutdf$class, levels=classnames)
  tstoutdf$class[which(is.na(tstoutdf$class))] <- missing_classes #these should be the same length
  
  
  return(tstoutdf)
}

loglet_vals <- function(t,nloglets,params) {
  #not rigorously error checked
  #based on term_gen()
  terms <- matrix(nrow=length(t),ncol=nloglets)
  for (i in 1:nloglets) {
    terms[,i] <- oneloglet(t,params[[paste("K",i,sep="")]],
                          params[[paste("B",i,sep="")]],
                          params[[paste("a",i,sep="")]])
  }
  term_seq = rowSums(terms)
  return(term_seq)
}

oneloglet <- function(t,K,B,a) {
  term <- K/(1+exp(-a*(t-B)))
  return(term)
}

oneloglet.t <- function(x, params) {
  t <- params$B - (log((params$K/x) - 1)/(params$a))
  return(t)
}

est.growth_range <- function(xvec,tvec,params) {
  #well, shoot... you don't actually need to fit the loglet to get the ranges, unless you are concerned about multiple loglets
  outpt <- data.frame(maturity = round(params$B,digits=0))
  #make sure the data is ordered
  vals_ordered <- xvec[order(tvec)]
  ind <- which(vals_ordered <= params$K*0.25) #list of indices with data values <= K/4
  outpt$growth <- tvec[max(ind)] #year in which x = K/4 or the closest below it
  ind <- which(vals_ordered <= params$K*0.75) #list of indices with data values <= 3K/4
  outpt$saturation <- tvec[max(ind)] #year in which x = 3K/4 or the closest below it
  ##I wonder if we should do some interpolation to get closer to the actual years, or just use a fit from the logistic equation
  return(outpt)
}

est.growth_range.fromloglet <- function(params) {
  #this version estimates the region points (0.25K, 0.5K=B. 0.75K) using the logistic equation and estimated parameters
  outpt <- data.frame(maturity = round(params$B,digits=0))
  
  outpt$growth <- round(oneloglet.t(x=params$K/4, params), digits=0) #closest whole year in which x = K/4
  
  outpt$saturation <- round(oneloglet.t(x=params$K*0.75, params), digits=0) #closest whole year in which x = 3K/4

  return(outpt)
}


#### plot 3 things:
##### 1: class_sums data as points
##### 2: predicted S-curve as line
##### 3: app_year of relevant patents as labels

check_scurve <- function(classnm, nlsobj, patyrs,intervals,xvec,tvec,chkerrors=FALSE) {
  #assumes certain column names
  ##patyrs should be in format data.frame(app_year,pat_num)
  #first need to bind everything together in a df for ggplot
  tmpdf <- data.frame(class=classnm,
                      sums=xvec,
                      year=tvec,
                      predicted=tryCatch(predict(nlsobj), 
                                         error=function(c) 0))
  #error checking
  if (chkerrors) {
    print(classnm)
    print(tmpdf)
  }
  #add in markers for the patent app years
  if(nrow(patyrs) == 0) {
    tmpdf$label <- ""
  }
  else {
    tmplabels <- data.frame()
    for (yr in unique(patyrs$app_year)) {
       tmplabels <- rbind(tmplabels, data.frame(year=yr, label=paste(patyrs$pat_num[which(patyrs$app_year == yr)],sep=",")))
    }
    tmpdf <- merge(tmpdf,tmplabels, all=TRUE)
    tmpdf$label <- as.character(tmpdf$label)
    tmpdf$label[which(is.na(tmpdf$label))] <- ""
  }
  #error checking
  if (chkerrors) {
    print("finished label section")
    print(tmpdf)
  }
  
  #add the regions
  if (chkerrors) {
    print("marking regions")
    print(intervals)
  }  
  tmpdf$regions <- markregions(tmpdf$year,intervals)
  tmpdf$regionval <- max(tmpdf$sums)
  
  if(chkerrors) {
    print("finished marking regions")
    print(tmpdf)
  }  
  
    #create the plot with the labels and points
  tmpplt <- ggplot(tmpdf,aes(x=year, y=sums, label=label)) + geom_point() + ggtitle(label=classnm)
  tmpplt <- tmpplt + geom_text(angle = 90, hjust=-1,position=position_jitter(h=10.0))
  #add the predicted curve
  tmpplt <- tmpplt + geom_line(aes(y=predicted))
  
  #f-it, I don't know why this doesn't work
  #tmpplt <- tmpplt + layer(data=tmpdf,aes(x=year,y=regionval,fill=regions),geom="area")
  tmpplt <- tmpplt + geom_area(aes(x=year,y=regionval,fill=regions), alpha=.2)
  #show the graph
  print(tmpplt)
  return(tmpplt)
}
markregions <- function(years,interval_lims) {
  #returns a vector with the s-curve region labels based on interval_lims and years (returned vector in same order as years)
  regionlst <- rep("",length(years))
  regionlst[which(years <= interval_lims$growth)] <- "Emerging"
  regionlst[which(years > interval_lims$growth & years <= interval_lims$maturity)] <- "Growth"
  regionlst[which(years > interval_lims$maturity & years <= interval_lims$saturation)] <- "Maturity"
  regionlst[which(years > interval_lims$saturation)] <- "Saturation"
  return(regionlst)
}

#now we are ready to search for relevant patents
#we could do this by patent, or we could do this by class
####here is the by class version
#1 - for class X
###2 - find all patent nums related to that class
###3 - determine which patents's app years are between growth and maturity
###4 - report patent numbers of qualifying patents
pats_growing_byclass <- function(classint, patyears, patclasses) {
  #classint is a vector with class name + growth, maturity, saturation dates
  #patyears is a df with patentnum and application year
  #patclass is a df with patentnum and associated class (1 per row)
  #error checking
  #dput(classint)
  ###2 - find all patent nums related to that class
  relpat <- patclasses$pat_num[which(patclasses$class == classint$class)]
  ###3 - determine which patents's app years are between growth and maturity
  ##3a grab app years only for those relevant patents
  relyrs <- subset(patyears,patyears$pat_num %in% relpat)
  ##3b check the date ranges for above subset
  qualifying <- relyrs$pat_num[which(
    (relyrs$app_year >= classint$growth) & 
      (relyrs$app_year < classint$maturity))]
  ###4 report qualifying patent numbers and years
  return(subset(patyears,patyears$pat_num %in% qualifying))
}

# this function isn't complete
check_scurve_wlab <- function(classnm, nlsobj, patyrs, intervals, xvec, tvec, chkerrors=FALSE) {
  #assumes certain column names
  ##patyrs should be in format data.frame(app_year,pat_num,lab.short2)
  #first need to bind everything together in a df for ggplot
  tmpdf <- data.frame(class=classnm,
                      sums=xvec,
                      year=tvec,
                      predicted=tryCatch(predict(nlsobj), 
                                         error=function(c) 0))
  #error checking
  if (chkerrors) {
    print(classnm)
    print(tmpdf)
  }
  #add in markers for the patent app years
  if(nrow(patyrs) == 0) {
    tmpdf$label <- ""
  }
  else {
    tmplabels <- data.frame()
    for (yr in unique(patyrs$app_year)) {
      tmplabels <- rbind(tmplabels, data.frame(year=yr, label=paste(patyrs$pat_num[which(patyrs$app_year == yr)], patyrs$lab.short2[which(patyrs$app_year == yr)], sep=",", collapse="/")))
    }
    tmpdf <- merge(tmpdf,tmplabels, all=TRUE)
    tmpdf$label <- as.character(tmpdf$label)
    tmpdf$label[which(is.na(tmpdf$label))] <- ""
  }
  #error checking
  if (chkerrors) {
    print("finished label section")
    print(tmpdf)
  }
  
  #add the regions
  if (chkerrors) {
    print("marking regions")
    print(intervals)
  }  
  tmpdf$regions <- markregions(tmpdf$year,intervals)
  tmpdf$regionval <- max(tmpdf$sums)
  
  if(chkerrors) {
    print("finished marking regions")
    print(tmpdf)
  }  
  
  #create the plot with the labels and points
  tmpplt <- ggplot(tmpdf,aes(x=year, y=sums, label=label)) + geom_point() + ggtitle(label=classnm)
  tmpplt <- tmpplt + geom_text(angle = 90, hjust=-1)
  #add the predicted curve
  tmpplt <- tmpplt + geom_line(aes(y=predicted))
  
  #f-it, I don't know why this doesn't work
  #tmpplt <- tmpplt + layer(data=tmpdf,aes(x=year,y=regionval,fill=regions),geom="area")
  tmpplt <- tmpplt + geom_area(aes(x=year,y=regionval,fill=regions), alpha=.2)
  tmpplt <- tmpplt + geom_histogram(data=patyrs, aes(x=app_year,color=lab.short2))
  #show the graph
  print(tmpplt)
  return(tmpplt)
}

#this function is ok
markregion_single <- function(yearvec,class,dfregions) {
  outregion <- c("")
  if(is.na(yearvec) | is.na(class)) {
    outregion <- c("NA")
    print(paste("year:",yearvec,"or class:",class," is NA", sep=" "))
  }
  else {
    outregion <- ifelse(yearvec <= dfregions[which(dfregions$class==class),"growth"], "Emerging", ifelse(yearvec <= dfregions[which(dfregions$class==class) ,"maturity"], "Growth", ifelse(yearvec <= dfregions[which(dfregions$class==class) ,"saturation"], "Maturity", "Saturation")))
  }
  #print(outregion)
  return(outregion)
}