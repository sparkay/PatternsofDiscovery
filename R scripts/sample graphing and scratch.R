#### Example graphing functions ####

# Edit the params to graph different labs

#params
saveme <- F
lablst <- c("AFRL")

#get count plot with line
fnMainGraph(Pats_useme,ilab='lab3', labstoinclude=lablst, vfontsize=8, saveopt=saveme, vpagesw=14/11)
#get density plot
fnMainGraph.density(Pats_useme, pmcf_cumsum,ilab='lab3', labstoinclude=lablst, vfontsize=8, saveopt=saveme, vpagesw=14/11)
#get early/late (per Eusebi) plot - I believe this is obsolete
fnMainGraph.markmain(Pats_useme,ilab='lab3', labstoinclude=lablst, fillswitch="patdetect", vfontsize=8, saveopt=saveme, vpagesw=14/11)


#-------------------------------------------------------
# old scratch work - trying to figure out if I need this

#need to get new clusters using grouped AFRL



#do a new histogram of class counts using AF together - exclude labs with fewer than 5 patents - get that list from compare USPTO coverage Patents.by.LAb
plt <- ggplot(count(subset(Pats_useme, !(lab3 %in% excludelst)),vars=c("lab3", "class"))) + geom_histogram(aes(x=freq), binwidth=1) + facet_wrap(~lab3, scales="free")
plt <- plt + theme_gray(base_size=16) + xlab("Patents per Class") + ylab("frequency") + theme(strip.text.x=element_text(size=10))
print(plt)
if(saveme) { ggsave(paste(path.images,"class_count_hist.png",sep="/"), width=14, height=9, dpi=600)}
#-------------------------------------------------------