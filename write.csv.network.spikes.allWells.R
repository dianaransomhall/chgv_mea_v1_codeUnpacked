write.csv.network.spikes.allWells <-
function(s,nspikes,outputdir) {
  #we want all network spikes
  active.wells <- active.wells.network.spikes(nspikes)$ns.all
  p<-10
  nsdata <- matrix(0,length(s$well) , p )
  # one protocol for if there's at least on well of ns
  if (length(active.wells) >0) {
    
    active.well.names<-names(active.wells)
    for (cur.well in 1:length(s$well) ) {
      
      cur.well.name<-s$well[cur.well]
      if( is.element( cur.well.name, active.well.names) ) {

        print(cur.well.name)
        temp <- active.wells[[cur.well.name ]]
        nsdata[cur.well, 1:length(temp$brief)] <- temp$brief
        nsdata[cur.well, length(temp$brief)+1] <- min(temp$measures[,"peak.val"])
        nsdata[cur.well, length(temp$brief)+2] <- max(temp$measures[,"peak.val"])
        
      } else {
        print(cur.well.name)
        nsdata[cur.well, 1:p]<-rep(0,p)
        
      }#end of if active.well
    } # end of for loop
    
    } #end of if LENGTH(active.wells)>0
  
    nsdata <- data.frame(nsdata)	
    names(nsdata)[1:length(temp$brief)] <- names(temp$brief)
    names(nsdata)[(length(temp$brief)+1):(length(temp$brief)+2)] <- c("peak.min","peak.max")
    
  
  # gather meta-data
    DIV<-strsplit(s$file,split="_")[[1]][4]
    nsdata.meta<- cbind(s$well, s$genotype, s$pup, DIV, s$trt.div,
                   s$treatment, s$dose, s$units, nsdata)
    names(nsdata.meta) <-c(c("well","genotype","pup","DIV","trt.div",
                             "treatment","dose","units"), colnames(nsdata))
    
    basename <- get.file.basename(s$file)
    csvfile= paste(outputdir,"/",basename,"_ns.csv",sep="")
    
    write.table(paste("file= ",  strsplit( basename(s$file),".h5")[[1]][1], sep=""),
                csvfile, sep=",", append=FALSE,row.names=FALSE,col.names=FALSE) 
    write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
    #recording time
    write.table(paste("recording time (s): [", paste(s$rec.time[1],round(s$rec.time[2]), sep=" ,"),
                      "]",sep=""),csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
    
    write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
    
    write.table("Network Spike analysis at well level",
                csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)   	
    suppressWarnings(write.table(nsdata.meta ,
                                 csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))
    
    write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
    write.table("Network Spike analysis at electrode level",
                csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)  
    
    
    en.df <- do.call("rbind",lapply(nspikes$ns.all, function(well) {
      temp <- well$en.brief
      temp
    }))
    en.df <- en.df[order(rownames(en.df)), ] 
    en.df <- cbind(rownames(en.df),en.df)
    colnames(en.df)[1] <- "electrode"
    suppressWarnings(write.table(en.df,
                                 csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))
    
  
  nsdata.meta
  
}
