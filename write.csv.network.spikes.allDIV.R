write.csv.network.spikes.allDIV <-
function(s, ns.allDIV, outputdir) {
  
  basename <- paste( strsplit(get.file.basename(s[[1]]$file), split="_")[[1]][1],
                     strsplit(get.file.basename(s[[1]]$file), split="_")[[1]][2],
                     strsplit(get.file.basename(s[[1]]$file), split="_")[[1]][3],
                     sep="_" )
  csvfile= paste(outputdir,"/",basename, "_multiDIV","_ns.csv",sep="")
  
  meta.header=cbind(unlist( lapply(s, function(x)
    paste( strsplit(basename(x$file), ".h5")[[1]][1],".h5",sep="" ) ) ),
                    unlist( lapply(s, function(x) paste(x$rec.time[1], 
                                                        x$rec.time[2],
                                                        sep=" to ") ) ) )
  colnames(meta.header)=c("file", "recording time (s)")
  
  ns.combo<-do.call(rbind, ns.allDIV )
  rownames(ns.combo)<-NULL
  
  write.table(paste("Output summary comprised of :"),
              csvfile, sep=",", append=FALSE,row.names=FALSE,col.names=FALSE) 
  write.table(meta.header,
              csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE)
  
  write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  
  write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  
  write.table("Network Spike analysis at well level",
              csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE) 
  
  suppressWarnings(write.table(ns.combo ,
                               csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))
  
  write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  
  
  
}
