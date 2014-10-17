filter.spikes.dh <-
function(h5Files,
                           elec.min.rate=(1/60), 
                           elec.max.rate=25,
                           well.min.rate=15){
	# rates are in Hz
  	for (i in 1:length(h5Files)){
    		if (!(i==1)){
      		rm(s1,s2)
    		}
    		s1<-h5.read.spikes.dh(h5Files[i], beg= NULL, end=NULL)
   
    		#indices of low and high firing rate
    		low <- which(s1$meanfiringrate < elec.min.rate)
    		high <- which(s1$meanfiringrate > elec.max.rate)
    
    		## TODO, check that low and high are non-zero length vectors.
    		extremes <- c(low, high)
    
    		bad.ids <- names(extremes)
    		bad.ids <- c("-", bad.ids)  # "-" needed to remove these ids!
    
    		s2 <- remove.spikes(s1, bad.ids)
    
    		s2$treatment<-s1$treatment
        s2$genotype<-s1$genotype
        s2$pup<-s1$pup
        s2$trt.div<-s1$trt.div
        s2$cw<-s1$cw
        s2$rec.time<-s1$rec.time
    		s2$units<-s1$units
    		s2$dose<-s1$dose
    		s2$well<-s1$well
    
    		#get.num.AE
    		s2<-get.num.AE(s2)
    
    		#indices of low and high firing rate
    
    		low <- which(s2$nAE < well.min.rate)
    
    		bad.wells <- names(low)
    		bad.wells <- c("-", bad.wells)   # "-" needed to remove these well!
    		#just these three for example
    		s<- remove.spikes(s2, bad.wells)
    
    		s$goodwells<-names(which(s2$nAE >= well.min.rate))
    

    		#[which(s2$nAE >= well.min.rate)
    		s$treatment<-s1$treatment
     		names(s$treatment)<-s1$well
        s$pup<-s1$pup
        names(s$pup)<-s1$well
        s$genotype<-s1$genotype
        names(s$genotype)<-s1$well
        s$trt.div<-s1$trt.div
        names(s$trt.div)<-s$trt.div
    		s$units<-s1$units
      	names(s$units)<-s1$well
    		s$dose<-s1$dose
      	names(s$dose)<-s1$well

    		s$rec.time<-s1$rec.time

    		s$well<-s1$well
    
    		s<-get.num.AE(s)
    
  	}#end of filter loop through folder
	s
}
