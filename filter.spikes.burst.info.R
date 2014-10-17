filter.spikes.burst.info <-
function(h5Files,user.defined.goodwells=c("GG"),
	elec.min.rate=1/60,elec.max.rate=25,well.min.rate = 10){  
  	h5Files<-sort(h5Files)
  	s=list()
  	count <- 0
  	##obtain data from all h5Files
  	for (i in 1:length(h5Files)){
    		timepoint<-substring(basename(h5Files[i]),
                         nchar(basename(h5Files[i]))-8,
                         nchar(basename(h5Files[i]))-7)
    		
      	current<-filter.spikes.dh(h5Files[i],
          elec.min.rate = elec.min.rate,
				 elec.max.rate = elec.max.rate,
         well.min.rate = well.min.rate )
      	current$timepoint<-timepoint
    		if (current$nspikes[1] >0) {
    			#allb vars: beg  end  IBI len    durn  mean.isis SI
    			#beg=index of first spike included in the burst in question
    			#end=index of last spike included in the burst in question
    			#len=# spikes in the burst
    			#durn=time within a burst
    			#mean.isis=mean interspike interval within a burst
    			#(len-1)*mean.isis~=durn
    		
    			current$allb <- lapply(current$spikes, mi.find.bursts)
    			current$bs<-calc.burst.summary(current)
			count <- count + 1
			s[[count]] <- current
		} 
    
  	}#end of for loop that loads all data
  	s
}
