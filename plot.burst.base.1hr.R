plot.burst.base.1hr <-
function(s , resp, resp.label ){    
  	#resp="meanfiringrate" ; resp.label="Mean Firing Rate (Hz)"  

  	if (length(s[[1]]$well)<=12){
    		well.layout=c(4,3)
    		well.names <- paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = "")
    		if (s[[1]]$size[1]!="NA") {
      		treatment_label<-paste(c(s[[1]]$treatment[9:12],s[[1]]$treatment[5:8],s[[1]]$treatment[1:4]),
                            c(s[[1]]$size[9:12],s[[1]]$size[5:8],s[[1]]$size[1:4]),sep=" ")
    		} else if (s[[1]]$dose[1]!="NA") {
      		treatment_label<-paste(c(s[[1]]$treatment[9:12],s[[1]]$treatment[5:8],s[[1]]$treatment[1:4]),
                             c(s[[1]]$dose[9:12],s[[1]]$dose[5:8],s[[1]]$dose[1:4]),sep=" ")
    		} else {
      		treatment_label<-paste(c(s[[1]]$treatment[9:12],s[[1]]$treatment[5:8],s[[1]]$treatment[1:4]))
    		}
    
    		names(well.names) <- paste( paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
                                treatment_label,sep='=')
    		par.strip = list(cex = 1) 
    
  	} else {
    		well.layout=c(8,6)   
    		well.names<-paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = "")
    		if (s[[1]]$size[1]!="NA") {
      		treatment_label<-paste(c(s[[1]]$treatment[41:48],s[[1]]$treatment[33:40],s[[1]]$treatment[25:32],
                            s[[1]]$treatment[17:24],s[[1]]$treatment[9:16],s[[1]]$treatment[1:8]),
                          c(s[[1]]$size[41:48],s[[1]]$size[33:40],s[[1]]$size[25:32],
                            s[[1]]$size[17:24],s[[1]]$size[9:16],s[[1]]$size[1:8]),
                          sep=" ")
    		} else if (s[[1]]$dose[1]!="NA") {
      		treatment_label<-paste(c(s[[1]]$treatment[41:48],s[[1]]$treatment[33:40],s[[1]]$treatment[25:32],
                               s[[1]]$treatment[17:24],s[[1]]$treatment[9:16],s[[1]]$treatment[1:8]),
                             c(s[[1]]$dose[41:48],s[[1]]$dose[33:40],s[[1]]$dose[25:32],
                               s[[1]]$dose[17:24],s[[1]]$dose[9:16],s[[1]]$dose[1:8]),
                             sep=" ")
      
    		} else {
      		treatment_label<-paste(c(s[[1]]$treatment[41:48],s[[1]]$treatment[33:40],s[[1]]$treatment[25:32],
                               s[[1]]$treatment[17:24],s[[1]]$treatment[9:16],s[[1]]$treatment[1:8]))
      
    		}
    		names(well.names) <- paste( paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = ""),
                                treatment_label,sep='=')
    		par.strip = list(cex = .45) 
    
  	}
  
  	s12=list()
  
  	temp1<-axion.elec2well(s[[1]]$channels)
  	temp2<-axion.elec2well(s[[2]]$channels)
  	s12$active.wells<-c(temp1,temp2)
  
  	if (length(strsplit(resp,"$",fixed=TRUE)[[1]])>1 ){
    		response1<-get(strsplit(resp,"$",fixed=TRUE)[[1]][2] , get(strsplit(resp,"$",fixed=TRUE)[[1]][1], s[[1]]) )
  	} else {
    		response1<-get(strsplit(resp,"$",fixed=TRUE)[[1]][1],  s[[1]])
  	}
  
  	if (length(strsplit(resp,"$",fixed=TRUE)[[1]])>1 ){
    		response2<-get(strsplit(resp,"$",fixed=TRUE)[[1]][2] , get(strsplit(resp,"$",fixed=TRUE)[[1]][1], s[[2]]) )
  	} else {
    		response2<-get(strsplit(resp,"$",fixed=TRUE)[[1]][1],  s[[2]])
  	}
  
  	s12$response<-c(response1, response2)
  
  	s12$channels<-c(s[[1]]$channels, s[[2]]$channels)
  
  	s12$timepoint<-c(rep(s[[1]]$timepoint,length(response1)), rep(s[[2]]$timepoint, length(response2)))
  
  	s12$file<-s[[1]]$file
  
  	p<-xyplot(response ~ factor(channels) | 
              factor(active.wells, labels=names(well.names),levels = well.names),
            data = s12, group=timepoint, 
            drop.unused.levels = FALSE, layout = well.layout, 
            key= list(text=list(c("Timepoint","0","1")),
                      points=list(pch=c(NA, 1,2), col=c(NA,"red","blue") ),
                      space="right"),   #this adds a complete key
            xlab = "Channels within well",
            ylab = paste(resp.label,sep=""), 
            
            pch = c(1,2) , col=c("red", "blue"),
            main= paste( paste(resp.label, " by Channels within Wells",sep=""), 
                         paste("file= ",  strsplit( basename(s12$file),".h5")[[1]][1], sep=""),                    
                         sep='\n') , 
            
            cex.main=1,  
            scales = list(x = list(relation = "free",
                                   draw = FALSE)),
            par.strip.text = par.strip,
            par.settings = list(layout.heights=list(strip=2)),)
  
  
  	print( p)
  	p
}
