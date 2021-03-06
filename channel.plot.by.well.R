channel.plot.by.well <-
function(s , resp, resp.label ){   
  	par(mfrow=c(1,1))  
  	if (length(s$well)<=12){
    		well.layout=c(4,3)
    		well.names <- paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = "")
    		treatment_size<-paste(c(s$treatment[9:12],s$treatment[5:8],s$treatment[1:4]),
                          c(s$size[9:12],s$size[5:8],s$size[1:4]),sep=" ")
    		names(well.names) <- paste( paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
                                treatment_size,sep='=')
    		par.strip = list(cex = 1) 

  	} else {
    		well.layout=c(8,6)   
    		well.names<-paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = "")
    		treatment_size<-paste(c(s$treatment[41:48],s$treatment[33:40],s$treatment[25:32],
                            s$treatment[17:24],s$treatment[9:16],s$treatment[1:8]),
                          c(s$size[41:48],s$size[33:40],s$size[25:32],
                            s$size[17:24],s$size[9:16],s$size[1:8]),sep=" ")
    		names(well.names) <- paste( paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
                                treatment_size,sep='=')
    		par.strip = list(cex = .6) 
    
  	}
  
  	s$active.wells<-axion.elec2well(s$channels)
  
  
  	if (length(strsplit(resp,"$",fixed=TRUE)[[1]])>1 ){
    		response<-get(strsplit(resp,"$",fixed=TRUE)[[1]][2] , get(strsplit(resp,"$",fixed=TRUE)[[1]][1], s) )
  	} else {
    		response<-get(strsplit(resp,"$",fixed=TRUE)[[1]][1],  s)
  	}
  
  	p <- xyplot(response ~ factor(channels) | 
                factor(active.wells, labels=names(well.names),levels = well.names),
              data = s, drop.unused.levels = FALSE, layout = well.layout, 
              xlab = "Channels within well",
              ylab = paste(resp.label,sep=""), pch = 20 ,
              main= paste( paste(resp.label, " by Channels within Wells",sep=""), 
                                  paste("file= ",  strsplit( basename(s$file),".h5")[[1]][1], sep=""),                             
                                  sep='\n') , 
              scales = list(x = list(relation = "free",
                                     draw = FALSE)),
              par.strip.text = par.strip )

  
  	print( p)
  	p
}
