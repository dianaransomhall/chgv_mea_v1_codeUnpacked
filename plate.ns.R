plate.ns <-
function(s){
  
  
  for (plate in length(s) ){
    
    ns=list()
    ns[[ length( s[[plate]]$goodwells ) ]] = 0
    for (well in c(1:length( s[[plate]]$goodwells ) ) ){
      
      ns[[well]] <- compute.ns(s[[plate]], ns.T=0.003, ns.N=6, 
                               sur=100, whichcells=s[[plate]]$goodwells[well]  )
      ns[[well]]$well = paste( s[[plate]]$goodwells[well] )
      print (paste("well ", paste(s[[plate]]$goodwells[well]), " finished" )  )
      
    } #end of for loop through wells in a plate
  } #end of loop through plates
  ns
}
