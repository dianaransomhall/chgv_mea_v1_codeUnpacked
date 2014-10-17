spike.as.dataframe <-
function(s) {
	s <- calculate.isis(s)
	df <- cbind(s$nspikes,s$meanfiringrate,s$mean.isis,s$sd.isis)
	colnames(df) <- c("nspikes","meanfiringrate","meanisis","sdisis")
	#df <- data.frame(df)
	return(df)
}
