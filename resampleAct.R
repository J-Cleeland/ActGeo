resampleAct <- function(d.act = d.act, sampling_int = sampling_int, record_bin = record_bin){
  #Expand the wet/dry status by the number of seconds in each state
  act1 <- d.act[rep(seq_len(nrow(d.act)), d.act[['Activity']]), ]
  #Now unpack the time to every second 
  act1[['Date']] <- act1[['Date']] + sequence(d.act[['Activity']]) - 1
  #Update the activity column to reflect state at one second intervals 
  act1[['Activity']] <- 1L
  #Cut to every 3 seconds
  act2 <- act1[!duplicated(cut(act1$Date, paste0(sampling_int, ' sec'), labels = F)), ]
  #Sum number of samples in 10 minute period
  d.act <- aggregate(act2$Wet, list(Date = cut(act2$Date,  seq.POSIXt(min(act2$Date), max(act2$Date)+60*record_bin, by=paste0(record_bin," min")))), FUN = function(x) sum(x == 'wet'))
  #Reformat to match import format
  colnames(d.act)[2] <- "Activity"
  d.act$Valid <- "ok"
  d.act$Julian <- as.numeric(NA)
  d.act <- d.act[,c("Valid", "Date", "Julian", "Activity")]
return(d.act)
}