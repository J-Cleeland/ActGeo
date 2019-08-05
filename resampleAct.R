#Resamples .act file from either a BAS Mk7 and Mk19 GLS tag to reflects other BAS GLS models eg. MK5, MK9, MK10, MK13, MK14, MK15
#which sample for wet/dry every x seconds and make a record of the total number of samples wet every y minutes.

resampleAct <- function(d.act = d.act, sampling_int = sampling_int, record_bin = record_bin){
  #remove errors
  d.act <- d.act[grep("ERROR", d.act$Valid, invert=T),]
  #Expand the wet/dry status (**Wet** column) by the number of seconds in each state.
  act1 <- d.act[rep(seq_len(nrow(d.act)), d.act[['Activity']]), ]
  #Now unpack the **Date** column to every second.
  act1[['Date']] <- act1[['Date']] + sequence(d.act[['Activity']]) - 1
  #Update the **Activity** column to reflect state at one second intervals.
  act1[['Activity']] <- 1L
  #Cut to every x seconds according to the sampling interval.
  act2 <- act1[!duplicated(cut(act1$Date, paste0(sampling_int, ' sec'), labels = F)), ]
  #Sum number of **Wet** samples in y minute period according to the record bin.
  d.act <- aggregate(act2$Wet, list(Date = cut(act2$Date,  seq.POSIXt(min(act2$Date), max(act2$Date)+60*record_bin, by=paste0(record_bin," min")))), FUN = function(x) sum(x == 'wet'))
  #Reformat to match import file structure.
  colnames(d.act)[2] <- "Activity"
  d.act$Valid <- "ok"
  #Original outout is the number of seconds elapsed since the reference chosen when the file was processed, here we choose the common origin 1900-01-01
  d.act$Julian <- julian(d.act$Date, origin = "1900-01-01")
  d.act$Wet <- NA
  d.act$Day <- floor(as.numeric(d.act$Julian))
  d.act$Time <- as.numeric(d.act$Julian) - d.act$Day
  d.act <- d.act[,c("Valid", "Date", "Julian", "Activity", "Wet", "Day", "Time")]
  return(d.act)
}
