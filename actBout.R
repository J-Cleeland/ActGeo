# A wet bout is any period where activity >0, and a dry bout is any period where activity = 0. So for the wet bouts,
#where activity ranges from 1-200, there can be many take-offs and landings, but a landing has to be a value between
#1 and 200 that follows a value of 0.

actBout <- function(d.act = d.act){
activity <- rep(NA, nrow(d.act))
activity[d.act$Activity==0] <- "dry"
activity[d.act$Activity>0] <- "wet"
d.act$bout <- as.numeric(rep(seq_len(length(rle(activity)$lengths)), rle(activity)$lengths))
return(d.act)
}
