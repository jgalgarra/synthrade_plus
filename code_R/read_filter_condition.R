filtered_file  <-  read.table("filtered_condition.txt",header=FALSE)
if (filtered_file$V1 == 1){
  filtered_string <- "_FILT" # if set to "_FILT" uses filtered matrix
} else
  filtered_string <- "" # if set to "" uses unfiltered matrix

if (filtered_file$V2 == 1){
  append_log <- TRUE # if set appends data to log simulation
} else
  append_log <- 0


if (filtered_file$V3 == 1){
  write_num_links <- TRUE # create the number of links file
} else
  write_num_links <- FALSE