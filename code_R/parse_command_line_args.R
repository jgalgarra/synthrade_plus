args = commandArgs(trailingOnly=TRUE)
if (length(args)==0){
  ini_seq <- 2000
  end_seq <- 2000
} else{
  ini_seq <- as.numeric(args[1])
  end_seq <- as.numeric(args[2])
}