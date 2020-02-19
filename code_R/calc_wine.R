library(grid)
library(gridExtra)
library(ggplot2)
library(kcorebip)

MPack <- function(matrix,normalize = TRUE)
{
  sum_row <- rep(0,nrow(matrix))
  sum_col <- rep(0,ncol(matrix))
  if (normalize)
    matrix = matrix/max(matrix)
  for (i in 1:nrow(matrix))
    sum_row[i] <- sum(matrix[i,])
  for (i in 1:ncol(matrix))
    sum_col[i] <- sum(matrix[,i])
  ord_matrix <- matrix[rev(order(sum_row)),rev(order(sum_col))]
  return(ord_matrix)      
}

source("parse_command_line_args.R")

NREPS <- 100
files <- paste0("RedAdyCom",seq(ini_seq,end_seq))
for (orig_file in files)
{
  file_name <- paste0(orig_file,"_FILT")
  experiment_files <- Sys.glob(paste0("../results/",file_name,"_W_*.txt"))
  dfanid <- data.frame("wine"=c(),"volume"=c(),"max"=c(),"exper"=c())
  
  numexper <- length(experiment_files)
  or_matrix <- read.table(paste0("../data/",orig_file,".txt"),sep="\t")
  sum_row <- rep(0,nrow(or_matrix))
  sum_col <- rep(0,ncol(or_matrix))
  ind_matrix_p <- MPack(or_matrix)
  dfint <- as.data.frame(ind_matrix_p)
  w <- wine(dfint,nreps=NREPS)
  obswine <- w$wine
  print(paste0("Original file ",orig_file," wine ",obswine))
  dfanid <- rbind(dfanid,data.frame("wine"=obswine,"volume"=sum(or_matrix),"max"=max(or_matrix),"exper"=-1))
  
  
  emp_matrix <- read.table(paste0("../data/",file_name,".txt"),sep="\t")
  sum_row <- rep(0,nrow(emp_matrix))
  sum_col <- rep(0,ncol(emp_matrix))
  ind_matrix_p <- MPack(emp_matrix)
  # Remove all zeroes columns and rows
  dfint <- as.data.frame(ind_matrix_p)
  w <- wine(dfint,nreps=NREPS)
  obswine <- w$wine
  print(paste0("Filtered file ",file_name," wine ",obswine))
  dfanid <- rbind(dfanid,data.frame("wine"=obswine,"volume"=sum(emp_matrix),"max"=max(emp_matrix),"exper"=0))
  
  
  for (i in 1:numexper){
    ind_matrix <- read.table(experiment_files[i],sep="\t")
    ind_matrix_p <- MPack(ind_matrix)
    dfint <- as.data.frame(ind_matrix_p)
    w <- wine(dfint,nreps=NREPS)
    obswine <- w$wine
    print(paste0(experiment_files[i]," wine ",obswine))
    dfanid <- rbind(dfanid,data.frame("wine"=obswine,"volume"=sum(ind_matrix),"max"=max(ind_matrix),"exper"=i))
  }
  
  write.table(dfanid,paste0("../nestedness/",file_name,"_nestvalues.csv"),row.names = FALSE,
              sep=";")
}
