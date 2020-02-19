source("parse_command_line_args.R")

get_data <- function(file_name,filter=FALSE, minconnectance = 0.0001)
{

connectance <- 0
wiperatio <- 0.001
while (connectance < minconnectance)
{
  r_df <- read.table(paste0("../data/",file_name,".txt"),sep="\t")
  r_df <- r_df[rowSums(r_df)>0,]
  r_df <- r_df[,colSums(r_df)>0]
  r_matrix <- as.matrix(r_df);
  r_matrix <- r_matrix[r_matrix>0]
  r_aux <- r_matrix[order(r_matrix)]
  sum_tot <- sum(r_matrix)
  cs <- cumsum(r_aux)
  partial <- r_aux[cs < wiperatio*sum_tot]
  posimin <- length(partial)
  min_allowed <- partial[posimin]
  print(paste("min_allowed",min_allowed))
  if (filter)
    r_matrix <- r_matrix[r_matrix >= min_allowed]
  
  maximo <- max(r_matrix)
  minimo <- min(r_matrix)
  ratio <- maximo/minimo
  numlinks <- sum(r_matrix > 0)
  print(file_name)
  print(paste0("Sum: ",sum(r_matrix)," orig_links ",sum(r_df>0)," filt links: ",numlinks," max:",maximo," min: ", minimo," ratio: ",ratio))
  for (i in 1:nrow(r_df))
    for (j in 1:ncol(r_df))
      if (r_df[i,j] < min_allowed)
        r_df[i,j] = 0
  print(paste0("Porcentaje eliminado: ", 100*(sum_tot-sum(r_matrix))/sum_tot))  
  r_df <- r_df[,colSums(r_df)>0]
  r_df <- r_df[rowSums(r_df)>0,]
  connectance <- sum(r_df>0)/((nrow(r_df)-1)*ncol(r_df))
  print(paste("connectance",connectance ))
  wiperatio <- wiperatio/2
}
return(r_df)
}
files = paste0("RedAdyCom",seq(ini_seq,end_seq))
for (nf in files){
  print("Filtered")
  r <- get_data(nf, filter = TRUE, minconnectance = 0.0001)    
  r <- r[,colSums(r)>0]
  r <- r[rowSums(r)>0,]
  write.table(r,paste0("../data/",nf,"_FILT.txt"),row.names = FALSE,col.names = FALSE,sep="\t")
}
