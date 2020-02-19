# Reads a trade matrix and removes lines with zeroes

read_and_remove_zeroes <- function(filename)
{
  or_matrix <- read.table(filename,sep="\t")
  clean_matrix <- or_matrix[,colSums(or_matrix) > 0]
  clean_matrix <- clean_matrix[rowSums(clean_matrix) > 0,]
  return(clean_matrix)
}

crea_lista_heatmap <- function(matriz, justcount = FALSE)
{
  df <- data.frame("N"=c(),"cuenta"=c(),"type"=c())
  # Only sum 1 per filled cell to return degree instead of weight
  if (justcount)
    matriz[matriz>0] = 1
  for (l in 1:nrow(matriz))
  {
    dfaux <- data.frame("N"=l,"cuenta"=sum(matriz[l,]),"type"="EXP")
    df <- rbind(df,dfaux)
  }
  for(m in 1:ncol(matriz))
  {
    dfaux <- data.frame("N"=m,"cuenta"=sum(matriz[,m]),"type"="IMP")
    df <- rbind(df,dfaux)
  }
  return(df)
}

MPack <- function(matrix,normalize = TRUE, transpose = FALSE)
{
  sum_row <- rep(0,nrow(matrix))
  sum_col <- rep(0,ncol(matrix))
  if (normalize)
    matrix = matrix/sum(matrix)
  for (i in 1:nrow(matrix))
    sum_row[i] <- sum(matrix[i,])
  for (i in 1:ncol(matrix))
    sum_col[i] <- sum(matrix[,i])
  ord_matrix <- matrix[rev(order(sum_row)),rev(order(sum_col))]
  if (transpose)
    return(t(ord_matrix))       # Transpose because of order of deprecated python-written matrixes
  else
    return(ord_matrix)
}

lread_network <- function(namenetwork, guild_astr = "pl", guild_bstr = "pol", directory="")
{
  # Reading species names
  namesred <- read.csv(paste0(directory,namenetwork),header=FALSE,stringsAsFactors=FALSE)
  names_guild_a <- namesred[1,2:ncol(namesred)]
  names_guild_b <- namesred[2:nrow(namesred),1]
  
  #Reading matrix data
  m <- read.csv(paste0(directory,namenetwork),header=TRUE,row.names=1)
  
  # Calc number of species of each guild
  num_guild_a <- ncol(m)
  num_guild_b <- nrow(m)
  # Create an graph object
  g <- graph.empty()
  # Add one node for each species and name it
  for (i in 1:num_guild_a){
    g <- g + vertices(paste0(guild_astr,i),color="white",guild_id="a",name_species=names_guild_a[i],id=i)
  }
  for (i in 1:num_guild_b){
    g <- g + vertices(paste0(guild_bstr,i),color="red",guild_id="b",name_species=names_guild_b[i],id=i)
  }
  
  # Adding links to the graph object
  mm <- matrix(unlist(list(m)),nrow=num_guild_b,ncol=num_guild_a)
  listedgesn <- which(mm!=0, arr.ind = T)
  listedgesn <- listedgesn[order(listedgesn[,1],listedgesn[,2]),]
  listedgesn[,1] <- paste0(guild_bstr,listedgesn[,1])
  listedgesn[,2] <- paste0(guild_astr,listedgesn[,2])
  g <- g + graph.edgelist(listedgesn)
  # Return values
  calc_values <- list("graph" = g, "matrix" = m, "num_guild_b" = num_guild_b, "num_guild_a" = num_guild_a,
                      "names_guild_a" = names_guild_a, "names_guild_b"=names_guild_b)
  return(calc_values)
  
}
