library(grid)
library(gridExtra)
library(ggplot2)
source("aux_functions_matrix.R")
source("parse_command_line_args.R")

# Third command argument allows to plot densities at build up time
TFstring <- as.character(args[3])
if (is.na(TFstring)){
  TFstring <- ""
} else
  TFstring <- "TF_"


crea_lista_heatsimp <- function(matriz)
{
  df <- data.frame("X"=c(),"Y"=c(),"cuenta"=c())
  for (l in 1:nrow(matriz))
    for(m in 1:ncol(matriz)){
      dfaux <- data.frame("X"=l,"Y"=m,"cuenta"=matriz[l,m])
      df <- rbind(df,dfaux)
    }
  return(df)
}

paint_int_matrix <- function(mq,titulo="",maximo=1)
{
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  if (min(mq$cuenta)==0){
    minv <- min(min(mq[mq$cuenta>0,]$cuenta),0.00000001)
    mq$NormWeight <- mq$cuenta + minv
    b <- c(minv,1/100000,1/1000,max(0.1,mq$cuenta))
    vcols <- c("grey95","blue", "cyan","red")
  }
  else{
    minv <- round(min(mq$cuenta))
    mq$NormWeight <- mq$cuenta
    b <- c(1/100000000,1/100000,1/1000,max(0.1,mq$cuenta))
    vcols <- jet.colors(6)
  }
  zp1 <- ggplot(mq,aes(x = X, y = rev(Y)))
  zp1 <- zp1 + geom_tile(aes(fill=NormWeight)) + scale_fill_gradientn(name=titulo,
                                                                      colours=vcols,
                                                                      trans = "log",
                                                                               breaks =b, 
                                                                               labels=log10(b))
  zp1 <- zp1 + coord_equal() + ylab("Exporters") + xlab("Importers") #+ ggtitle(titulo) + 
  zp1 <- zp1 + theme_bw() +theme(panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 #legend.position = "right",
                                 axis.text = element_blank(),
                                 legend.title = element_text(face="bold", size=9),
                                 axis.ticks = element_blank(),
                                 panel.border = element_blank(),
                                 plot.title = element_text(hjust = 0.5))
  return(zp1)
}

if (TFstring == ""){
  subdir <- ""
} else
  subdir <- "TFMatrix/"


files <- paste0(TFstring,"RedAdyCom",seq(ini_seq,end_seq),"_FILT")
for (file_name in files)
{
  year=gsub("_FILT","",strsplit(file_name,"RedAdyCom")[[1]][-1])
  sbestKS <- TRUE
  if (sbestKS){
    bestKS <- read.table("../results/BestKS.txt",header=TRUE)
    experiment<- bestKS[bestKS$Year==year,]$Experiment
  }
  else
    experiment<-1
  
  if (length(experiment)== 0)
    experiment<-1
  experiment_files <- Sys.glob(paste0("../results/",subdir,file_name,"_W_*.txt"))
  zero_matrix <- read_and_remove_zeroes(experiment_files[experiment])
  # for (l in 1:nrow(zero_matrix))
  #   for(m in 1:ncol(zero_matrix))
  #     zero_matrix[l,m]<-0
  zero_matrix <- 0

  numexper <- 1
  for (i in 1:numexper){
    ind_matrix <- read_and_remove_zeroes(experiment_files[i])
    zero_matrix <- zero_matrix + ind_matrix
  }
  synth_matrix <- zero_matrix/numexper
  dred <- gsub(TFstring,"",file_name)
  emp_matrix <- read_and_remove_zeroes(paste0("../data/",dred,".txt"))
  # eliminar filas y columnas a cero
  sum_row <- rowSums(emp_matrix)
  sum_col <- colSums(emp_matrix)
  emp_matrix <- emp_matrix[sum_row>0,]
  emp_matrix <- emp_matrix[,sum_col>0]
  hm_emp <- crea_lista_heatsimp(MPack(emp_matrix,transpose = FALSE))
  hm_synth <- crea_lista_heatsimp(MPack(synth_matrix, transpose = FALSE ))
  
  # Marginal probabilities
  pr_emp_E <- rowSums(emp_matrix) 
  pr_emp_I <- colSums(emp_matrix)
  pr_emp_matrix <- pr_emp_E[] %o% pr_emp_I[]
  hm_emp_prob <- crea_lista_heatsimp(MPack(pr_emp_matrix,transpose = FALSE))
  
  # Marginal probabilities
  pr_synth_E <- rowSums(synth_matrix) 
  pr_synth_I <- colSums(synth_matrix)
  pr_synth_matrix <- pr_synth_E[] %o% pr_synth_I[]
  hm_synth_prob <- crea_lista_heatsimp(MPack(pr_synth_matrix,transpose = FALSE))
  
  maxleg <- (1+round(max(max(emp_matrix),max(synth_matrix)))%/%100)*100
  m_emp <- paint_int_matrix(hm_emp,titulo="log(W)\nEmpirical")
  m_synth <- paint_int_matrix(hm_synth,titulo="log(W)\nSynthetic")
  
  m_prob_emp <- paint_int_matrix(hm_emp_prob,titulo="log(P)\nEmpirical")
  m_prob_synth <- paint_int_matrix(hm_synth_prob,titulo="log(P)\nSynthetic")
  ppi <- 300  
  dir.create("../figures/matrixes", showWarnings = FALSE)
  fsal <- paste0("../figures/matrixes/STRENGTH_",file_name,"_nexper_",numexper,"_IntMatrix.png")
  png(fsal, width=10*ppi, height=4*ppi, res=ppi)
  grid.arrange(m_emp, m_synth,ncol=2)
  dev.off()
  
  fsal <- paste0("../figures/matrixes/SANDPROB_",file_name,"_nexper_",numexper,"_IntMatrix.png")
  png(fsal, width=10*ppi, height=8*ppi, res=ppi)
  grid.arrange(m_emp, m_synth, m_prob_emp, m_prob_synth,  ncol=2,nrow=2)
  dev.off()
}
