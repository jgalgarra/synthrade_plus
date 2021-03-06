# Density and probability plots of synthetic and empirical networks
#
# It shows 
#        1. Density matrixes of empirical, synthetic, BOOST 50 2% and BOOST 200 2%
#        2. Probability matrixes of synthetic, BOOST 50 2% and BOOST 200 2% in a row
#        3. Weight and probability matrixes of synthetic, BOOST 50 2% and BOOST 200 2% in a row
#    
# Author: Javier Garcia Algarra
#
# Invocation: Rscript paint_matrix_plus year
#
# Example: Rscript paint_matrix_plus.R 2005

library(grid)
library(gridExtra)
library(ggplot2)
source("aux_functions_matrix.R")

args = commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])

subdir <- ""
numexper <- 1

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
  zp1 <- zp1 + coord_equal() + ylab("Exporters") + xlab("Importers") 
  zp1 <- zp1 + theme_bw() +theme(panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 #legend.position = "right",
                                 axis.text = element_blank(),
                                 legend.title = element_text(face="bold", size=9),
                                 axis.ticks = element_blank(),
                                 panel.border = element_blank(),
                                 plot.title = element_text(size = 14,  face = "bold", hjust = -0.1))
  return(zp1)
}

files <- paste0("RedAdyCom",year,"_FILT")
for (file_name in files)
{
  year=gsub("_FILT","",strsplit(file_name,"RedAdyCom")[[1]][-1])
  experiment<-1

  experiment_files <- Sys.glob(paste0("../results/",subdir,file_name,"_W_",experiment,".txt"))
  exper_improved_50 <-Sys.glob(paste0("../results/",subdir,file_name,"_W_",experiment,"_FBAL_BOOST_0.98_50.txt"))
  exper_improved_200 <-Sys.glob(paste0("../results/",subdir,file_name,"_W_",experiment,"_FBAL_BOOST_0.98_200.txt"))
  zero_matrix <- read_and_remove_zeroes(experiment_files[experiment])
  zero_matrix <- 0

  synth_matrix <- read_and_remove_zeroes(experiment_files[1])
  synthimpr_matrix_50 <- read_and_remove_zeroes(exper_improved_50[1])
  synthimpr_matrix_200 <- read_and_remove_zeroes(exper_improved_200[1])
  #dred <- gsub(TFstring,"",file_name)
  dred <- file_name
  emp_matrix <- read_and_remove_zeroes(paste0("../data/",dred,".txt"))
  # eliminar filas y columnas a cero
  sum_row <- rowSums(emp_matrix)
  sum_col <- colSums(emp_matrix)
  emp_matrix <- emp_matrix[sum_row>0,]
  emp_matrix <- emp_matrix[,sum_col>0]
  hm_emp <- crea_lista_heatsimp(MPack(emp_matrix,transpose = FALSE))
  hm_synth <- crea_lista_heatsimp(MPack(synth_matrix, transpose = FALSE ))
  hm_synthimpr_matrix_50 <- crea_lista_heatsimp(MPack(synthimpr_matrix_50, transpose = FALSE ))
  hm_synthimpr_matrix_200 <- crea_lista_heatsimp(MPack(synthimpr_matrix_200, transpose = FALSE ))
  
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

  # Marginal probabilities
  pr_synthimpr_matrix_50_E <- rowSums(synthimpr_matrix_50)
  pr_synthimpr_matrix_50_I <- colSums(synthimpr_matrix_50)
  pr_synthimpr_matrix_50 <- pr_synthimpr_matrix_50_E[] %o% pr_synthimpr_matrix_50_I
  hm_synthimpr_matrix_50_prob <- crea_lista_heatsimp(MPack(pr_synthimpr_matrix_50,transpose = FALSE))

  
  # Marginal probabilities
  pr_synthimpr_matrix_200_E <- rowSums(synthimpr_matrix_200)
  pr_synthimpr_matrix_200_I <- colSums(synthimpr_matrix_200)
  pr_synthimpr_matrix_200 <- pr_synthimpr_matrix_200_E[] %o% pr_synthimpr_matrix_200_I
  hm_synthimpr_matrix_200_prob <- crea_lista_heatsimp(MPack(pr_synthimpr_matrix_200,transpose = FALSE))
  
  
  maxleg <- (1+round(max(max(emp_matrix),max(synth_matrix)))%/%100)*100
  m_emp <- paint_int_matrix(hm_emp,titulo="log(W)\nEmpirical")
  m_synth <- paint_int_matrix(hm_synth,titulo="log(W)\nSynthetic")
  m_synthimpr_matrix_50 <- paint_int_matrix(hm_synthimpr_matrix_50,titulo="log(W)\nBOOST 50") 
  m_synthimpr_matrix_200 <- paint_int_matrix(hm_synthimpr_matrix_200,titulo="log(W)\nBOOST 200") 
  
  m_prob_emp <- paint_int_matrix(hm_emp_prob,titulo="log(P)\nEmpirical")
  m_prob_synth <- paint_int_matrix(hm_synth_prob,titulo="log(P)\nSynthetic")
  m_prob_synthimpr_matrix_50 <- paint_int_matrix(hm_synthimpr_matrix_50_prob,titulo="log(P)\nBOOST 50")
  m_prob_synthimpr_matrix_200 <- paint_int_matrix(hm_synthimpr_matrix_200_prob,titulo="log(P)\nBOOST 200")
  
  
  ppi <- 300
  dir.create("../figures/", showWarnings = FALSE)
  dir.create("../figures/matrixes", showWarnings = FALSE)
  fsal <- paste0("../figures/matrixes/STRENGTH_",file_name,"_nexper_",numexper,"_IntMatrix.png")
  png(fsal, width=10*ppi, height=8*ppi, res=ppi)
  grid.arrange(m_emp, m_synth,m_synthimpr_matrix_50,m_synthimpr_matrix_200,ncol=2,nrow=2)
  dev.off()
  
  fsal <- paste0("../figures/matrixes/PROB_",file_name,"_nexper_",numexper,"_IntMatrix.png")
  png(fsal, width=14*ppi, height=4*ppi, res=ppi)
  grid.arrange(m_prob_synth, m_prob_synthimpr_matrix_50,m_prob_synthimpr_matrix_200, ncol=3)
  dev.off()
  
  fsal <- paste0("../figures/matrixes/SANDPROB3_",file_name,"_nexper_",numexper,"_IntMatrix.png")
  png(fsal, width=14*ppi, height=8*ppi, res=ppi)
  grid.arrange(m_synth,m_synthimpr_matrix_50,m_synthimpr_matrix_200,m_prob_synth, m_prob_synthimpr_matrix_50,m_prob_synthimpr_matrix_200, nrow=2,ncol=3)
  dev.off()
  
  
  
  ppi <- 144
  fsal <- paste0("../figures/matrixes/SANDPROB3_",file_name,"_nexper_",numexper,"_IntMatrix.eps")
  cairo_ps(filename = fsal,
           width = 7, height = 4, pointsize = 4,
           fallback_resolution = ppi)
  g <- grid.arrange(m_synth+ggtitle("a"),m_synthimpr_matrix_50+ggtitle("b"),
                    m_synthimpr_matrix_200+ggtitle("c"),m_prob_synth+ggtitle("d"), 
                    m_prob_synthimpr_matrix_50+ggtitle("e"),m_prob_synthimpr_matrix_200+ggtitle("f"),
                    nrow=2,ncol=3)
  print(g)
  invisible(dev.off())
  
  fsal <- paste0("../figures/matrixes/SANDPROB3_",file_name,"_nexper_",numexper,"_IntMatrix.pdf")
  ggsave(fsal,g, width=7, height=4, units="in", scale=1)
}
