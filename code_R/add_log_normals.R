library(grid)
library(gridExtra)
library(ggplot2)

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

PaintHist <- function(serie)
{
  auxdf <- serie[serie$cuenta > 0,]
  dist_deg <- ggplot(data = auxdf) + 
    geom_histogram(aes(x= cuenta, color = collection, fill = collection),  alpha = .1,
                   data=auxdf, position = "identity")+
    #scale_y_log10(breaks=c(0.01,0.2,0.5,1.0)) +
    #scale_x_log10(breaks = seq_breaks) + scale_y_log10(breaks=c(0.1,0.2,0.5,1.0)) + 
    scale_x_log10()+scale_y_log10()+
    xlab("Degree") + 
    #ylab(cumulativetxt) + ggtitle("") +  scale_shape_manual(values=c(21, 15)) +
    scale_alpha(guide = 'none') +  scale_size_identity() +# ggtitle(series) +
    theme_bw() +
    theme(
      axis.title.x = element_text(color="grey30", size=15),
      axis.title.y = element_text(color="grey30", size=15),
      legend.title=element_blank(),
      legend.text=element_text(size=14),
      axis.text.x = element_text(face="bold", color="grey30", size=13),
      axis.text.y = element_text(face="bold", color="grey30", size=13)
    )
  return(dist_deg)
}

MSimp <- function(matrix,normalize = TRUE)
{
  sum_row <- rep(0,nrow(matrix))
  sum_col <- rep(0,ncol(matrix))
  if (normalize)
    matrix = matrix/sum(matrix)
    #matrix = matrix
  sum_row <- rowSums(matrix)
  sum_col <- colSums(matrix)
  matrix <- matrix[sum_row>0,]
  matrix <- matrix[,sum_col>0]
  return(t(matrix))       # Transpose because of order of python-written matrix
}

PaintDensPlot <- function(datos,titletext,xlabel)
{
  p <- ggplot() + geom_density(aes(x= cuenta, color = collection, fill = collection),  alpha = .1,
                               data=datos, position = "identity", adjust=2)+ 
    xlab(xlabel)+ylab("Count\n")+
    ggtitle(titletext)+ scale_x_log10()+
    scale_fill_manual(values=c("blue","white","red","green"))+
    scale_color_manual(values=c("blue","grey","red","green"))+
    theme_bw() +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), 
          legend.title = element_blank(),
          legend.text = element_text(size=12, face="bold"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(lineheight=.8, face="bold"),
          axis.text = element_text(face="bold", size=13),
          axis.title.x = element_text(face="bold", size=13),
          axis.title.y  = element_text(face="bold", size=13) )
  
  return(p)
}


PaintBoxPlot <- function(datos,titletext,xlabel)
{
  p <- ggplot() + geom_boxplot(aes(x=as.factor(collection),y= cuenta, color = collection, fill = collection),  alpha = .1,
                               data=datos, position = "identity")+ 
    xlab(xlabel)+ylab("Count\n")+
    ggtitle(titletext)+ scale_y_log10()+
    scale_fill_manual(values=c("blue","white","red"))+
    scale_color_manual(values=c("blue","grey","red"))+
    theme_bw() +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), 
          legend.title = element_blank(),
          legend.text = element_text(size=12, face="bold"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(lineheight=.8, face="bold"),
          axis.text = element_text(face="bold", size=13),
          axis.title.x = element_text(face="bold", size=13),
          axis.title.y  = element_text(face="bold", size=13) )
  
  return(p)
}

source("parse_command_line_args.R")

anyos <- seq(ini_seq,end_seq)


anyos <- seq(1980,1980)
sbestlillies <- FALSE        # If set to TRUE searches the best GOF in BestLillies.txt
# else chooses experiment number 1
#bestlillies <- read.table("../results/BestLillies.txt",header=TRUE)
for (year in anyos){
  if (sbestlillies)
    posbest <- bestlillies[bestlillies$Year==year,]$Experiment
  else
    posbest <- 1
  file_name <- paste0("RedAdyCom",year,"_FILT")
  file_orig <- paste0("RedAdyCom",year)
  filt_matrix <- read.table(paste0("../data/",file_name,".txt"),sep="\t")
  orig_matrix <- read.table(paste0("../data/",file_orig,".txt"),sep="\t")
  hm_filt <- crea_lista_heatmap(MSimp(filt_matrix,normalize = FALSE),justcount = TRUE)
  experiment_files <- Sys.glob(paste0("../results/",file_name,"_W_*.txt"))
  numexper <- 1
  for (j in experiment_files){
    print(paste("Numexper",numexper))
    sim_matrix <- read.table(j,sep="\t")
    
    hm_sim <- crea_lista_heatmap(MSimp(sim_matrix,normalize = FALSE),justcount = TRUE)
    hm_orig <- crea_lista_heatmap(MSimp(orig_matrix,normalize = FALSE),justcount = TRUE)
    hm_filt$collection <- "Filtered"
    hm_sim$collection <- "Synthetic"
    hm_orig$collection <- "Original"
    hm_all_deg <- rbind(hm_filt,hm_sim,hm_orig)
    hm_all_importers_deg <- hm_all_deg[hm_all_deg$type=="IMP",]
    hm_filt <- crea_lista_heatmap(MSimp(filt_matrix,normalize = TRUE))
    hm_sim <- crea_lista_heatmap(MSimp(sim_matrix,normalize = TRUE))
    hm_orig <- crea_lista_heatmap(MSimp(orig_matrix,normalize = TRUE))
    hm_filt$collection <- "Filtered"
    hm_sim$collection <- "Synthetic"
    hm_orig$collection <- "Original"
    #hm_all_weight <- rbind(hm_filt,hm_sim,hm_orig)
    hm_all_weight <- rbind(hm_filt,hm_orig)
    hm_all_importers_weight <- hm_all_weight[hm_all_weight$type=="IMP",]

    hm_all_exporters_deg <- hm_all_deg[hm_all_deg$type=="EXP",]
    hm_filt <- crea_lista_heatmap(MSimp(filt_matrix,normalize = TRUE))
    hm_sim <- crea_lista_heatmap(MSimp(sim_matrix,normalize = TRUE))
    hm_orig <- crea_lista_heatmap(MSimp(orig_matrix,normalize = TRUE))
    hm_filt$collection <- "Filtered"
    hm_sim$collection <- "Synthetic"
    hm_orig$collection <- "Original"
    #hm_all_weight <- rbind(hm_filt,hm_sim,hm_orig)
    
    hm_all_weight <- rbind(hm_filt,hm_orig)
    
    hm_all_exporters_weight <- hm_all_weight[hm_all_weight$type=="EXP",]
    hm_sim_exp_cuenta <- hm_sim[(hm_sim$collection=="Synthetic") & (hm_sim$type=="EXP"),]$cuenta
    hm_sim_imp_cuenta <- hm_sim[(hm_sim$collection=="Synthetic") & (hm_sim$type=="IMP"),]$cuenta
    if (numexper == 1){
      hm_sim_exporters_weight_log <- log10(hm_sim_exp_cuenta)
      hm_sim_importers_weight_log <- log10(hm_sim_imp_cuenta)
    }
    else{
      hm_sim_exporters_weight_log <- c(hm_sim_exporters_weight_log,log10(hm_sim_exp_cuenta))
      hm_sim_importers_weight_log <- c(hm_sim_importers_weight_log,log10(hm_sim_imp_cuenta))
    }
    numexper <- numexper + 1
  }
  hm_sim_avg_exporters_weight <- 10^hm_sim_exporters_weight_log
  veces <- length(hm_sim_avg_exporters_weight)
  df <- data.frame("N"=seq(1,veces),"cuenta"=rep(0,veces),"type"=rep("EXP",veces),"collection"=rep("Synthetic_Avg",veces))
  zeros <- rep(0,length(hm_sim_avg_exporters_weight))
  df$cuenta <- zeros
  for (k in seq(1:length(hm_sim_avg_exporters_weight))){
    df$cuenta[k] <- hm_sim_avg_exporters_weight[k]
  }
  hm_all_exporters_weight <- rbind(hm_all_exporters_weight,df)
  
  hm_sim_avg_importers_weight <- 10^hm_sim_importers_weight_log
  veces <- length(hm_sim_avg_importers_weight)
  df <- data.frame("N"=seq(1,veces),"cuenta"=rep(0,veces),"type"=rep("IMP",veces),"collection"=rep("Synthetic_Avg",veces))
  zeros <- rep(0,length(hm_sim_avg_importers_weight))
  df$cuenta <- zeros
  for (k in seq(1:length(hm_sim_avg_importers_weight))){
    df$cuenta[k] <- hm_sim_avg_importers_weight[k]
  }
  hm_all_importers_weight <- rbind(hm_all_importers_weight,df)
  
  r <- PaintDensPlot(hm_all_importers_weight,"Importers","Normalized strength")
  t <- PaintDensPlot(hm_all_exporters_weight,"Exporters","Normalized strength")
  
  br <- PaintBoxPlot(hm_all_importers_weight,"Importers","Normalized strength")
  bt <- PaintBoxPlot(hm_all_exporters_weight,"Exporters","Normalized strength")
  dir.create("../figures/densities/", showWarnings = FALSE)
  fsal <- paste0("../figures/densities/Density_DegStr_Average_",year,".png")
  ppi <- 600
  png(fsal, width=12*ppi, height=3*ppi, res=ppi)
  grid.arrange(r,t, ncol=2, nrow=1,top=year )
  dev.off()
  fsal2 <- paste0("../figures/densities/Boxplot_DegStr_Average_",year,".png")
  ppi <- 600
  png(fsal2, width=12*ppi, height=3*ppi, res=ppi)
  grid.arrange(br,bt, ncol=2, nrow =1,top=year )
  dev.off()
  
  
}
