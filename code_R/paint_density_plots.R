# Density and boxplots of synthetic and empirical networks
#
# Author: Javier Garcia Algarra
#
# Invocation: Rscript paint_density_plots iniseq finseq KSarg HOriginal
#                    iniseq : Initial year
#                    finseq : Final year
#                    KSarg: 1 Find best fitting synthetic experiment   0 Experiment 1
#                    HOriginal: FS Show Filtered and Synthetic (default)
#                               FOS (Filtered, Original, Synthetic)
#                               OS (Original, Synthetic)
#                               O  (Only Original)
#                               F  (Only Filtered)
#
# Example: Rscript paint_density_plots 1962 1976 1 FOS

library(grid)
library(gridExtra)
library(ggplot2)
source("aux_functions_matrix.R")
source("parse_command_line_args.R")


# Third argument. If set to TRUE searches the best GOF in BestKS.txt
# else chooses experiment number 1
sbestKS <- TRUE 

KSarg <- as.character(args[4])

if (!is.na(KSarg)){
  if (KSarg == 0)
    sbestKS <- FALSE
}

# Fourth command argument allows to hide original densities
HOriginal <- as.character(args[4])
if (is.na(HOriginal)){
  SFilt <- TRUE
  SSynth <- TRUE
  sOrig <- FALSE
  HOstr <- "FS"
} else{
  HOstr <- ""
  if (grepl("F",HOriginal)){
    SFilt <- TRUE
    HOstr <- paste0(HOstr,"F")
  }
  if (grepl("O",HOriginal)){
    SOrig <- TRUE
    HOstr <- paste0(HOstr,"O")
  }
  if (grepl("S",HOriginal)){
    SOrig <- TRUE
    HOstr <- paste0(HOstr,"S")
  }

}


PaintDensPlot <- function(datos,titletext,xlabel)
{
  p <- ggplot() + geom_density(aes(x= cuenta, color = collection, fill = collection),  alpha = .1,
                           data=datos, position = "identity", adjust= 2)+
    xlab(paste(titletext,xlabel))+ylab("Density\n")#+
    if (xlabel == "Degree")
      p <- p + scale_x_log10(limits=c(0.1,1000))
    else
      p <- p + scale_x_log10(limits=c(10^-7,1))
    if (HOstr == "FS"){
      p <- p + scale_fill_manual(values=c("blue","red","red"))+
        scale_color_manual(values=c("blue","red","red"))
    } 
    if (HOstr == "FOS")
    {
      p <- p + scale_fill_manual(values=c("blue","orange","red"))+
      scale_color_manual(values=c("blue","orange","red"))
    }
    if (HOstr == "FO")
    {
      p <- p + scale_fill_manual(values=c("blue","orange","orange"))+
        scale_color_manual(values=c("blue","orange","orange"))
    }
    if (HOstr == "F")
    {
      p <- p + scale_fill_manual(values=c("red","red","red"))+
        scale_color_manual(values=c("red","red","red"))
    }
    if (HOstr == "O")
    {
      p <- p + scale_fill_manual(values=c("orange","orange","orange"))+
        scale_color_manual(values=c("orange","orange","orange"))
    }
    p <-p +theme_bw() +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), 
          legend.title = element_blank(),
          legend.position = "top",
          legend.text = element_text(size=10, face="bold"),
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
    xlab(paste("\n",titletext,xlabel))+ylab("Density\n")+
    scale_y_log10()
  if (HOstr == "FS"){
    p <- p + scale_fill_manual(values=c("blue","red","red"))+
      scale_color_manual(values=c("blue","red","red"))
  } 
  if (HOstr == "FOS")
  {
    p <- p + scale_fill_manual(values=c("blue","orange","red"))+
      scale_color_manual(values=c("blue","orange","red"))
  }
  if (HOstr == "FO")
  {
    p <- p + scale_fill_manual(values=c("blue","orange","orange"))+
      scale_color_manual(values=c("blue","orange","orange"))
  }
  if (HOstr == "F")
  {
    p <- p + scale_fill_manual(values=c("red","red","red"))+
      scale_color_manual(values=c("red","red","red"))
  }
  if (HOstr == "O")
  {
    p <- p + scale_fill_manual(values=c("orange","orange","orange"))+
      scale_color_manual(values=c("orange","orange","orange"))
  }
    p <- p + theme_bw() +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), 
          legend.title = element_blank(),
          legend.position = "none",
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

if (sbestKS)
  bestKS <- read.table("../results/BestKS.txt",header=TRUE)

for (year in anyos){
  if (sbestKS)
    posbest <- bestKS[bestKS$Year==year,]$Experiment
  else
    posbest <- 1
  if (length(posbest)== 0)
    posbest <- 1
  file_name <- paste0("RedAdyCom",year,"_FILT")
  file_orig <- paste0("RedAdyCom",year)
  experiment_files <- Sys.glob(paste0("../results/",file_name,"_W_",posbest,".txt"))
  filt_matrix <- read_and_remove_zeroes(paste0("../data/",file_name,".txt"))
  orig_matrix <- read_and_remove_zeroes(paste0("../data/",file_orig,".txt"))
  sim_matrix <- read_and_remove_zeroes(experiment_files[1])
  hm_filt <- crea_lista_heatmap(MPack(filt_matrix,normalize = FALSE),justcount = TRUE)
  hm_sim <- crea_lista_heatmap(MPack(sim_matrix,normalize = FALSE),justcount = TRUE)
  hm_orig <- crea_lista_heatmap(MPack(orig_matrix,normalize = FALSE),justcount = TRUE)
  hm_filt$collection <- "Empirical (Filtered) "
  hm_sim$collection <- "Synthetic  "
  hm_orig$collection <- "Original  "
  if (HOstr == "FS")
    hm_all_deg <- rbind(hm_filt,hm_sim)
  if (HOstr == "FOS")
    hm_all_deg <- rbind(hm_filt,hm_sim,hm_orig)
  if (HOstr == "FO")
    hm_all_deg <- rbind(hm_filt,hm_orig)
  if (HOstr == "O")
    hm_all_deg <- rbind(hm_orig)
  if (HOstr == "F")
    hm_all_deg <- rbind(hm_filt)
  hm_all_importers_deg <- hm_all_deg[hm_all_deg$type=="IMP",]
  hm_all_exporters_deg <- hm_all_deg[hm_all_deg$type=="EXP",]
  hm_filt <- crea_lista_heatmap(MPack(filt_matrix,normalize = TRUE))
  hm_sim <- crea_lista_heatmap(MPack(sim_matrix,normalize = TRUE))
  hm_orig <- crea_lista_heatmap(MPack(orig_matrix,normalize = TRUE))
  hm_filt$collection <- " Empirical (Filtered)"
  hm_sim$collection <- " Synthetic  "
  hm_orig$collection <- " Original  "
  if (HOstr == "FS")
    hm_all_weight <- rbind(hm_filt,hm_sim)
  if (HOstr == "FOS")
    hm_all_weight <- rbind(hm_filt,hm_sim,hm_orig)
  if (HOstr == "FO")
    hm_all_weight <- rbind(hm_filt,hm_orig)
  if (HOstr == "O")
    hm_all_weight <- rbind(hm_orig)
  if (HOstr == "F")
    hm_all_weight <- rbind(hm_filt)
  hm_all_exporters_weight <- hm_all_weight[hm_all_weight$type=="EXP",]
  hm_all_importers_weight <- hm_all_weight[hm_all_weight$type=="IMP",]
  
  print(paste("Exporters:",nrow(filt_matrix),"Importers",ncol(filt_matrix)))
  
  q <- PaintDensPlot(hm_all_importers_deg,"Importers","Degree")
  r <- PaintDensPlot(hm_all_importers_weight,"Importers","Normalized Strength")
  s <- PaintDensPlot(hm_all_exporters_deg,"Exporters","Degree")
  t <- PaintDensPlot(hm_all_exporters_weight,"Exporters","Normalized Strength")
  
  ppi <- 300
  bq <- PaintBoxPlot(hm_all_importers_deg,"Importers","Degree")
  br <- PaintBoxPlot(hm_all_importers_weight,"Importers","Normalized Strength")
  bs <- PaintBoxPlot(hm_all_exporters_deg,"Exporters","Degree")
  bt <- PaintBoxPlot(hm_all_exporters_weight,"Exporters","Normalized Strength")
  dir.create("../figures/densities/", showWarnings = FALSE)
  fsal <- paste0("../figures/densities/Density_DegStr_",year,"_",HOstr,".png")
  png(fsal, width=10*ppi, height=8*ppi, res=ppi)
  grid.arrange(q,r,s,t, ncol=2, nrow=2,top=year )
  dev.off()
  fsal2 <- paste0("../figures/densities/Boxplot_DegStr_",year,"_",HOstr,".png")
  png(fsal2, width=10*ppi, height=8*ppi, res=ppi)
  grid.arrange(bq,br,bs,bt, ncol=2, nrow=2 )
  dev.off()

  
}

