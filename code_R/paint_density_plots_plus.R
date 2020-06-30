# Density and boxplots of synthetic and empirical networks
#
# Author: Javier Garcia Algarra
#
# Invocation: Rscript paint_density_plots_plus year
#
# Example: Rscript paint_density_plots_plus 2005

rm(list=ls())
library(grid)
library(gridExtra)
library(ggplot2)
library(scales)
source("aux_functions_matrix.R")
source("parse_command_line_args.R")


# Third argument. If set to TRUE searches the best GOF in BestKS.txt
# else chooses experiment number 1
sbestKS <- FALSE 


#Our transformation function
scaleFUN <- function(x) sprintf("%.2f", x)

PaintDensPlot <- function(datos,titletext,xlabel,legendp="bottom")
{
  p <- ggplot() + geom_density(aes(x= cuenta, color = collection, fill = collection),  alpha = .1,
                           data=datos, position = "identity", adjust= 2)+
    xlab(paste(titletext,xlabel))+ylab("Density\n") + scale_fill_manual(values=c("blue","green4","red")) +
    scale_color_manual(values=c("blue","green4","red"))
    if (grepl("Degree",xlabel))
      p <- p + scale_x_log10(limits=c(0.1,1000))
    else
      p <- p + scale_x_log10(limits=c(10^-7,1))
    if (legendp == "top")
      lt <- element_blank()
    else
      lt <- element_text(size=10, face="bold")
    p <- p + scale_y_continuous(labels=scaleFUN)
    p <-p +facet_wrap(~type)+ theme_bw() +
    theme(panel.border = element_blank(),
          #legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), 
          legend.title = element_blank(),
          legend.position = legendp,
          legend.text = lt, 
          axis.line = element_line(colour = "black"),
          plot.title = element_text(lineheight=1.5, face="bold"),
          axis.text = element_text(face="bold", size=12),
          axis.title.x = element_text(face="bold", size=12),
          axis.title.y  = element_text(face="bold", size=12) )
    if (legendp == "top")
      p <- p + theme(legend.key = element_rect(size = 6, fill = "white", colour = "white"), legend.key.size = unit(0, "cm"))
    
  return(p)
}

PaintBoxPlot <- function(datos,titletext,xlabel)
{
  datos$collection <- gsub("GLOBAL","\nGLOBAL",datos$collection)
  p <- ggplot() + geom_boxplot(aes(x=as.factor(collection),y= cuenta, color = collection, fill = collection),  alpha = .1,
                               data=datos, position = "identity")+ scale_fill_manual(values=c("blue","green4","red")) +
                               scale_color_manual(values=c("blue","green4","red")) +
    xlab(paste("\n",titletext,xlabel))+ylab("Density\n")+
    scale_y_log10() 
  p <-p +facet_wrap(~type)
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
          plot.title = element_text(lineheight=1.5, face="bold"),
          axis.text = element_text(face="bold", size=11),
          axis.title.x = element_text(face="bold", size=12),
          axis.title.y  = element_text(face="bold", size=12) )
  
  return(p)
}

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0){
  print("Syntax: Rscript paint_ginis_region.R year")
  exit()
} else{
  year <- as.numeric(args[1])
}


ini_seq <- year
end_seq <- year


ImprovedFraction <- 2
BoostPercentage <- c(50,200)
Scope <- "GLOBAL"
ginis_detail <- read.csv("../results/ginis_detail_regions.txt", sep=";")
ginis_detail <- ginis_detail[ginis_detail$Region == "WORLD",]

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
  mean_vals_year <- read.csv(paste0("../results/ginis_means_regions_",year,".txt"), sep=";")
  file_name <- paste0("RedAdyCom",year,"_FILT")
  file_orig <- paste0("RedAdyCom",year)
  experiment_files <- Sys.glob(paste0("../results/",file_name,"_W_",posbest,".txt"))
  
  method <- paste0("BOOST_",(100-ImprovedFraction)/100,"_",BoostPercentage)
  if (Scope=="REGIONAL")
    method <- paste0(method,"_REGIONAL")
  filt_matrix <- read_and_remove_zeroes(paste0("../data/",file_name,".txt"))
  orig_matrix <- read_and_remove_zeroes(paste0("../data/",file_orig,".txt"))
  sim_matrix <- read_and_remove_zeroes(experiment_files[1])
  
  
  hm_filt <- crea_lista_heatmap(MPack(filt_matrix,normalize = FALSE),justcount = TRUE)
  hm_sim <- crea_lista_heatmap(MPack(sim_matrix,normalize = FALSE),justcount = TRUE)
  hm_orig <- crea_lista_heatmap(MPack(orig_matrix,normalize = FALSE),justcount = TRUE)

  
  for (BP in BoostPercentage){
    selgini <- ginis_detail[(ginis_detail$Method == method) & (ginis_detail$Year == year),]
    mean_val_gini <- mean_vals_year[ #(mean_vals_year$Year == year) &
                                      (mean_vals_year$Scope == Scope) &
                                      (mean_vals_year$TradeBoost == BP) ,]$Gini_import
                                      #(mean_vals_year$ImprovedFraction == ImprovedFraction),]$Gini_import
    diffgini <- abs(selgini$Gini_import - mean_val_gini)
    meanexper <- selgini[which(diffgini==min(diffgini)),][1,]
    posmeangini <- ginis_detail[(ginis_detail$Year==year) & 
                                  (ginis_detail$Method==method),]$Gini_import
    
    improved_matrix <- read_and_remove_zeroes(as.character(meanexper$File))
    if (!exists("hm_improved")){
      hm_improved <- crea_lista_heatmap(MPack(improved_matrix,normalize = FALSE),justcount = TRUE)
      hm_improved$collection <- paste0(meanexper$MethodLabel,"_",meanexper$TradeBoost," ",
                                       meanexper$ImprovedFraction,"% ",meanexper$Scope)
      
    }
    else{
      hm1_improved <- crea_lista_heatmap(MPack(improved_matrix,normalize = FALSE),justcount = TRUE)
      hm1_improved$collection <- paste0(meanexper$MethodLabel,"_",meanexper$TradeBoost," ",
                                       meanexper$ImprovedFraction,"% ",meanexper$Scope)
      
      hm_improved <- rbind(hm_improved,hm1_improved)    
    }
      
  }
  
  
  hm_filt$collection <- "Empirical (Filtered)"
  hm_sim$collection <- "Synthetic"
  hm_orig$collection <- "Original"
  
  hm_all_deg <- rbind(hm_sim,hm_improved)
  hm_all_importers_deg <- hm_all_deg[hm_all_deg$type=="IMP",]
  hm_all_exporters_deg <- hm_all_deg[hm_all_deg$type=="EXP",]
  hm_all_deg$collection <- as.factor(hm_all_deg$collection)
  hm_all_deg$type <- as.character(hm_all_deg$type)
  hm_all_deg[hm_all_deg$type=="IMP",]$type="Importers"
  hm_all_deg[hm_all_deg$type=="EXP",]$type="Exporters"
  hm_all_deg$type <- as.factor(hm_all_deg$type)
  
  
  hm_filt <- crea_lista_heatmap(MPack(filt_matrix,normalize = TRUE))
  hm_sim <- crea_lista_heatmap(MPack(sim_matrix,normalize = TRUE))
  hm_orig <- crea_lista_heatmap(MPack(orig_matrix,normalize = TRUE))
  hm_improved <- crea_lista_heatmap(MPack(improved_matrix,normalize = TRUE))
  hm_filt$collection <- "Empirical (Filtered)"
  hm_sim$collection <- "Synthetic"
  hm_orig$collection <- "Original"
  hm_improved$collection <- paste0(meanexper$MethodLabel,"_",meanexper$TradeBoost," ",
                                   meanexper$ImprovedFraction,"% ",meanexper$Scope)
  
  for (BP in BoostPercentage){
    selgini <- ginis_detail[(ginis_detail$Method == method) & (ginis_detail$Year == year),]
    mean_val_gini <- mean_vals_year[ (mean_vals_year$Scope == Scope) &
                                     (mean_vals_year$TradeBoost == BP) ,]$Gini_import
    diffgini <- abs(selgini$Gini_import - mean_val_gini)
    meanexper <- selgini[which(diffgini==min(diffgini)),][1,]
    posmeangini <- ginis_detail[(ginis_detail$Year==year) & 
                                  (ginis_detail$Method==method),]$Gini_import
    
    improved_matrix <- read_and_remove_zeroes(as.character(meanexper$File))
    if (!exists("hm_improved_w")){
      hm_improved_w <- crea_lista_heatmap(MPack(improved_matrix,normalize = TRUE))
      hm_improved_w$collection <- paste0(meanexper$MethodLabel,"_",meanexper$TradeBoost," ",
                                       meanexper$ImprovedFraction,"% ",meanexper$Scope)
      
    }
    else{
      hm1_improved_w <- crea_lista_heatmap(MPack(improved_matrix,normalize = TRUE))
      hm1_improved_w$collection <- paste0(meanexper$MethodLabel,"_",meanexper$TradeBoost," ",
                                        meanexper$ImprovedFraction,"% ",meanexper$Scope)
      
      hm_improved_w <- rbind(hm_improved_w,hm1_improved_w)    
    }
    
  }
  
  hm_all_weight <- rbind(hm_sim,hm_improved_w)
  hm_all_exporters_weight <- hm_all_weight[hm_all_weight$type=="EXP",]
  hm_all_importers_weight <- hm_all_weight[hm_all_weight$type=="IMP",]
  hm_all_weight$collection <- as.factor(hm_all_weight$collection)
  hm_all_weight$type <- as.character(hm_all_weight$type)
  hm_all_weight[hm_all_weight$type=="IMP",]$type="Importers"
  hm_all_weight[hm_all_weight$type=="EXP",]$type="Exporters"
  hm_all_weight$type <- as.character(hm_all_weight$type)
  hm_all_weight$collection <- ordered(hm_all_weight$collection,levels(as.factor(hm_all_deg$collection)))
  hm_all_weight$collection <- as.factor(paste0(" ",as.character(hm_all_weight$collection)," "))
  hm_all_deg$collection <- as.factor(paste0(" ",as.character(hm_all_deg$collection)," "))
  
  
  
  
  deg <- PaintDensPlot(hm_all_deg,"","Degree\n",legendp = "top")
  weight <- PaintDensPlot(hm_all_weight,"","Normalized strength", legendp = "bottom")
  ppi <- 300
  fsal <- paste0("../figures/densities/Density_DegStr_",year,"_",ImprovedFraction,"_",Scope,".png")
  png(fsal, width=10*ppi, height=8*ppi, res=ppi)
  grid.arrange(deg,weight, ncol=1, nrow=2,top=as.character(year) )
  dev.off()
  
  fsal <- paste0("../figures/densities/Density_DegStr_",year,"_",ImprovedFraction,"_",Scope,".eps")
  cairo_ps(filename = fsal,
           width = 10, height = 8, pointsize = 12,
           fallback_resolution = ppi)
  g <- grid.arrange(deg,weight, ncol=1, nrow=2,top="" )
  print(g)
  invisible(dev.off())
  
  
  bdeg <- PaintBoxPlot(hm_all_deg,"","Degree\n")
  bweight <- PaintBoxPlot(hm_all_weight,"","Normalized strength")
  ppi <- 300
  fsal <- paste0("../figures/densities/BoxPlot_DegStr_",year,"_",ImprovedFraction,"_",Scope,".png")
  png(fsal, width=10*ppi, height=8*ppi, res=ppi)
  grid.arrange(bdeg,bweight, ncol=1, nrow=2,top=as.character(year) )
  dev.off()
  
  
  
}

