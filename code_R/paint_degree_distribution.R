library("grid")
library("gridExtra")
library("igraph")
library("ggplot2")
source("aux_functions_matrix.R")
source("parse_command_line_args.R")


gen_deg_distribution <- function(red,series, colors, seq_breaks = c(1,5,10,20,50,100))
{
  
  gen_deg_data_frame <- function(input_matrix,tipo,tamanyo,nalpha,serie)
  {
    
    # Remove all zeroes columns and rows
    dfint <- as.data.frame(input_matrix)
    write.csv(dfint,"dfcab.csv")
    dr <- lread_network("dfcab.csv", guild_astr = "Exporter", guild_bstr = "Importer", directory="")
    grafo <- as.undirected(dr[["graph"]])
    ddegree <- igraph::degree(grafo,mode = c("out"), loops = TRUE, normalized = FALSE)
    dfdeg <- data.frame("degree" = as.numeric(ddegree))
    dfdeg$type <- "Exporter"
    dfdeg$tamanyo <- tamanyo
    dfdeg$nalpha <- nalpha
    dfdeg$nodename <- names(ddegree)
    dfdeg[grepl("Importer",dfdeg$nodename),]$type <- as.character("Importer")
    dfdeg <- dfdeg[order(dfdeg$degree),]
    dfdeg$weight <- 0
    for (k in 1:nrow(dfdeg)){
      if (dfdeg$type[k] == "Importer")
      {
        indice <- as.numeric(strsplit(dfdeg$nodename[k],"Importer")[[1]][2])
        dfdeg$weight[k] <- sum(as.numeric(dr$m[indice,]))
      }
      else
      {
        indice <- as.numeric(strsplit(dfdeg$nodename[k],"Exporter")[[1]][2])
        dfdeg$weight[k] <- sum(as.numeric(dr$m[,indice]))
      }
    }
    dfdeg$weight <- dfdeg$weight/sum(as.numeric(dfdeg$weight))
    ddeg_exporter <- dfdeg[dfdeg$type == "Exporter",]
    occur <- ddeg_exporter$degree
    woccur <- ddeg_exporter$weight
    woccur <- woccur[order(woccur)]
    alpha_level = 0.5
    p = occur/sum(occur)
    dy = rev(cumsum(rev(p)))
    dx = occur
    wp = woccur/sum(woccur)
    wdy = rev(cumsum(rev(wp)))
    wdx = woccur
    type = ddeg_exporter$type
    tamanyo = ddeg_exporter$tamanyo
    tamanyo = 1.2
    nalpha = ddeg_exporter$nalpha
    auxdf_exporter <- data.frame(dx,dy,type,tamanyo,nalpha)
    auxdfw_exporter <- data.frame(wdx,wdy,type,tamanyo,nalpha)
    
    ddeg_importer <- dfdeg[dfdeg$type == "Importer",]
    occur <- ddeg_importer$degree
    
    woccur <- ddeg_importer$weight
    
    woccur <- woccur[order(woccur)]
    alpha_level = 0.5
    p = occur/sum(occur)
    dy = rev(cumsum(rev(p)))
    dx = occur
    wp = woccur/sum(woccur)
    wdy = rev(cumsum(rev(wp)))
    wdx = woccur
    type = ddeg_importer$type
    tamanyo = ddeg_importer$tamanyo
    tamanyo = 1.2
    nalpha = ddeg_importer$nalpha
    auxdf_importer <- data.frame(dx,dy,type,tamanyo,nalpha)
    auxdfw_importer <- data.frame(wdx,wdy,type,tamanyo,nalpha)
    if (serie == "Exporter"){
      auxdf <- auxdf_exporter
      auxdfw <- auxdfw_exporter
    }
    if (serie == "Importer"){
      auxdf <- auxdf_importer
      auxdfw <- auxdfw_importer
    }
    if (serie == "Both"){
      auxdf <- rbind(auxdf_exporter, auxdf_importer)
      auxdfw <- rbind(auxdfw_exporter, auxdfw_importer)
    }
    auxdf$method <- tipo
    auxdfw$method <- tipo
    calc_values <- list("auxdf" = auxdf, "auxdfw" = auxdfw)
    return(calc_values)
  }
  dred <- gsub(TFstring,"",red)
  emp_matrix <- read.table(paste0("../data/",dred,".txt"),sep="\t")
  auxdf_emp <- gen_deg_data_frame(emp_matrix,"Empirical",1,0.2,series)
  auxdf <- auxdf_emp[["auxdf"]]
  auxdfw <- auxdf_emp[["auxdfw"]]
  if (TFstring == ""){
    subdir <- ""
    ficheros <- Sys.glob(paste0("../results/",subdir,red,"_W_*",".txt"))
  } else {
    subdir <- "TFMatrix/"
    ficheros <- Sys.glob(paste0("../results/",subdir,red,"_W_1",".txt"))
  }
  for (j in ficheros){
    sim_matrix <- read.table(j,sep="\t")
    auxdf_sim <- gen_deg_data_frame(sim_matrix,"Simulated",0.5,0.02,series)
    auxdf <- rbind(auxdf,auxdf_sim[["auxdf"]])
    auxdfw <- rbind(auxdfw,auxdf_sim[["auxdfw"]])
  }
  
  auxdf <- auxdf[auxdf$dx > 0,]
  dist_deg <- ggplot(data = auxdf, aes(x = dx, y = dy)) + 
    geom_point(aes(alpha = nalpha,shape=method,size=tamanyo,stroke=tamanyo),color=colors) +
    scale_x_log10(breaks = seq_breaks) + scale_y_log10(breaks=c(0.1,0.2,0.5,1.0)) + 
    xlab(paste(year,series,"Degree")) + 
    ylab(cumulativetxt) + scale_shape_manual(values=c(21,16)) +
    scale_alpha(guide = 'none') + scale_size_identity()  +
    theme_bw() +
    theme(
      axis.title.x = element_text(color="grey30", size = 11),
      axis.title.y = element_text(color="grey30", size= 11),
      legend.title=element_blank(),
      legend.position = "top",
      legend.text=element_text(size=10),
      axis.text.x = element_text(face="bold", color="grey30", size=10),
      axis.text.y = element_text(face="bold", color="grey30", size=10)
    )
  
  auxdfw <- auxdfw[auxdfw$wdx > 0,]
  dist_wdeg <- ggplot(data = auxdfw, aes(x = wdx, y = wdy)) + 
    geom_point(aes(alpha = nalpha,shape=method,size=tamanyo,stroke=tamanyo),color=colors) +
    scale_x_log10(breaks=c(0.001,0.01,0.1,1)) + scale_y_log10(breaks=c(0.1,0.2,0.5,1.0)) + 
    xlab(paste(year,series,"Normalized Strength")) + 
    ylab(cumulativetxt) + scale_shape_manual(values=c(21,16)) +
    scale_alpha(guide = 'none') +  scale_size_identity() + 
    theme_bw() +
    theme(
      axis.title.x = element_text(color="grey30", size = 11),
      axis.title.y = element_text(color="grey30", size= 11),
      legend.title=element_blank(),
      legend.position = "top",
      legend.text=element_text(size=10),
      axis.text.x = element_text(face="bold", color="grey30", size=10),
      axis.text.y = element_text(face="bold", color="grey30", size=10)
    )
  calc_values <- list("dist_deg" = dist_deg, "dist_wdeg" = dist_wdeg)
  return(calc_values)
}


languageEl <- "EN"
if (languageEl == "EN"){
  cumulativetxt = "Cumulative distribution probability"
  xscale = "degree scale"
} else {
  cumulativetxt = "Distribuci?n acumulada de probabilidad"
  xscale = "escala degree"
}


# Third command argument allows to plot densities at build up time
TFstring <- as.character(args[3])
if (is.na(TFstring)){
  TFstring <- ""
} else
  TFstring <- "TF_"

files <- paste0(TFstring,"RedAdyCom",seq(ini_seq,end_seq))
for (orig_file in files)
{
  red <- paste0(orig_file,"_FILT")
  series = "Exporter"
  year=gsub("_FILT","",strsplit(red,"RedAdyCom")[[1]][-1])
  grafs <- gen_deg_distribution(paste0(red),series,"blue")
  e_degree <- grafs$dist_deg
  e_weight <- grafs$dist_wdeg
  series = "Importer"
  grafs <- gen_deg_distribution(paste0(red),series,"red")
  i_degree <- grafs$dist_deg
  i_weight <- grafs$dist_wdeg
  ppi <- 300
  dir.create("../figures/degdistributions/", showWarnings = FALSE)
  png(paste0("../figures/degdistributions/ALLdist_",red,"_",languageEl,".png"), width=(8*ppi), height=8*ppi, res=ppi)
  grid.arrange(i_degree,i_weight, e_degree,e_weight,ncol=2, nrow=2)
  dev.off()
}