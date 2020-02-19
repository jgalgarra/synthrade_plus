library("grid")
library("gridExtra")
library("igraph")
library("ggplot2")
library("poweRlaw")
source("aux_functions_matrix.R")
source("parse_command_line_args.R")

estimate_powerlaw <- function(datosfit)
{
  m_bs = conpl$new(datosfit$strength)
  est = estimate_xmin(m_bs)
  m_bs$setXmin(est)
  plot(m_bs)
  print(paste("pendiente strength",m_bs$pars))
  lines(m_bs, col=2, lwd=2)
  
  m_bd = conpl$new(datosfit$degree)
  est = estimate_xmin(m_bd)
  m_bd$setXmin(est)
  plot(m_bd)
  print(paste("pendiente degree",m_bd$pars))
  lines(m_bd, col=3, lwd=2)
  
}

calc_accum <- function(datosinput)
{
  #datosacc <- datosinput[order(datosinput$weight),]
  datosacc <- datosinput[order(datosinput$degree),]
  datosacc$ac_degree <- 0
  datosacc$ac_strength <- 0
  datosacc$ac_degree[1] <- datosacc$degree[1]
  datosacc$ac_strength[1] <- datosacc$weight[1]
  for (i in 2:nrow(datosacc)){
    datosacc$ac_degree[i] <- datosacc$ac_degree[i-1]+datosacc$degree[i]
    datosacc$ac_strength[i] <- datosacc$ac_strength[i-1]+datosacc$weight[i]
  }
  datosacc$ac_strength <- datosacc$ac_strength/max(datosacc$ac_strength)
  return(datosacc)
}

gen_links_strength_distribution <- function(red,series, colors, seq_breaks = c(1,5,10,20,50,100), empirical = FALSE)
{
  gen_ls_data_frame <- function(input_matrix,tipo,tamanyo,nalpha,serie,titlestr)
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
    dfdeg$weight <- dfdeg$weight /max(as.numeric(dfdeg$weight))
    
    dfaccum <- calc_accum(dfdeg) 
    ddeg_exporter <- dfaccum[dfaccum$type == "Exporter",]
    
    degree <- ddeg_exporter$degree
    weight <- ddeg_exporter$weight
    ac_strength <- ddeg_exporter$ac_strength
    ac_degree <- ddeg_exporter$ac_degree
    datosplot <- data.frame("degree" = degree, "strength" = weight, 
                            "ac_strength" = ac_strength, "ac_degree" = ac_degree)
    dpexp <- datosplot
    
    mod <- lm(datosplot$strength ~ datosplot$degree)
    etmodel <- sprintf("log10 s = %.4f log10 d %.4f     Adj. R^2 = %0.3f",
                       as.numeric(mod[[1]][2]),as.numeric(mod[[1]][1]),summary(mod)$adj.r.squared)
    exptf <- ggplot(datosplot,aes(x=degree,y=strength))+geom_point(color="blue",alpha=0.5)+scale_x_log10()+scale_y_log10()+
      ggtitle(paste0("Exporters at ",titlestr))+ 
      geom_smooth(method = "lm", se = FALSE, show.legend = TRUE,color="grey50",linetype = "dashed")+
      geom_text(x=1, y=0,label=etmodel, size = 5)+xlab("Degree")+ylab("Normalized strength")+
      theme_bw() +  theme(plot.title = element_text(hjust = 0.5, size = 18),
                          axis.title.x = element_text(color="grey30", size = 15, face="bold"),
                          axis.title.y = element_text(color="grey30", size= 15, face="bold"),
                          legend.title=element_blank(),
                          legend.position = "top",
                          legend.text=element_text(size=10),
                          panel.grid.minor = element_blank(),
                          axis.text.x = element_text(face="bold", color="grey30", size=14),
                          axis.text.y = element_text(face="bold", color="grey30", size=14)
      )

    dfaccum <- calc_accum(dfdeg) 
    #ddeg_importer <- dfdeg[dfdeg$type == "Importer",]
    ddeg_importer <- dfaccum[dfaccum$type == "Importer",]
    degree <- ddeg_importer$degree
    weight <- ddeg_importer$weight
    ac_strength <- ddeg_importer$ac_strength
    ac_degree <- ddeg_importer$ac_degree
    datosplot <- data.frame("degree" = degree, "strength" = weight, 
                            "ac_strength" = ac_strength, "ac_degree" = ac_degree)
    dpimp <- datosplot
    mod <- lm(datosplot$strength ~ datosplot$degree)
    etmodel <- sprintf("log10 s = %.4f log10 d %.4f     Adj. R^2 = %0.3f",as.numeric(mod[[1]][2]),as.numeric(mod[[1]][1]),summary(mod)$adj.r.squared)
    
    imptf <- ggplot(datosplot,aes(x=degree,y=strength))+geom_point(colour="red",alpha=0.5)+scale_x_log10()+scale_y_log10()+
         ggtitle(paste0("Importers at ",titlestr))+
         geom_smooth(method = "lm", se = FALSE, show.legend = TRUE,color="grey50",linetype = "dashed")+
         geom_text(x=1, y=max(log10(datosplot$strength)),label=etmodel, size = 5)+xlab("Degree")+ylab("Normalized strength")+
      theme_bw() +  theme(plot.title = element_text(hjust = 0.5, size = 18),
                          axis.title.x = element_text(color="grey30", size = 15, face="bold"),
                          axis.title.y = element_text(color="grey30", size= 15, face="bold"),
                          legend.title=element_blank(),
                          legend.position = "top",
                          legend.text=element_text(size=10),
                          panel.grid.minor = element_blank(),
                          axis.text.x = element_text(face="bold", color="grey30", size=14),
                          axis.text.y = element_text(face="bold", color="grey30", size=14)
      )


    calc_values <- list("imptf" = imptf, "exptf" = exptf, "data_exp" = dpexp, "data_imp" = dpimp)
    return(calc_values)
  }
  
  experiment <- 1
  sbestKS <- TRUE
  if (sbestKS)
    bestKS <- read.table("../results/BestKS.txt",header=TRUE)
  
  if (!empirical)
  {
    if (sbestKS)
      experiment<- bestKS[bestKS$Year==year,]$Experiment
    dred <- gsub(TFstring,"",red)
    subdir <- "TFMatrix/"
    ficheros <- Sys.glob(paste0("../results/",subdir,red,"_W_",experiment,".txt"))
    for (j in ficheros){
      sim_matrix <- read.table(j,sep="\t")
      plots_TF <- gen_ls_data_frame(sim_matrix,"Simulated",0.5,0.02,series,"TF")
    }
    subdir <- ""
    ficheros <- Sys.glob(gsub("TF_","",paste0("../results/",subdir,red,"_W_",experiment,".txt")))
    for (j in ficheros){
      sim_matrix <- read.table(j,sep="\t")
      plots_final <- gen_ls_data_frame(sim_matrix,"Simulated",0.5,0.02,series,"TT")
    }
  }
  
  else
  {
    dred <- gsub(TFstring,"",red)
    subdir <- "data/"
    ficheros <- Sys.glob(paste0("../",subdir,dred,".txt"))
    for (j in ficheros){
      sim_matrix <- read.table(j,sep="\t")
      plots_final <- gen_ls_data_frame(sim_matrix,"Empirical",0.5,0.02,series,"TT")
      plots_TF <- plots_final
    }

  }
  
  calc_values <- list("plots_TF" = plots_TF, "plots_final" = plots_final)
  return(calc_values)

}

plot_sq_fit <- function(datosplot,titlestr="",dcol="red")
{
  
  datatrf <- datosplot
  datatrf$log10_degree <- log10(datosplot$degree)^2
  datatrf$log10_strength <- log10(datosplot$strength)
  mod <- lm(datatrf$log10_strength ~ datatrf$log10_degree)
  minx <- min(sqrt(datatrf$log10_degree))
  maxx <- round(max(sqrt(datatrf$log10_degree)))
  
  etmodel <- sprintf("log10 s = %.3f (log10 d)^2 %.3f     Adj. R^2 = %0.2f",
                     as.numeric(mod[[1]][2]),as.numeric(mod[[1]][1]),summary(mod)$adj.r.squared)
  imptf <- ggplot(datatrf,aes(x=log10_degree,y=log10_strength))+geom_point(color=dcol,alpha=0.5)+
    ggtitle(titlestr)+xlab("Degree")+ylab("Normalized strength")+
    scale_x_continuous(breaks=c(0,1,4),labels=c(1,10,100))+
    scale_y_continuous(breaks=c(0,-2,-4),labels=c("1","1e-02","1e-04"))+
    geom_smooth(method = "lm", se = FALSE, show.legend = TRUE,color="grey50",linetype = "dashed")+
    geom_text(x=2, y=min(datatrf$log10_strength),label=etmodel, size = 5)+
    theme_bw() +  theme(plot.title = element_text(hjust = 0.5, size = 18),
                        axis.title.x = element_text(color="grey30", size = 15, face="bold"),
                        axis.title.y = element_text(color="grey30", size= 15, face="bold"),
                        legend.title=element_blank(),
                        legend.position = "top",
                        legend.text=element_text(size=10),
                        panel.grid.minor = element_blank(),
                        axis.text.x = element_text(face="bold", color="grey30", size=14),
                        axis.text.y = element_text(face="bold", color="grey30", size=14)
    )
  return(imptf)
}


plot_log_fit <- function(datosplot,titlestr="",dcol="red")
{
  
  datatrf <- datosplot
  datatrf$log10_acdegree <- log10(datatrf$ac_degree)
  datatrf$log10_acstrength <- log10(datatrf$ac_strength)
  # datosfit <- datatrf[(datatrf$log10_acstrength< quantile(datatrf$log10_acstrength,probs=c(0.7))) &
  #                       (datatrf$log10_acstrength> quantile(datatrf$log10_acstrength,probs=c(0.1)))  ,]
  datosfit <- datatrf[(datatrf$log10_acstrength< quantile(datatrf$log10_acstrength,probs=c(0.6))),]
  mod <- lm(datosfit$log10_acstrength ~ datosfit$log10_acdegree)
  beta <- mod[[1]][1]
  alpha <- mod[[1]][2]
  xmin <- min(datosfit$log10_acdegree)
  ymin <- alpha*xmin+beta
  xmax <- max(datosfit$log10_acdegree)
  ymax <- alpha*xmax+beta

  etmodel <- sprintf("log(Cs) = %.2f log(Cd) %.2f Adj. R^2 = %0.3f",as.numeric(mod[[1]][2]),as.numeric(mod[[1]][1]),summary(mod)$adj.r.squared)
  imptf <- ggplot(datatrf,aes(x=ac_degree,y=ac_strength))+geom_point(color=dcol,alpha=0.5)+
    ggtitle(titlestr)+xlab("Cumulative Degree")+ylab("Cumulative Normalized strength")+
    scale_x_log10()+scale_y_log10()+
    geom_text(x=quantile(datatrf$log10_acdegree,probs=c(0.02)), 
              y=min(datatrf$log10_acstrength),label=etmodel, size = 5, hjust=0)+
    geom_text(x=xmax,y=ymax,label="*")+
    geom_abline(slope = alpha, intercept = beta, color = "black", alpha = 0.5, linetype = 2) +
    theme_bw() +  theme(plot.title = element_text(hjust = 0.5, size = 18),
                        axis.title.x = element_text(color="grey30", size = 15, face="bold"),
                        axis.title.y = element_text(color="grey30", size= 15, face="bold"),
                        legend.title=element_blank(),
                        legend.position = "top",
                        legend.text=element_text(size=10),
                        panel.grid.minor = element_blank(),
                        axis.text.x = element_text(face="bold", color="grey30", size=14),
                        axis.text.y = element_text(face="bold", color="grey30", size=14)
    )
  return(imptf)
}


plot_logcumulative_fit <- function(datosplot,titlestr="",dcol="red")
{
   
  datatrf <- datosplot
  datatrf$ac_strength <- datatrf$ac_strength
  datatrf$log10_degree <- log10(datatrf$degree)
  datatrf$log10_acstrength <- log10(datatrf$ac_strength)
  datosfit <- datatrf
  mod <- lm(datosfit$log10_acstrength ~ datosfit$log10_degree)
  beta <- mod[[1]][1]
  alpha <- mod[[1]][2]
  xmin <- min(datosfit$log10_degree)
  ymin <- alpha*xmin+beta
  xmax <- max(datosfit$log10_degree)
  ymax <- alpha*xmax+beta
  
  ic <- confint(mod,level=0.95)
  etmodel <- sprintf("log(S) = %.2f log(d) %.2f Adj. R^2 = %0.3f",as.numeric(mod[[1]][2]),as.numeric(mod[[1]][1]),summary(mod)$adj.r.squared)
  imptf <- ggplot(datatrf,aes(x=degree,y=ac_strength))+geom_point(color=dcol,alpha=0.5)+
    ggtitle(titlestr)+xlab("Degree")+ylab("Cumulative Normalized strength")+
    scale_x_log10()+scale_y_log10()+
    geom_text(x=quantile(datatrf$log10_degree,probs=c(0.03)), 
              y=min(datatrf$log10_acstrength),label=etmodel, size = 5, hjust=0)+
    geom_text(x=xmax,y=ymax,label="*")+
    geom_abline(slope = alpha, intercept = beta, color = "black", alpha = 0.5, linetype = 2) +
    theme_bw() +  theme(plot.title = element_text(hjust = 0.5, size = 18),
                        axis.title.x = element_text(color="grey30", size = 15, face="bold"),
                        axis.title.y = element_text(color="grey30", size= 15, face="bold"),
                        legend.title=element_blank(),
                        legend.position = "top",
                        legend.text=element_text(size=10),
                        panel.grid.minor = element_blank(),
                        axis.text.x = element_text(face="bold", color="grey30", size=14),
                        axis.text.y = element_text(face="bold", color="grey30", size=14)
    )
  return(imptf)
}



plot_linear_fit <- function(datosplot,titlestr="",dcol="red")
{
  
  datatrf <- datosplot
  datatrf$log10_degree <- log10(datatrf$degree)
  datatrf$log10_strength <- log10(datatrf$strength)
  datosfit <- datatrf[(datatrf$log10_strength< quantile(datatrf$log10_strength,probs=c(0.9))),]
  mod <- lm(datosfit$log10_strength ~ datosfit$log10_degree)
  beta <- mod[[1]][1]
  alpha <- mod[[1]][2]
  xmin <- min(datosfit$log10_degree)
  ymin <- alpha*xmin+beta
  xmax <- max(datosfit$log10_degree)
  ymax <- alpha*xmax+beta
  
  etmodel <- sprintf("log(s) = %.2f log(d) %.2f Adj. R^2 = %0.3f",as.numeric(mod[[1]][2]),as.numeric(mod[[1]][1]),summary(mod)$adj.r.squared)
  imptf <- ggplot(datatrf,aes(x=degree,y=strength))+geom_point(color=dcol,alpha=0.5)+
    ggtitle(titlestr)+xlab("Degree")+ylab("Normalized strength")+
    scale_x_log10()+scale_y_log10()+
    geom_text(x=quantile(datatrf$log10_degree,probs=c(0.02)), 
              y=min(datatrf$log10_strength),label=etmodel, size = 5, hjust=0)+
    geom_text(x=xmax,y=ymax,label="*")+
    geom_abline(slope = alpha, intercept = beta, color = "black", alpha = 0.5, linetype = 2) +
    theme_bw() +  theme(plot.title = element_text(hjust = 0.5, size = 18),
                        axis.title.x = element_text(color="grey30", size = 15, face="bold"),
                        axis.title.y = element_text(color="grey30", size= 15, face="bold"),
                        legend.title=element_blank(),
                        legend.position = "top",
                        legend.text=element_text(size=10),
                        panel.grid.minor = element_blank(),
                        axis.text.x = element_text(face="bold", color="grey30", size=14),
                        axis.text.y = element_text(face="bold", color="grey30", size=14)
    )
  return(imptf)
}




TFstring = "TF_"
files <- paste0(TFstring,"RedAdyCom",seq(ini_seq,end_seq))

for (orig_file in files)
{
  red <- paste0(orig_file,"_FILT")
  redorig <- gsub(TFstring,"",red)                #Empirical data
  series = "Exporter"
  year=gsub("_FILT","",strsplit(red,"RedAdyCom")[[1]][-1])
  grafs <- gen_links_strength_distribution(red,series,"blue",empirical = FALSE)
  
  data_e_TF <- grafs$plots_TF$data_exp
  data_i_TF <- grafs$plots_TF$data_imp
  
  sqe_TF <- plot_sq_fit(data_e_TF, titlestr = "Synthetic Exporters at TF", dcol="blue")
  sqi_TF <- plot_sq_fit(data_i_TF, titlestr = "Synthetic Importers at TF", dcol="red")
 
  line_TF <- plot_linear_fit(data_e_TF, titlestr = "Synthetic Exporters at TF", dcol="blue")
  lini_TF <- plot_linear_fit(data_i_TF, titlestr = "Synthetic Importers at TF", dcol="red")
  
  acc_e_TF <- plot_log_fit(data_e_TF, titlestr = "Synthetic Exporters at TF", dcol="blue")
  acc_i_TF <- plot_log_fit(data_i_TF, titlestr = "Synthetic Importers at TF", dcol="red")
  
  acc_ecumulative_TF <- plot_logcumulative_fit(data_e_TF, titlestr = "Synthetic Exporters at TF", dcol="blue")
  acc_icumulative_TF <- plot_logcumulative_fit(data_i_TF, titlestr = "Synthetic Importers at TF", dcol="red")
  
  #estimate_powerlaw(data_e_TF)
  
  data_e <- grafs$plots_final$data_exp
  data_i <- grafs$plots_final$data_imp
  
  # sqe <- plot_sq_fit(data_e, titlestr = "Synthetic Exporters at TT", dcol="blue")
  # sqi <- plot_sq_fit(data_i, titlestr = "Synthetic Importers at TT", dcol="red")
  # 
  # line <- plot_linear_fit(data_e, titlestr = "Synthetic Exporters at TT", dcol="blue")
  # lini <- plot_linear_fit(data_i, titlestr = "Synthetic Importers at TT", dcol="red")
  # 
  # acc_e <- plot_log_fit(data_e, titlestr = "Synthetic Exporters at TT", dcol="blue")
  # acc_i <- plot_log_fit(data_i, titlestr = "Synthetic Importers at TT", dcol="red")
  
  acc_ecumulative <- plot_logcumulative_fit(data_e, titlestr = "Synthetic Exporters", dcol="blue")
  acc_icumulative <- plot_logcumulative_fit(data_i, titlestr = "Synthetic Importers", dcol="red")
  
  
  grafsemp <-  gen_links_strength_distribution(red,series,"blue",empirical = TRUE)
  data_e_emp <- grafsemp$plots_final$data_exp
  data_i_emp <- grafsemp$plots_final$data_imp
  acc_ecumulative_emp <- plot_logcumulative_fit(data_e_emp, titlestr = "Empirical Exporters", dcol="blue")
  acc_icumulative_emp <- plot_logcumulative_fit(data_i_emp, titlestr = "Empirical Importers", dcol="red")
  
  
  dir.create("../figures/linksstrength/", showWarnings = FALSE)
  ppi <- 300

  
  # LOG CS ~ LOG D
  png(paste0("../figures/linksstrength/LS_ALL_LOGCUMULATIVE_",red,".png"), width=(14*ppi), height=12*ppi, res=ppi)
  grid.arrange(acc_icumulative, acc_icumulative_emp, acc_ecumulative, acc_ecumulative_emp, ncol=2, nrow=2)
  dev.off()
  
  # LOG CS ~ LOG D
  png(paste0("../figures/linksstrength/LS_EXPORTERS_LOGCUMULATIVE_",red,".png"), width=(14*ppi), height=6*ppi, res=ppi)
  grid.arrange(acc_ecumulative, acc_ecumulative_emp, ncol=2, nrow=1)
  dev.off()
}