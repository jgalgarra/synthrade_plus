library(grid)
library(gridExtra)
library(igraph)
library(ggplot2)
source("read_filter_condition.R")
source("parse_command_line_args.R")
years <- seq(ini_seq,end_seq)

PaintProbPlot <- function(datos,titletext,xlabel="")
{
  p <- ggplot() + geom_density(aes(x= cuenta, color = Instant, fill = Instant),  alpha = .05,
                               data=datos, position = "identity", adjust=1.5)+ 
    xlab(paste(titletext,"Probability"))+ylab("Density\n")+scale_x_continuous(breaks = c(-6,-4,-2,0), 
                                                      labels = c("1e-06","1e-04","1e-02","1"), 
                                                      limits = c(-6,0)) +
    ggtitle("")+  
    scale_fill_manual(values=c("orange","blue"))+
    scale_color_manual(values=c("orange","blue"))+
    theme_bw() +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), 
          legend.title = element_text(size=13, face="bold"),
          legend.text = element_text(size=13, face="bold"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size = 20, face="bold", hjust = 0.5),
          axis.text = element_text(face="bold", size=13),
          axis.title.x = element_text(face="bold", size=13),
          axis.title.y  = element_text(face="bold", size=13)
          )
  
  return(p)
}


for (lyear in years)
{
   probtf <- read.delim(paste0("../results/probs/PR_TF_RedAdyCom",lyear,"_FILT_W_1.txt"), header=FALSE)
   probtt <- read.delim(paste0("../results/probs/PR_TT_RedAdyCom",lyear,"_FILT_W_1.txt"), header=FALSE)
   
   exp_probtf <- rowSums(probtf)
   imp_probtf <- colSums(probtf)
   exp_probtt <- rowSums(probtt)
   imp_probtt <- colSums(probtt)
   

   zeros <- rep(0,length(exp_probtf))
   dataptf <- data.frame("cuenta" = zeros,"Instant" = "Formation")
   dataptf$cuenta <- exp_probtf
   
   zeros <- rep(0,length(exp_probtt))
   dataptt <- data.frame("cuenta" = zeros,"Instant" = "Final")
   dataptt$cuenta <- exp_probtt
   
   datape <- rbind(dataptt,dataptf)
   datape$cuenta <- log10(datape$cuenta)
   pplot1 <- PaintProbPlot(datape,paste("Exporters"),xlabel = "Probability")
   
   zeros <- rep(0,length(imp_probtf))
   dataptf <- data.frame("cuenta" = zeros,"Instant" = "Formation")
   dataptf$cuenta <- imp_probtf
   
   zeros <- rep(0,length(imp_probtt))
   dataptt <- data.frame("cuenta" = zeros,"Instant" = "Final")
   dataptt$cuenta <- imp_probtt
   
   datapi <- rbind(dataptt,dataptf)
   datapi$cuenta <- log10(datapi$cuenta)
   pplot2 <- PaintProbPlot(datapi,paste("Importers"),xlabel = "Probability")
   
   dataall <- rbind(datapi,datape)
   pplot3 <- PaintProbPlot(dataall,paste("Full matrix"),xlabel = "Probability")
   
   ppi <- 600
   dir.create("../figures/probdensities/", showWarnings = FALSE)
   png(paste0("../figures/probdensities/PD_ALL_",lyear,".png"), width=(24*ppi), height=5*ppi, res=ppi)
   grid.arrange(pplot1,pplot2,pplot3, ncol=3, nrow=1 )
   dev.off()
   
   ppi <- 600
   dir.create("../figures/probdensities/", showWarnings = FALSE)
   png(paste0("../figures/probdensities/PD_GUILDS_",lyear,".png"), width=(6*ppi), height=8*ppi, res=ppi)
   grid.arrange(pplot1,pplot2, ncol=1, nrow=2 )
   dev.off()
}

