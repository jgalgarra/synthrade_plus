library(grid)
library(gridExtra)
library(ggplot2)

PaintKS <- function(series,fillcol,title)
{
  r <- ggplot(data=series,aes(Year,p.value)) +
    geom_boxplot(aes(group=Year),fill=fillcol, alpha = 0.5)+
    geom_hline(aes(yintercept=0.05), colour="green", alpha = 0.8, size = 1)+ylab(paste("KS test p.value\n",title,"\n"))+xlab("")+
    xlim(c(min(series$Year)-1,max(series$Year)+1))+ ggtitle("")+ theme_bw()+
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 3, color="ivory3"),
          panel.grid.major.x = element_line(linetype = 3, color="ivory3"), 
          legend.title = element_blank(),
          legend.position = "none",
          legend.text = element_text(size=12, face="bold"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(lineheight=.8, size=12, face="bold",hjust = 0.5),
          axis.text.y = element_text(face="bold", size=12),
          axis.text.x = element_text(face="bold", size=12, angle = 45, hjust = 1),
          axis.title.x = element_text(face="bold", size=11),
          axis.title.y  = element_text(face="bold", size=11) )
  return(r)
}

KSdata <- read.table("../results/KSTEST.txt",header=TRUE);
Exdata <- data.frame("Year" = KSdata$Year, "p.value" = KSdata$KSexport)
KSexp <- PaintKS(Exdata,"blue","Exporters Strength")
Impdata <- data.frame("Year" = KSdata$Year, "p.value" = KSdata$KSimport)
KSImp <- PaintKS(Impdata,"red","Importers Strength")
dir.create("../figures/", showWarnings = FALSE)
dir.create("../figures/tests/", showWarnings = FALSE)
fsal <- paste0("../figures/tests/KSplots.png")
ppi <- 300
png(fsal, width=10*ppi, height=8*ppi, res=ppi)
grid.arrange(KSexp, KSImp, ncol=1, nrow=2,top="" )
dev.off()