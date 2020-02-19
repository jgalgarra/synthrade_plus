# Paint the results of Lilliefors tests
#
# Author: Javier Garcia Algarra

library(grid)
library(gridExtra)
library(ggplot2)

paint_lillies <- function(data,fcol,titletext)
{
  pl <- ggplot(data)+geom_point(aes(x=Year, y=pvalue), fill=fcol,color="transparent",shape=21,size=3)+
    xlab("")+ylab(paste("Lilliefors test p.value\n",titletext,"\n"))+ggtitle("")+
    geom_hline(aes(yintercept=0.05), colour="green", alpha = 0.8, size = 0.8)+ theme_bw() +
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
          plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
          axis.text = element_text(face="bold", size=12),
          axis.title.x = element_text(face="bold", size=11),
          axis.title.y  = element_text(face="bold", size=11) )
  return(pl)
}

Lillies <- read.delim("../results/BestLillies.txt")

datalill <- data.frame("Year"=c(),"pvalue"=c(),"Type"=c())
for (i in 1:nrow(Lillies)){
  datatemp <- data.frame("Year"=Lillies$Year[i],"pvalue"=Lillies$Empirical_exporter_filtered[i],"Type"="Exporter")
  datalill <- rbind(datalill,datatemp)
  datatemp <- data.frame("Year"=Lillies$Year[i],"pvalue"=Lillies$Empirical_importer_filtered[i],"Type"="Importer")
  datalill <- rbind(datalill,datatemp)
}
  
exp <- paint_lillies(datalill[datalill$Type=="Exporter",],"blue","Empirical Exporters Strength")
imp <- paint_lillies(datalill[datalill$Type=="Importer",],"red","Empirical Importers Strength")

dir.create("../figures/tests/", showWarnings = FALSE)
fsal <- paste0("../figures/tests/Lillies.png")
ppi <- 300
png(fsal, width=5*ppi, height=8*ppi, res=ppi)
grid.arrange(exp,imp, ncol=1, nrow=2,top="" )
dev.off()