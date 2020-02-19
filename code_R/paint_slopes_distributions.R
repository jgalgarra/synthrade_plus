# Cumulative degree-cumulative strength distributions 
#  
# Author: Javier Garcia Algarra
#
# Invocation: Rscript paint_slopes_distributions
#                   
#
#  Plot at /figures/linksstrength/ALL_FILTERED_Slopes.png

library(grid)
library(gridExtra)
library(ggplot2)
source("parse_command_line_args.R")

slopes <- read.delim("../results/Slopes.txt")
slopes_emp <- slopes#[slopes$Experiment==1,]
dataslope <- data.frame("Year"=c(),"slope"=c(),"Node"=c())
for (i in 1:nrow(slopes_emp)){
  datatemp <- data.frame("Year"=slopes_emp$Year[i],"slope"=slopes_emp$ExpSlopeEmp[i]-1,"Node"="Exporter Empirical")
  dataslope <- rbind(dataslope,datatemp)
  datatemp <- data.frame("Year"=slopes_emp$Year[i],"slope"=slopes_emp$ExpSlopeSynth[i]-1,"Node"="Exporter Synth")
  dataslope <- rbind(dataslope,datatemp)
  datatemp <- data.frame("Year"=slopes_emp$Year[i],"slope"=slopes_emp$ImpSlopeEmp[i]-1,"Node"="Importer Empirical")
  dataslope <- rbind(dataslope,datatemp)
  datatemp <- data.frame("Year"=slopes_emp$Year[i],"slope"=slopes_emp$ImpSlopeSynth[i]-1,"Node"="Importer Synth")
  dataslope <- rbind(dataslope,datatemp)
}

p <- ggplot(data=dataslope,aes(Node,slope)) + 
     geom_boxplot(aes(group=Node,fill=Node),alpha=0.3) + ylim(c(0,2))+
     ylab("Strength vs Degree Exponent\n")+xlab("")+ggtitle("")+
     theme_bw()+
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
        plot.title = element_text(lineheight=.8, size=12, face="bold",hjust = 0.5),
        axis.text.x = element_text(face="bold", size=11, angle = 25, hjust = 1),
        axis.text.y = element_text(face="bold", size=11),
        axis.title.x = element_text(face="bold", size=12),
        axis.title.y  = element_text(face="bold", size=12) )

p <- p +scale_fill_manual(values=c("steelblue1","steelblue4","red1", "red3"))

datayearexp <- dataslope[dataslope$Node==("Exporter Synth"),]
datayearexpemp <- dataslope[dataslope$Node==("Exporter Empirical"),]
q <- ggplot(data=datayearexp,aes(Year,slope)) + 
  geom_boxplot(aes(group=Year),fill="blue",alpha=0.3) + ylim(c(0,2))+
  geom_point(data=datayearexpemp,aes(Year,slope),shape=21,size=2,fill="blue",color="transparent")+
  ylab("Exponent")+xlab("")+ggtitle("Strength vs Degree (Exporters)")+
  theme_bw()+
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
        plot.title = element_text(lineheight=.8, size=12, face="bold",hjust = 0.5),
        axis.text.x = element_text(face="bold", size=9, angle=20, hjust=1),
        axis.text.y = element_text(face="bold", size=9),
        axis.title.x = element_text(face="bold", size=13),
        axis.title.y  = element_text(face="bold", size=13) )

datayearimp <- dataslope[dataslope$Node==("Importer Synth"),]
datayearimpemp <- dataslope[dataslope$Node==("Importer Empirical"),]
r <- ggplot(data=datayearimp,aes(Year,slope)) + 
  geom_boxplot(aes(group=Year,fill=Node),alpha=0.3) + ylim(c(0,2))+
  geom_point(data=datayearimpemp,aes(Year,slope),shape=21,size=2,fill="red",color="transparent")+
  ylab("Exponent")+xlab("")+ggtitle("Strength vs Degree (Importers)")+
  theme_bw()+
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
        plot.title = element_text(lineheight=.8, size=12, face="bold",hjust = 0.5),
        axis.text.x = element_text(face="bold", size=9, angle=20, hjust=1),
        axis.text.y = element_text(face="bold", size=9),
        axis.title.x = element_text(face="bold", size=13),
        axis.title.y  = element_text(face="bold", size=13) )

dir.create("../figures/linksstrength/", showWarnings = FALSE)
fsal <- paste0("../figures/linksstrength/Slopes_ALL.png")
ppi <- 300
png(fsal, width=6*ppi, height=6*ppi, res=ppi)
print(p)
dev.off()
fsal <- paste0("../figures/linksstrength/Slopes_Year.png")
png(fsal, width=10*ppi, height=8*ppi, res=ppi)
grid.arrange(q, r, ncol=1, nrow=2,top="" )
dev.off()