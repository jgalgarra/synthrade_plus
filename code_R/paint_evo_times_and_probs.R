# Analysis of numlinks log files
#
# numlinks fields:
#         Simulation Step
#         Number of links
#         Number of tokens
#         Exporters
#         Importers
#         Probability matrix Tukey five number
#         Probability of last existing link
#         Cumulative probability of empty links
#         Median probability of prob matrix
#         Variance of prob matrix


library("ggplot2")
library(scales)
source("read_filter_condition.R")
source("parse_command_line_args.R")
# Read simulation log
symlog <- read.csv("../results/symlog.txt", header=FALSE, sep=";")
datalog <- symlog
names(datalog) <- c("Instant", "Year", "NumExper", "SimStep", "TokenCount", "LinkCount", "TotalLinks")

experiment <- 1

years <- seq(ini_seq,end_seq)

for (year in years){
  logfilt <- datalog[datalog$Year==year & datalog$Instant=="FT" & datalog$NumExper == experiment,]
  ftdata <- logfilt[nrow(logfilt),]
  file_name <- paste0("numlinks_",year,"_FILT_W_",experiment,".txt")
  simdata <- read.csv(paste0("../results/numlinks/",file_name), header=FALSE, sep=";")
  names(simdata) <- c("SimStep","Links","Tokens","Exporters","Importers","T5Min","T5Q25","T5Median","T5Q75","T5Max",
                      "LastLink","EmptyCells","meanprob","varsigma")
  levplot <- c("Min","Median","Max","LastLink","EmptyCells")
  rowsEvo <- nrow(simdata)*length(levplot)
  EvoDataProb <- data.frame("Step"=rep(0,rowsEvo),"Prob"=rep(0,rowsEvo),"Type"=rep("Min",rowsEvo))
  levels(EvoDataProb$Type) <- levplot
  j=1;
  for (i in seq(1:nrow(simdata))){
    EvoDataProb$Step[j] <- simdata$SimStep[i]
    EvoDataProb$Prob[j] <- simdata$T5Min[i]
    EvoDataProb$Type[j] <- "Min"
    j = j + 1
    EvoDataProb$Step[j] <- simdata$SimStep[i]
    EvoDataProb$Prob[j] <- simdata$T5Median[i]
    EvoDataProb$Type[j] <- "Median"
    j = j + 1
    EvoDataProb$Step[j] <- simdata$SimStep[i]
    EvoDataProb$Prob[j] <- simdata$T5Max[i]
    EvoDataProb$Type[j] <- "Max"
    j = j + 1
    EvoDataProb$Step[j] <- simdata$SimStep[i]
    EvoDataProb$Prob[j] <- simdata$LastLink[i]
    EvoDataProb$Type[j] <- "LastLink"
    j = j + 1
    EvoDataProb$Step[j] <- simdata$SimStep[i]
    EvoDataProb$Prob[j] <- simdata$EmptyCells[i]
    EvoDataProb$Type[j] <- "EmptyCells"
    j = j + 1
  }
  
  if (max(EvoDataProb$Step)<=80000){
    timebreak <- 20000
  } else
    timebreak <- 50000
  
  p <- ggplot(data=EvoDataProb,aes(x=Step, y=10**Prob, color=Type))+geom_line(size=0.7)+
       scale_x_continuous(trans = sqrt_trans(),
                          breaks = c(50,500,ftdata$SimStep,15000,seq(0,(1+(max(EvoDataProb$Step)%/%timebreak))*timebreak,by=timebreak))) +
       scale_y_continuous(trans = log10_trans(),
                                         breaks = trans_breaks("log10", function(x) 10^x),
                                         labels = trans_format("log10", math_format(10^.x)))+
       ggtitle("")+xlab("Simulation Step")+ylab("Probability")+
       geom_vline(data=ftdata, aes(xintercept=SimStep),
               linetype="dashed", size=0.7, colour="lightblue")+
       theme_bw() +
       theme(legend.title = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text = element_text(size=13, face="bold"),
          plot.title = element_text(size=13,lineheight=.5, face="bold",hjust = 0.5),
          axis.text.x = element_text(size=13,angle=25,hjust=1,face="bold"),
          axis.text.y = element_text(size=14, face="bold"),
          axis.title.x = element_text(face="bold", size=15),
          axis.title.y  = element_text(face="bold", size=15) )

  dir.create("../figures/probevolution/", showWarnings = FALSE)
  ppi <- 600
  png(paste0("../figures/probevolution/",year,"_probevo.png"), width=(8*ppi), height=8*ppi, res=ppi)
  print(p)
  dev.off()
  transfrate <- max(simdata$Tokens/simdata$Links)
  q <- ggplot(data=simdata,aes(x=SimStep))+geom_line(aes(y=Links,colour="Links"))+
                               geom_line(aes(y=Tokens/transfrate,colour="Tokens"))+
       scale_colour_manual(values = c("blue", "red"))+ labs(y = "Links",x = "Tokens",colour = "") + 
       scale_x_continuous(breaks = c(seq(0,(1+(max(EvoDataProb$Step)%/%timebreak))*timebreak,by=timebreak))) +
       scale_y_continuous(sec.axis = sec_axis(~.*transfrate, name = "Tokens")
                          )+
          ggtitle(year)+xlab("Simulation Step")+ theme_bw() +       
          geom_vline(data=ftdata, aes(xintercept=SimStep), linetype="dashed", size=0.7, colour="lightblue")+
          theme(legend.title = element_blank(),
                legend.position = c(0.2, 0.95),
          panel.border = element_blank(),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(), 
          legend.text = element_text(size=9, face="bold"),
          plot.title = element_text(size=12,lineheight=.5, face="bold",hjust = 0.5),
          axis.text = element_text(size=10),
          axis.title.x = element_text(face="bold", size=10),
          axis.title.y  = element_text(face="bold", size=11) )
  
  dir.create("../figures/linksandtokens/", showWarnings = FALSE)
  png(paste0("../figures/linksandtokens/",year,"_links_tokens.png"), width=(7*ppi), height=4*ppi, res=ppi)
  print(q)
  dev.off()
}