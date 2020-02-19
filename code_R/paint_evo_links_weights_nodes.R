# Extension of analysis of numlinks log files for t < TF
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
  names(simdata) <- c("Step","Links","Tokens","Exporters","Importers","T5Min","T5Q25","T5Median","T5Q75","T5Max",
                      "LastLink","EmptyCells","meanprob","varsigma")
  formationdata <- simdata[simdata$Step <= ftdata$SimStep,]
  levplot <- c("Links","Tokens","Exporters","Importers")
  rowsEvo <- nrow(formationdata)*length(levplot)
  EvoDataFormation <- data.frame("Step"=rep(0,rowsEvo),"Data"=rep(0,rowsEvo),"Type"=rep("Links",rowsEvo))
  levels(EvoDataFormation$Type) <- levplot
  i = 1;
  j=1;
  for (i in seq(1:nrow(formationdata))){
    EvoDataFormation$Step[j] <- formationdata$Step[i]
    EvoDataFormation$Data[j] <- formationdata$Links[i]
    EvoDataFormation$Type[j] <- "Links"
    j = j + 1
    EvoDataFormation$Step[j] <- formationdata$Step[i]
    EvoDataFormation$Data[j] <- formationdata$Tokens[i]
    EvoDataFormation$Type[j] <- "Tokens"
    j = j + 1
    EvoDataFormation$Step[j] <- formationdata$Step[i]
    EvoDataFormation$Data[j] <- formationdata$Exporters[i]
    EvoDataFormation$Type[j] <- "Exporters"
    j = j + 1
    EvoDataFormation$Step[j] <- formationdata$Step[i]
    EvoDataFormation$Data[j] <- formationdata$Importers[i]
    EvoDataFormation$Type[j] <- "Importers"
    j = j + 1
  }
  
  if (max(EvoDataFormation$Step)<=80000){
    timebreak <- 20000
  } else
    timebreak <- 50000
  EvoLT <- EvoDataFormation[EvoDataFormation$Type == "Links" | EvoDataFormation$Type == "Tokens",]
 
  EvoN <- EvoDataFormation[EvoDataFormation$Type == "Importers" | EvoDataFormation$Type == "Exporters",]
  transfrate <- max(formationdata$Tokens/max(formationdata$Exporters,formationdata$Importers))
  EvoLT$Data <- EvoLT$Data / transfrate
  p <- ggplot(data=EvoN,aes(x=Step))+geom_line(aes(y=Data,color=Type),size=0.7)+
    
      geom_line(aes(y=Data,color=Type),size=0.7,data=EvoLT)+
       scale_y_continuous(sec.axis = sec_axis(~.*transfrate, name = "Links & Tokens")
      )+       ggtitle(paste(year,"Build up dynamics"))+xlab("Simulation Step")+ylab("Nodes")+

       theme_bw() +
       theme(legend.title = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          panel.grid.major =  element_line(linetype = 3, color="ivory3"),
          panel.grid.minor = element_blank(),
          legend.text = element_text(size=9, face="bold"),
          plot.title = element_text(size=12,lineheight=.5, face="bold",hjust = 0.5),
          axis.text = element_text(size=10),
          axis.title.x = element_text(face="bold", size=10),
          axis.title.y  = element_text(face="bold", size=11) )
  
  
  
  
  dir.create("../figures/linksandtokens/", showWarnings = FALSE)
  ppi <- 300
  png(paste0("../figures/linksandtokens/",year,"_All_FT.png"), width=(6*ppi), height=4*ppi, res=ppi)
  print(p)
  dev.off()

}