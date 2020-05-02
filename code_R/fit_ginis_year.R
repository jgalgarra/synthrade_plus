# Fits the Gini indexes of a year to a second degree polynomial
#
# Author: Javier Garcia Algarra
#
# Invocation: Rscript fit_ginis_year.R year
#                    year : year to plot
#
# Example: Rscript fit_ginis_year.R 2011

library(ggplot2)

exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0){
  print("Syntax: Rscript fit_ginis_year.R year")
  year <- 2017 #exit()
} else{
  year <- as.numeric(args[1])
}

# The file ginis_means_regions_XXXX.txt contains a table with the values of boosted Ginis
# for an improved fraction of 2%
ginis_means <- read.csv(paste0("../results/ginis_means_regions_",year,".txt"), sep=";")

dglobal <- dglobal <- ginis_means[ginis_means$Scope == "GLOBAL",]

fmodelglobal <- lm(dglobal$Gini_import ~ dglobal$TradeBoost + I(dglobal$TradeBoost^2))
predglobal <- predict(fmodelglobal,data.frame("TradeBoost"=dglobal$TradeBoost))
comparedata <- data.frame("Boost"=c(),"Importer_Gini"=c(),"Type"=c())
for (i in seq(1,length(dglobal$TradeBoost))){
  comparedata <- rbind(comparedata,data.frame("Boost"=dglobal$TradeBoost[i],
                                              "Importer_Gini"=dglobal$Gini_import[i],"Type"="Computed"))
  comparedata <- rbind(comparedata,data.frame("Boost"=dglobal$TradeBoost[i],
                                              "Importer_Gini"=predglobal[i],"Type"="Predicted"))
}
comparedata$Scope <- "GLOBAL"
print(summary(fmodelglobal))

dregional <- ginis_means[ginis_means$Scope == "REGIONAL",]
fmodelregional <- lm(dregional$Gini_import ~ dregional$TradeBoost )
predregional <- predict(fmodelregional,data.frame("TradeBoost"=dregional$TradeBoost))
comparedatareg <- data.frame("Boost"=c(),"Importer_Gini"=c(),"Type"=c())
for (i in seq(1,length(dregional$TradeBoost))){
  comparedatareg <- rbind(comparedatareg,data.frame("Boost"=dregional$TradeBoost[i],
                                              "Importer_Gini"=dregional$Gini_import[i],"Type"="Computed"))
  comparedatareg <- rbind(comparedatareg,data.frame("Boost"=dregional$TradeBoost[i],
                                              "Importer_Gini"=predregional[i],"Type"="Predicted"))
}
comparedatareg$Scope <- "REGIONAL"
print(summary(fmodelregional))

compall <- rbind(comparedata,comparedatareg)

p <- ggplot(data=compall,aes(x=Boost,y=Importer_Gini, color=Type))+
     geom_point(shape = 16, size = 2,alpha=0.5)+ylab("Importers Gini")+xlab("Boost percentage")+
     facet_wrap(~Scope,scales="free_x")+
     theme_bw()+
     theme(text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        legend.position = "bottom",
        axis.text.x=element_text(size = 11)) 

ppi <- 300
fsal <- paste0("../figures/Ginis_predicted_",year,".png")
png(fsal, width=8*ppi, height=4*ppi, res=ppi)
print(p)
dev.off()