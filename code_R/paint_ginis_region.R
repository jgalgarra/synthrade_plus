# Plots of Gini import indexes
#
# Author: Javier Garcia Algarra
#
# Invocation: Rscript paint_ginis_region year
#                    year : year to plot
#
# Example: Rscript paint_ginis_region_year 2011


library(ggplot2)
library(grid)
library(gridExtra)

exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}


args = commandArgs(trailingOnly=TRUE)

if (length(args)==0){
  print("Syntax: Rscript paint_ginis_region.R year")
  exit()
} else{
  year <- as.numeric(args[1])
}


imprfrac = 2

region_filter = "ALL"   # Originally intented to plot just one region, now plots all

paint_ginis_year <- function(datos)
{
  ginis_year <- datos
  pres <- ggplot(ginis_year, aes(x = as.numeric(TradeBoost), y = Gini_import, 
                                 color = Policy)) +
    geom_line(size=0.7,alpha=0.7)+geom_point(size=2,alpha=0.7)+
    ylim(c(0.5,1))+ xlab("Boost percentage") + 
    facet_wrap(~Year,scales="free_x")+
    theme_bw() +
    theme(plot.title = element_text(size = 14,  face = "bold"),
          legend.position = "bottom",
          text = element_text(size = 12),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11)) 
  return(pres)
  
}

Ginis <- read.delim("../results/GinisRegions.txt")

Ginis$Year <- 0
Ginis$Method <- "SYNTH"
Ginis$MethodLabel <- "SYNTH"
Ginis$Scope <- "GLOBAL"
Ginis$TradeBoost <- 0
Ginis$Experiment <- 0
Ginis$ImprovedFraction <- 0.0
for (i in 1:nrow(Ginis)){
  texto <- strsplit(strsplit(as.character(Ginis[i,]$File),"RedAdyCom")[[1]][2],"_FILT")
  Ginis$Year[i] <- texto[[1]][1]
  Method <- strsplit(texto[[1]][2],"_W")[[1]][2]
  Experiment <- strsplit(gsub(".txt","_",texto[[1]][2]),"_")[[1]][3]
  Ginis$Experiment[i] <- Experiment
  if (grepl("FBAL",Method)){
    if (grepl("BOOST",Method)){
      Ginis$MethodLabel[i] = "BOOST"
      Method <- gsub(".txt","",Method)
      Ginis$Method[i] <- strsplit(strsplit(Method,"FBAL_")[[1]][2],".txt")[[1]][1]
      info_boost <-  strsplit(strsplit(Method,"BOOST_")[[1]][2],"_")
      Ginis$ImprovedFraction[i] <- as.integer(100*(1-as.numeric(info_boost[[1]][1])))
      Ginis$TradeBoost[i] <- as.numeric(info_boost[[1]][2])
      if (grepl("REGIONAL",info_boost[[1]][3]))
        Ginis$Scope[i] <- "REGIONAL"
    } 
  }
  if (grepl("EMPIRICAL",Method)){
    Ginis$Method[i] <- "EMPIRICAL"
    Ginis$MethodLabel[i] <- "EMPIRICAL"
  }
  
}
Ginis$Method <- as.factor(Ginis$Method)


Ginis_WORLD <- Ginis[Ginis$Region=="WORLD",]
Ginis_WORLD <- Ginis_WORLD[Ginis_WORLD$Method %in% c("SYNTH","EMPIRICAL"),]
Ginis_WORLD <- Ginis_WORLD[Ginis_WORLD$Method %in% c("SYNTH","EMPIRICAL"),]
Ginis_WORLD <- unique(Ginis_WORLD)
Ginis_WORLD <- Ginis_WORLD[Ginis_WORLD$Experiment != "RAW",]

# Boxplot of empirical vs synthetic networks by year
pallyears_imp <- ggplot(Ginis_WORLD, aes(x = as.factor(Year), y = Gini_import, 
                                    color = Method)) +
  geom_boxplot(alpha=0.7) + # ggtitle(paste0(year," improved fraction ",imprfrac,"%")) +
  ylim(c(0.5,1))+ xlab("Year")+ylab("Importers Gini index")+
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10,angle = 45,hjust=1))

pallyears_expt <- ggplot(Ginis_WORLD, aes(x = as.factor(Year), y = Gini_export, 
                                         color = Method)) +
  geom_boxplot(alpha=0.7) + # ggtitle(paste0(year," improved fraction ",imprfrac,"%")) +
  ylim(c(0.5,1))+ xlab("Year")+ylab("Exporters Gini index")+
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10,angle = 45,hjust=1))


Ginis_ALL <- Ginis
# Filter by command line args conditions
Ginis <- Ginis[Ginis$Year %in% c(year),]

p10 <- ggplot(Ginis, aes(x = Year, y = Gini_import, fill = Method)) +
  geom_boxplot(alpha=0.7) +
  scale_x_discrete(name = "Year") + ylim(c(0.5,1))+
  ggtitle("Gini index by year") +
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Accent")

Ginis_Sel <- Ginis[(Ginis$MethodLabel=="BOOST") | (Ginis$MethodLabel=="SYNTH") ,]

if (region_filter!="ALL"){
  Ginis_Mean <- Ginis_Sel[Ginis_Sel$Region == lRegion,]
} else
  Ginis_Mean <- Ginis_Sel[Ginis_Sel$Region == "WORLD",]

mean_vals=aggregate(Ginis_Mean$Gini_import , by=list(Ginis_Mean$TradeBoost,Ginis_Mean$Scope), mean)
names(mean_vals)= c("TradeBoost","Scope","Gini_import")
# Add a phantom record with the same TradeBoost 0 value of the unimproved network, with "REGIONAL"
# scope, so that the trend lines start at TradeBoost == 0
mean_add <- mean_vals[mean_vals$TradeBoost==0,]
mean_add$Scope = "REGIONAL"
mean_vals <- rbind(mean_vals,mean_add)

Ginis_Sel$YF <- paste(Ginis_Sel$Year,Ginis_Sel$ImprovedFraction)
Ginis_Regions <- Ginis_Sel # Data frame with all regional data

if (region_filter %in% c("ALL","WORLD"))  {
  Ginis_Sel <- Ginis_Sel[Ginis_Sel$Region == "WORLD",]
  lRegion = "WORLD"
} else {
  Ginis_Sel <- Ginis_Sel[Ginis_Sel$Region == region_filter,]
  lRegion = region_filter 
}

# Selection of improved fraction
Ginis_Disp = Ginis_Sel[(Ginis_Sel$ImprovedFraction == imprfrac) |(Ginis_Sel$ImprovedFraction == 0) ,]
# Scatter plot, only for selected region values, only year
p11 <- ggplot(Ginis_Disp, aes(x = as.numeric(TradeBoost), y = Gini_import, fill = Scope)) +
  geom_jitter(alpha=0.3,shape=21,size=2,color="transparent",width=1) +
  geom_line(data=mean_vals,aes(x = as.numeric(TradeBoost), y = Gini_import, color= Scope),size=1,alpha=0.7)+
  ylim(c(0.5,1))+ xlab("Boost percentage")+ylab("Importers Gini")+#facet_wrap(~YF)+
  ggtitle(paste0(year," improved fraction ",imprfrac,"%")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) 

Ginis_DispR = Ginis_Regions[(Ginis_Regions$ImprovedFraction == imprfrac) |(Ginis_Regions
                                                                           $ImprovedFraction == 0) ,]
# Boxplots, only for all regions, only year
pregions <- ggplot(Ginis_DispR, aes(x = as.factor(TradeBoost), y = Gini_import, 
                                    color = Scope)) +
  geom_boxplot(alpha=0.7) + ggtitle(paste0(year," improved fraction ",imprfrac,"%")) +
  ylim(c(0.2,1))+ xlab("Boost percentage")+ylab("Importers Gini")+facet_wrap(~Region)+
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) 

mean_vals_year=aggregate(Ginis_Sel$Gini_import , 
                         by=list(Ginis_Sel$Year,Ginis_Sel$TradeBoost,
                                 Ginis_Sel$Scope,Ginis_Sel$ImprovedFraction),
                         mean)
names(mean_vals_year)= c("Year","TradeBoost","Scope","ImprovedFraction","Gini_import")

for (k in unique(mean_vals_year$Year))
{
  mean_add_year <- mean_vals_year[(mean_vals_year$TradeBoost==0) & (mean_vals_year$Year==k),]
  # Add phantom records with the same TradeBoost 0 value of the unimproved network, with "REGIONAL"
  # scope, so that the trend lines start at TradeBoost == 0
  fraction_values <- unique(mean_vals_year$ImprovedFraction)
  fraction_values <- fraction_values[fraction_values>0]
  
  for (j in fraction_values)
  {
    mean_add_year$TradeBoost == 0
    mean_add_year$ImprovedFraction = j
    mean_add_year$Scope = "REGIONAL"
    mean_vals_year <- rbind(mean_vals_year,mean_add_year)
    mean_add_year$Scope = "GLOBAL"
    mean_vals_year <- rbind(mean_vals_year,mean_add_year)
  }
}
mean_vals_year <- mean_vals_year[mean_vals_year$ImprovedFraction!=0,]
mean_vals_year$Policy <- paste0(mean_vals_year$Scope," ",mean_vals_year$ImprovedFraction,"%")

write.table(Ginis_ALL,"../results/ginis_detail_regions.txt",sep=";",row.names = FALSE)
write.table(mean_vals,paste0("../results/ginis_means_regions_",year,".txt"),sep=";",row.names = FALSE)

# Plot of Gini import means as a function of boost and improved fraction
p12 <- paint_ginis_year(mean_vals_year)

Ginis_Box <- Ginis_Sel[Ginis_Sel$ImprovedFraction>0,]
# Boxplots
Ginis_Box$ImprovedFraction <- paste("Improved Fraction",Ginis_Box$ImprovedFraction,"%")
p13 <- ggplot(Ginis_Box, aes(x = as.factor(TradeBoost), y = Gini_import, color = Scope)) +
  geom_boxplot(alpha=0.8) + #geom_jitter(width=1.5) +
  ylim(c(0.5,1))+ xlab("Boost percentage")+facet_wrap(~ImprovedFraction)+
  labs(title=paste(year,lRegion))+
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) 



ppi <- 300
fsal <- paste0("../figures/Ginis_Empirical_vs_Synth_ALLYEARS.png")
png(fsal, width=10*ppi, height=4*ppi, res=ppi)
grid.arrange(pallyears_expt,pallyears_imp, ncol=2, nrow=1,top="" )
dev.off()

ppi <- 300
fsal <- paste0("../figures/Ginis_evol_regions_",year,"_",lRegion,".png")
png(fsal, width=10*ppi, height=6*ppi, res=ppi)
print(p12)
dev.off()

ppi <- 300
fsal <- paste0("../figures/Ginis_dispersion_WORLD_",year,"_",lRegion,".png")
png(fsal, width=10*ppi, height=6*ppi, res=ppi)
print(p11)
dev.off()

ppi <- 300
fsal <- paste0("../figures/Ginis_BOX_WBREGIONS_regions_",year,"_",lRegion,".png")
png(fsal, width=10*ppi, height=6*ppi, res=ppi)
print(pregions)
dev.off()


ppi <- 300
fsal <- paste0("../figures/Ginis_BoxPlots_regions_",year,"_",lRegion,".png")
png(fsal, width=10*ppi, height=4*ppi, res=ppi)
print(p13)
dev.off()

# ppi <- 300
# fsal <- paste0("../figures/Ginis_BoxPlots_ALLYEARS.png")
# png(fsal, width=8*ppi, height=6*ppi, res=ppi)
# print(pallyears)
# dev.off()