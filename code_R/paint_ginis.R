library(ggplot2)
library(grid)
library(gridExtra)
Ginis <- read.delim("../results/Ginis.txt")
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

boxplot(Gini_export ~ Method, data = Ginis)


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


mean_vals=aggregate(Ginis_Sel$Gini_import , by=list(Ginis_Sel$TradeBoost,Ginis_Sel$Scope) , mean)
names(mean_vals)= c("TradeBoost","Scope","Gini_import")
# Add a phantom record with the same TradeBoost 0 value of the unimproved network, with "REGIONAL"
# scope, so that the trend lines start at TradeBoost == 0
mean_add <- mean_vals[mean_vals$TradeBoost==0,]
mean_add$Scope = "REGIONAL"
mean_vals <- rbind(mean_vals,mean_add)

Ginis_Sel$YF <- paste(Ginis_Sel$Year,Ginis_Sel$ImprovedFraction)
p11 <- ggplot(Ginis_Sel, aes(x = as.numeric(TradeBoost), y = Gini_import, fill = Scope)) +
  geom_jitter(alpha=0.5,shape=21,size=3,color="transparent",width=1) +
  geom_line(data=mean_vals,aes(x = as.numeric(TradeBoost), y = Gini_import, color= Scope),size=1,alpha=0.7)+
  ylim(c(0.5,1))+ xlab("Boost percentage")+facet_wrap(~YF)+
  # ggtitle("Gini index by year") +
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold"),
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

paint_ginis_year <- function(datos)#,year)
{
  #ginis_year <- datos[datos$Year == year,]
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


write.table(Ginis,"../results/ginis_detail.txt",sep=";",row.names = FALSE)
write.table(mean_vals_year,"../results/ginis_means.txt",sep=";",row.names = FALSE)


p12 <- paint_ginis_year(mean_vals_year)#,2011)

ppi <- 300
fsal <- paste0("../figures/Ginis_evol.png")
png(fsal, width=10*ppi, height=6*ppi, res=ppi)
print(p12)
dev.off()

ppi <- 300
fsal <- paste0("../figures/Ginis_dispersion.png")
png(fsal, width=10*ppi, height=6*ppi, res=ppi)
print(p11)
dev.off()



ppi <- 300
Ginis_Box <- Ginis_Sel[Ginis_Sel$ImprovedFraction>0,]
Ginis_Box$ImprovedFraction <- paste("Improved Fraction",Ginis_Box$ImprovedFraction,"%")
p13 <- ggplot(Ginis_Box, aes(x = as.factor(TradeBoost), y = Gini_import, color = Scope)) +
   geom_boxplot(alpha=0.8) + #geom_jitter(width=1.5) +
   ylim(c(0.5,1))+ xlab("Boost percentage")+facet_wrap(~ImprovedFraction)+
   # ggtitle("Gini index by year") +
   theme_bw() +
   theme(plot.title = element_text(size = 14,  face = "bold"),
         text = element_text(size = 12),
         axis.title = element_text(face="bold"),
         axis.text.x=element_text(size = 11)) 
fsal <- paste0("../figures/Ginis_BoxPlots.png")
png(fsal, width=10*ppi, height=4*ppi, res=ppi)
print(p13)
dev.off()