# Compute the difference between Empirical and Synthetic Gini coefficients
#

ginis_WORLD_ALL_YEARS <- read.csv("../results/ginis_WORLD_ALL_YEARS.txt", sep=";")
gdifs <- data.frame("Year"=c(),"Difference"=c(),"Distribution"=c())


for (y in unique(ginis_WORLD_ALL_YEARS$Year)){
  for (d in unique(ginis_WORLD_ALL_YEARS$Distribution)){

  gini_emp <- ginis_WORLD_ALL_YEARS[(ginis_WORLD_ALL_YEARS$Year==y)&(ginis_WORLD_ALL_YEARS$Type=="EMPIRICAL")&(ginis_WORLD_ALL_YEARS$Distribution==d),]$Gini
  gini_synth <- ginis_WORLD_ALL_YEARS[(ginis_WORLD_ALL_YEARS$Year==y)&(ginis_WORLD_ALL_YEARS$Type=="SYNTH")&(ginis_WORLD_ALL_YEARS$Distribution==d),]$Gini
  difference <- 100*abs(gini_emp-gini_synth)/gini_emp
  gdifs <- rbind(gdifs,data.frame("Year"=y,"Difference"=difference,"Distribution"=d))
  }
}
print(paste("Gini Importers Synth: ",
       mean(ginis_WORLD_ALL_YEARS[ginis_WORLD_ALL_YEARS$Distribution=="Importer"&ginis_WORLD_ALL_YEARS$Type=="SYNTH",]$Gini)))
print(paste("Gini Importers Empirical: ",
            mean(ginis_WORLD_ALL_YEARS[ginis_WORLD_ALL_YEARS$Distribution=="Importer"&ginis_WORLD_ALL_YEARS$Type=="EMPIRICAL",]$Gini)))
print(paste("Gini Exporters Synth: ",
            mean(ginis_WORLD_ALL_YEARS[ginis_WORLD_ALL_YEARS$Distribution=="Exporter"&ginis_WORLD_ALL_YEARS$Type=="SYNTH",]$Gini)))
print(paste("Gini Exporters Empirical: ",
            mean(ginis_WORLD_ALL_YEARS[ginis_WORLD_ALL_YEARS$Distribution=="Exporter"&ginis_WORLD_ALL_YEARS$Type=="EMPIRICAL",]$Gini)))

write.table(gdifs,paste0("../results/diffginis.txt"),sep=";",row.names = FALSE)

