library(grid)
library(gridExtra)
library(ggplot2)
library(kcorebip)

file_name <- "RedAdyCom????.txt"
data_files <- Sys.glob(paste0("../data/",file_name))
nldf <- data.frame("file"=c(),"year"=c(),"nexp"=c(),
                  "nimp"=c(),"links"=c(), "connectance" = c())
for (i in data_files)
{
  raw_data <- read.table(i)
  max_possible_links <- sum(rowSums(raw_data)>0)*sum(colSums(raw_data)>0)
  nlinks <- sum(raw_data>0)
  nfile <- strsplit(strsplit(i,"data/")[[1]][2],".txt")[[1]][1]
  year <- gsub("_FILT","",strsplit(strsplit(i,"Com")[[1]][2],".txt")[[1]][1])
  
  
  unfiltered_file <- gsub("_FILT","",i)
  
  nldf <- rbind(nldf,data.frame("file"=nfile,"year"=year,"links"=nlinks, 
                                "nexp"=sum(rowSums(raw_data)>0),"nimp"=sum(colSums(raw_data)>0),
                                "connectance" = nlinks/max_possible_links))
}

plot(nldf$year,nldf$connectance,main="Connectance")
plot(nldf$year,nldf$links,main="Number of Links")
print(paste("Average connectnce",mean(nldf$connectance)))

write.table(nldf,"../results/NUMLINKS_RAW.txt",row.names = FALSE)

p <- ggplot(data=nldf)+
  ggtitle("")+xlab("Year")+ylab("Connectance")+
  geom_point(aes(year,connectance), alpha = 0.5)+
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size=13, face="bold"),
        plot.title = element_text(size=13,lineheight=.5, face="bold",hjust = 0.5),
        axis.text.x = element_text(size=10,angle=70,hjust=1,face="bold"),
        axis.text.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(face="bold", size=13),
        axis.title.y  = element_text(face="bold", size=13) )+coord_flip()

fsal2 <- paste0("../figures/tests/connectance_raw.png")
ppi <- 600
png(fsal2, width=4*ppi, height=8*ppi, res=ppi)
grid.arrange(p, ncol=1, nrow=1 )
dev.off()