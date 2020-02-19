# Paint connectance and number of links plots
#  
# Author: Javier Garcia Algarra
#
# Invocation: Rscript paint_links_and_connectance

library(grid)
library(gridExtra)
library(ggplot2)
library(kcorebip)

file_name <- "RedAdyCom????_FILT.txt"
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

print(paste("Average filetred connectance",mean(nldf$connectance)))

write.table(nldf,"../results/NUMLINKS.txt",row.names = FALSE)

file_name <- "RedAdyDirty????.txt"
data_files <- Sys.glob(paste0("../data/",file_name))
nldf_raw <- data.frame("file"=c(),"year"=c(),"nexp"=c(),
                   "nimp"=c(),"links"=c(), "connectance" = c())
for (i in data_files)
{
  raw_data <- read.table(i)
  max_possible_links <- sum(rowSums(raw_data)>0)*sum(colSums(raw_data)>0)
  nlinks <- sum(raw_data>0)
  nfile <- strsplit(strsplit(i,"data/")[[1]][2],".txt")[[1]][1]
  year <- gsub("_FILT","",strsplit(strsplit(i,"Dirty")[[1]][2],".txt")[[1]][1])
  
  
  unfiltered_file <- gsub("_FILT","",i)
  
  nldf_raw <- rbind(nldf_raw,data.frame("file"=nfile,"year"=year,"links"=nlinks, 
                                "nexp"=sum(rowSums(raw_data)>0),"nimp"=sum(colSums(raw_data)>0),
                                "connectance" = nlinks/max_possible_links))
}

print(paste("Average clean connectance",mean(nldf_raw$connectance)))

write.table(nldf_raw,"../results/NUMLINKS_RAW.txt",row.names = FALSE)

file_name <- "RedAdyCom????.txt"
data_files <- Sys.glob(paste0("../data/",file_name))
nldf_clean <- data.frame("file"=c(),"year"=c(),"nexp"=c(),
                       "nimp"=c(),"links"=c(), "connectance" = c())
for (i in data_files)
{
  clean_data <- read.table(i)
  max_possible_links <- sum(rowSums(clean_data)>0)*sum(colSums(clean_data)>0)
  nlinks <- sum(clean_data>0)
  nfile <- strsplit(strsplit(i,"data/")[[1]][2],".txt")[[1]][1]
  year <- gsub("_FILT","",strsplit(strsplit(i,"Com")[[1]][2],".txt")[[1]][1])
  
  
  unfiltered_file <- gsub("_FILT","",i)
  
  nldf_clean <- rbind(nldf_clean,data.frame("file"=nfile,"year"=year,"links"=nlinks, 
                                        "nexp"=sum(rowSums(clean_data)>0),"nimp"=sum(colSums(clean_data)>0),
                                        "connectance" = nlinks/max_possible_links))
}

print(paste("Average raw connectance",mean(nldf_clean$connectance)))

write.table(nldf_raw,"../results/NUMLINKS_CLEAN.txt",row.names = FALSE)



nldf_raw$Set <- "Raw Data"
nldf_clean$Set <- "Clean Data"
nldf$Set <- "Filtered Data"

nldf_all <- rbind(nldf,nldf_clean,nldf_raw)
nldf_all$Set <- as.factor(nldf_all$Set)

p <- ggplot(data=nldf_all)+
  ggtitle("")+xlab("Year")+ylab("Connectance")+
  geom_point(aes(x=year,y=connectance,fill=Set),color="transparent",shape=21, size=2, alpha = 0.5)+ylim(c(0,0.7))+
  theme_bw() +
  theme(
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=13,lineheight=.5, face="bold",hjust = 0.5),
        axis.text.x = element_text(size=9,angle=70,hjust=1,face="bold"),
        axis.text.y = element_text(size=9, face="bold"),
        axis.title.x = element_text(face="bold", size=13),
        axis.title.y  = element_text(face="bold", size=13) )

nldf_links <- rbind(nldf_raw,nldf_clean,nldf)
nldf_links$Set <- as.factor(nldf_links$Set)
q <- ggplot(data=nldf_links)+
  ggtitle("")+xlab("Year")+ylab("Number of links")+
  geom_line(aes(x=year,y=links,group=Set,colour=Set),size=2, alpha = 0.5)+
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=10, face="bold"),
    plot.title = element_text(size=13,lineheight=.5, face="bold",hjust = 0.5),
    axis.text.x = element_text(size=9,angle=70,hjust=1,face="bold"),
    axis.text.y = element_text(size=9, face="bold"),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y  = element_text(face="bold", size=13) )


ppi <- 300
p <- p + scale_fill_manual(values=c("blue","red","green"))+
  scale_color_manual(values=c("blue","red","green"))

q <- q + scale_fill_manual(values=c("blue","red","green"))+
  scale_color_manual(values=c("blue","red","green"))

fsal3 <- paste0("../figures/tests/links_all.png")
png(fsal3, width=10*ppi, height=5*ppi, res=ppi)
grid.arrange(q, ncol=1, nrow=1 )
dev.off()

fsal2 <- paste0("../figures/tests/connectance_all.png")
png(fsal2, width=10*ppi, height=5*ppi, res=ppi)
grid.arrange(p, ncol=1, nrow=1 )
dev.off()

