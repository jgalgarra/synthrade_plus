library(grid)
library(gridExtra)
library(ggplot2)


xperiment_files <- Sys.glob(paste0("../nestedness/*Com*_nestvalues.csv"))
df_all_nestvalues <- data.frame("wine"=c(),"exper"=c(),"year"=c())
for (i in xperiment_files){
  data_file <- read.table(i,sep=";",header=TRUE);
  year <- strsplit(strsplit(i,"Com")[[1]][2],"_nestvalues")[[1]][1]
  data_file$year <- year
  df_all_nestvalues <- rbind(df_all_nestvalues,data_file)
}
df_empirical_nestvalues <- df_all_nestvalues[df_all_nestvalues$exper==0,]
df_unfiltered_nestvalues <- df_all_nestvalues[df_all_nestvalues$exper==-1,]
df_synthetic_nestvalues <- df_all_nestvalues[df_all_nestvalues$exper>0,]
p <- ggplot(df_synthetic_nestvalues, aes(as.factor(year),as.numeric(wine)))
p <- p + geom_boxplot() + theme_bw() + ylim(c(0,1))
p <- p + geom_boxplot(data=df_empirical_nestvalues,aes(as.factor(year),as.numeric(wine)),color="red")

q <- ggplot(df_synthetic_nestvalues, aes(as.factor(year),as.numeric(wine)))
q <- q + geom_point(color="blue",alpha=0.1) + theme_bw() + ylim(c(0.7,1))
q <- q + geom_boxplot(data=df_empirical_nestvalues,aes(as.factor(year),as.numeric(wine)),color="red")
q <- q + geom_boxplot(data=df_unfiltered_nestvalues,aes(as.factor(year),as.numeric(wine)),color="green")


df_diffs <- df_empirical_nestvalues
df_diffs$diff <- 0
for (i in 1:nrow(df_diffs))
  df_diffs$diff[i] = 100*(mean(df_synthetic_nestvalues[df_synthetic_nestvalues$year==df_diffs$year[i],]$wine)-
                          df_diffs$wine[i])/df_diffs$wine[i]

theme_set(theme_bw())

df_diffs$type <- ifelse(df_diffs$diff < 0, "below", "above")  #
r <- ggplot(df_diffs, aes(x=year, y=diff, label=diff)) + 
  geom_bar(stat='identity', aes(fill=type),  width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(title= "WINE difference percentage") +
  theme(legend.position = "none")