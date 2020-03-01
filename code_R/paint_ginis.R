library(ggplot2)
Ginis <- read.delim("../results/Ginis.txt")
Ginis$Year <- 0
Ginis$Method <- "SYNTH"
for (i in 1:nrow(Ginis)){
  texto <- strsplit(strsplit(as.character(Ginis[i,]$File),"RedAdyCom")[[1]][2],"_FILT")
  Ginis$Year[i] <- texto[[1]][1]
  Method <- strsplit(texto[[1]][2],"_W")[[1]][2]
  if (grepl("FBAL",Method))
    Ginis$Method[i] <- strsplit(strsplit(Method,"FBAL_")[[1]][2],".txt")[[1]][1]
  if (grepl("EMPIRICAL",Method))
      Ginis$Method[i] <- "EMPIRICAL"
}
Ginis$Method <- as.factor(Ginis$Method)
Ginis$Method <- ordered(Ginis$Method,levels=c("PRIME_0.98_150",
                                              "PRIME_0.98_100",
                                              "PRIME_0.98_50",
                                              "PRIME_0.98_150_REGIONAL",
                                              "PRIME_0.98_100_REGIONAL",
                                              "PRIME_0.98_50_REGIONAL",
                                              "SYNTH",
                                              "EMPIRICAL"))
boxplot(Gini_export ~ Method, data = Ginis)

p10 <- ggplot(Ginis, aes(x = Year, y = Gini_import, fill = Method)) +
  geom_boxplot(alpha=0.7) +
  # scale_y_continuous(name = "Mean ozone in\nparts per billion",
  #                    breaks = seq(0, 175, 25),
  #                    limits=c(0, 175)) +
  scale_x_discrete(name = "Year") + ylim(c(0.5,1))+
  ggtitle("Gini index by year") +
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Accent")
p10