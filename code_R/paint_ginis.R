library(ggplot2)
Ginis <- read.delim("../results/Ginis.txt")
Ginis$Year <- 0
Ginis$Method <- "SYNTH"
Ginis$MethodLabel <- "SYNTH"
Ginis$Scope <- "GLOBAL"
Ginis$TradeBoost <- 0
Ginis$ImprovedFraction <- 0.0
for (i in 1:nrow(Ginis)){
  texto <- strsplit(strsplit(as.character(Ginis[i,]$File),"RedAdyCom")[[1]][2],"_FILT")
  Ginis$Year[i] <- texto[[1]][1]
  Method <- strsplit(texto[[1]][2],"_W")[[1]][2]
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
Ginis$Method <- ordered(Ginis$Method,levels=c("BOOST_0.98_150",
                                              "BOOST_0.98_100",
                                              "BOOST_0.98_50",
                                              "BOOST_0.98_150_REGIONAL",
                                              "BOOST_0.98_100_REGIONAL",
                                              "BOOST_0.98_50_REGIONAL",
                                              "BOOST_0.97_150_REGIONAL",
                                              "BOOST_0.97_100_REGIONAL",
                                              "BOOST_0.97_50_REGIONAL",
                                              "SYNTH",
                                              "EMPIRICAL"))
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

Ginis_Sel <- Ginis[(Ginis$MethodLabel=="BOOST")&(Ginis$ImprovedFraction==2) | (Ginis$MethodLabel=="SYNTH") ,]
p11 <- ggplot(Ginis_Sel, aes(x = as.numeric(TradeBoost), y = Gini_import, color = Scope)) +
  geom_point(alpha=0.8) + geom_jitter(width=1.5) +
  ylim(c(0.5,1))+ xlab("Boost percentage")+
  # ggtitle("Gini index by year") +
  theme_bw() +
  theme(plot.title = element_text(size = 14,  face = "bold"),
         text = element_text(size = 12),
         axis.title = element_text(face="bold"),
         axis.text.x=element_text(size = 11)) 