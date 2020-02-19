source("parse_command_line_args.R")

ISO_CC <- c("AFG","ALB","DZA","ASM","AND","AGO","AIA","ATA","ATA","ARG",
            "ARM","ABW","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL",
            "BLZ","BEN","BMU","BTN","BOL","BES","BIH","BWA","IOT","VGB",
            "BRA","BRN","BGR","BFA","BDI","CPV","KHM","CMR","CAN",
            "CYM","CAF","TCD","CHL","CHN","HKG","MAC","CXR","CCK","COL","COM","COG",
            "COK","CRI","CIV","HRV","CUB","CUW","CYP","CZE","CSK","PRK","COD","DNK",
            "DJI","DMA","DOM","PAK","ECU","EGY","SLV","GNQ","ERI","EST","ETH",
            "FRO","FLK","FJI","FIN","YEM","DDR","VDR","YMD",
            "DEU","PCI","PAN","PCZ","VNM","SDN","SUN","YUG","ATF",
            "FRA","GUF","PYF","FSM","GAB","GMB","GEO","DEU","GHA","GIB","GRC",
            "GRL","GRD","GLP","GUM","GTM","GIN","GNB","GUY","HTI","HMD","VAT","HND",
            "HUN","ISL","IND","IND","IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN",
            "JOR","KAZ","KEN","KIR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR",
            "LBY","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MHL","MTQ","MRT",
            "MUS","MYT","MEX","MNG","MNE","MSR","MAR","MOZ","MMR","MNP","NAM","NRU",
            "NPL","ANT","NLD","NCL","NZL","NIC","NER","NGA","NIU","NFK",
            "NOR","OMN","PAK","PLW","PAN","PNG","PRY",
            "PER","PHL","PCN","POL","PRT","QAT","KOR","MDA","REU","ROU","RUS",
            "RWA","BLM","SHN","KNA","KNA","LCA","SXM","SPM","VCT","WSM",
            "SMR","STP","SAU","SEN","SRB","SCG","SYC","SLE","SGP","SVK",
            "SVN","ZAF","SLB","SOM","ZAF","SGS","SSD","ESP","LKA","PSE","SDN",
            "SUR","SWZ","SWE","CHE","SYR","TJK","MKD","THA","TLS","TGO","TKL","TON",
            "TTO","TUN","TUR","TKM","TCA","TUV","UGA","UKR","ARE","GBR","TZA","UMI",
            "URY","VIR","USA","USA","UZB","VUT","VEN","VNM","WLF","ESH",
            "YEM","ZMB","ZWE")

new1984 <- toupper(c("aia","and","ant","atg","brn","btn","cck","cok","com","cpv","cxr","cym","dma","esh","fro","grd","iot","lca","mdv","msr","mtq","nfk","niu","nru","pci","pcn","pyf","reu","slb","stp","tca","ton","tuv","vct","vgb","vut","wlf","yem"))


files <- seq(ini_seq,end_seq)
for (nfile in files)
{
  print(nfile)


  ctdata <- read.csv(paste0("../data/raw_data/TodosYR",nfile,".csv"), sep=";")
  outdata <- data.frame("or"=ctdata$origin,"dest"=ctdata$dest,"imp"=ctdata$export_val)
  write.table(outdata,paste0("../data/raw_data/RedRaw",nfile,".txt"),sep=" ",row.names = FALSE, col.names = FALSE)
  raw_data <- read.table(paste0("../data/raw_data/RedRaw",nfile,".txt"))
  names(raw_data) <- c("or","dest","imp")
  raw_data$or <- toupper(raw_data$or)
  raw_data$dest <- toupper(raw_data$dest)
  clean_data <- raw_data[is.element(raw_data$or,ISO_CC),]
  clean_data <- clean_data[is.element(clean_data$dest,ISO_CC),]    # remove aggregations
  dirty_data <- clean_data                                         # Keep 1984 islands
  clean_data <- clean_data[!is.element(clean_data$dest,new1984),]  # remove set of small islands added in 1984
  clean_data <- clean_data[!is.element(clean_data$or,new1984),]
  countries1 <- unique(clean_data$or)
  countries2 <- unique(clean_data$dest)
  if (length(countries1) > length(countries2))
    countries = countries1
  else
    countries = countries2
  dim = length(countries)
  B <- matrix( rep(0,dim*dim), nrow=dim, ncol=dim)
  dfsal = as.data.frame(B)
  rownames(dfsal) <- countries
  colnames(dfsal) <- countries
  for (j in 1:nrow(clean_data))
    dfsal[as.character(clean_data$or[j]),as.character(clean_data$dest[j])] <- as.numeric(clean_data$imp[j])
  write.csv(dfsal,paste0("../data/RedAdyNames",nfile,".txt"),row.names = TRUE)
  write.table(dfsal,paste0("../data/RedAdyCom",nfile,".txt"),row.names = FALSE, col.names = FALSE, sep ="\t")

  countries1 <- unique(dirty_data$or)
  countries2 <- unique(dirty_data$dest)
  if (length(countries1) > length(countries2))
    countries = countries1
  else
    countries = countries2
  dim = length(countries)
  B <- matrix( rep(0,dim*dim), nrow=dim, ncol=dim)
  dfsal = as.data.frame(B)
  rownames(dfsal) <- countries
  colnames(dfsal) <- countries
  for (j in 1:nrow(dirty_data))
    dfsal[as.character(dirty_data$or[j]),as.character(dirty_data$dest[j])] <- as.numeric(dirty_data$imp[j])
  write.table(dfsal,paste0("../data/RedAdyDirty",nfile,".txt"),row.names = FALSE, col.names = FALSE, sep ="\t")
}