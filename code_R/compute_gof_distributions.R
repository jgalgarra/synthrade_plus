# Performs the Lilliefors normality test and computes the Kolmogorov-Smirnov distance between the
# strength filtered empirical network and the synthetic network. Results are stored at results/BestKS.txt and
# results/BestLillies.txt
#
# Author: Javier Garcia Algarra
#
# Invocation: Rscript compute_gof_distrbutions iniseq finseq 

library(nortest)
source("read_filter_condition.R")
source("aux_functions_matrix.R")

if (nchar(filtered_string)>1) {
  fcond <- "YES"
} else
  fcond <- "NO"

source("parse_command_line_args.R")

anyos <- seq(ini_seq,end_seq)

dfbestlillies <- data.frame("Year"=c(),"Experiment"=c())
dfbestkolmogorov <- data.frame("Year"=c(),"Experiment"=c(),"geom_mean"=c(),"KS_import"=c(),"KS_export"=c())
dfkolmogorov <- data.frame("Year"=c(),"Experiment"=c(),"KSimport"=c(),"KSexport"=c())
dflillies <- data.frame("Year"=c(),"Experiment"=c(),"Geom_mean"=c(),
                                                "Synthetic_exporter" = c(),
                                                "Synthetic_importer" = c(),
                                                "Empirical_exporter_filtered"=c(),
                                                "Empirical_importer_filtered"=c(),
                                                "Empirical_exporter_unfiltered"=c(),
                                                "Empirical_importer_unfiltered"=c())


for (year in anyos){
  
  unfilt_name <- paste0("RedAdyCom",year)
  file_name <- paste0("RedAdyCom",year,filtered_string)
  file_orig <- paste0("RedAdyCom",year)
  experiment_files <- Sys.glob(paste0("../results/",file_name,"_W_*.txt"))
  if (length(experiment_files)>0){
    
    filt_matrix <- read_and_remove_zeroes(paste0("../data/",file_name,".txt"))
    
    unfilt_matrix <- read_and_remove_zeroes(paste0("../data/",unfilt_name,".txt"))
    orig_matrix <- read_and_remove_zeroes(paste0("../data/",file_orig,".txt"))
    sim_matrix <- read_and_remove_zeroes(experiment_files[1])
    hm_unfilt <- crea_lista_heatmap(MPack(unfilt_matrix,normalize = TRUE))
    hm_filt <- crea_lista_heatmap(MPack(filt_matrix,normalize = TRUE))
    hm_filt <- hm_filt[hm_filt$cuenta>0,]
    hm_sim <- crea_lista_heatmap(MPack(sim_matrix,normalize = TRUE))
    maxlilliescore <- 0
    poslilliescore <- 0
    for (i in seq(1,length(experiment_files))){
      other_sim_matrix <- read_and_remove_zeroes(experiment_files[i])
      other_hm_sim <- crea_lista_heatmap(MPack(other_sim_matrix,normalize = TRUE))
      hm_sim_exp <- other_hm_sim[other_hm_sim$type == "EXP",]$cuenta
      hm_sim_imp <- other_hm_sim[other_hm_sim$type == "IMP",]$cuenta
      plillieexp <- lillie.test(log(hm_sim_exp))$p.value
      plillieimp <- lillie.test(log(hm_sim_imp))$p.value
      plillie <- sqrt(plillieexp*plillieimp)
      if (plillie > maxlilliescore){
        maxlilliescore <- plillie
        poslilliescore <- i
        maxlillieexp <- plillieexp
        maxlillieimp <- plillieimp
      }
      ll_filt_exp <- lillie.test(log(hm_filt[hm_filt$type == "EXP",]$cuenta))$p.value
      ll_filt_imp <- lillie.test(log(hm_filt[hm_filt$type == "IMP",]$cuenta))$p.value
      hm_unfilt <- hm_unfilt[hm_unfilt$cuenta>0,]
      ll_unfilt_exp <- lillie.test(log(hm_unfilt[hm_unfilt$type == "EXP",]$cuenta))$p.value
      ll_unfilt_imp <- lillie.test(log(hm_unfilt[hm_unfilt$type == "IMP",]$cuenta))$p.value
      
      
      dflillies <- rbind(dflillies,data.frame("Year"=year,"Experiment"=i,"Geom_mean"=plillie,
                                             "Synthetic_exporter" = plillieexp,
                                             "Synthetic_importer" = plillieimp,
                                             "Empirical_exporter_filtered"=ll_filt_exp,
                                             "Empirical_importer_filtered"=ll_filt_imp,
                                             "Empirical_exporter_unfiltered"=ll_unfilt_exp,
                                             "Empirical_importer_unfiltered"=ll_unfilt_imp))
      
      hm_filt_exp <- hm_filt[hm_filt$type == "EXP",]$cuenta
      hm_filt_imp <- hm_filt[hm_filt$type == "IMP",]$cuenta
      
      # Kolmogorov test
      x <- log(hm_sim_imp)
      y <- log(hm_filt_imp)
      ks_imp_pvalue <- ks.test(x, y)$p.value
      v <- log(hm_sim_exp)
      w <- log(hm_filt_exp)
      ks_exp_pvalue <- ks.test(v, w)$p.value
      dfkolmogorov <- rbind(dfkolmogorov, data.frame("Year"=year,"Experiment"=i,
                                                     "KSimport"=ks_imp_pvalue,
                                                     "KSexport"=ks_exp_pvalue))
    }
    print(paste("Year",year,"Experiment",poslilliescore))
    dfbestlillies <- rbind(dfbestlillies,data.frame("Year"=year,"Experiment"=poslilliescore,
                                                    "Geom_mean"=sqrt(maxlillieexp*maxlillieimp),
                                                    "Synthetic_exporter" = maxlillieexp,
                                                    "Synthetic_importer" = maxlillieimp,
                                                    "Empirical_exporter_filtered"=ll_filt_exp,
                                                    "Empirical_importer_filtered"=ll_filt_imp,
                                                    "Empirical_exporter_unfiltered"=ll_unfilt_exp,
                                                    "Empirical_importer_unfiltered"=ll_unfilt_imp))
    if (fcond == "YES")
      write.table(dfbestlillies,"../results/BestLillies.txt",sep="\t",row.names = FALSE)
    else
      write.table(dfbestlillies,"../results/BestLilliesUnfiltered.txt",sep="\t",row.names = FALSE)
    write.table(dfkolmogorov,"../results/KSTEST.txt",sep="\t",row.names = FALSE)
    write.table(dflillies,"../results/Lillies.txt",sep="\t",row.names = FALSE)
  }

}
# Find best KS test results
for (year in anyos){
  dyear <- dfkolmogorov[dfkolmogorov$Year == year,]
  dyear$geom_mean <- sqrt(dyear$KSimport*dyear$KSexport)
  posbest <- which(dyear$geom_mean == max(dyear$geom_mean))[1]
  dfbestkolmogorov <- rbind(dfbestkolmogorov,data.frame("Year" = year,"Experiment" = posbest,
                                                        "geom_mean"= dyear$geom_mean[posbest],
                                                        "KS_import" = dyear$KSimport[posbest],
                                                        "KS_export"= dyear$KSexport[posbest]
                                                        ))
}
if (fcond == "YES"){
  write.table(dfbestkolmogorov,"../results/BestKS.txt",sep="\t",row.names = FALSE)
} else
  write.table(dfbestkolmogorov,"../results/BestKSUnfiltered.txt",sep="\t",row.names = FALSE)
