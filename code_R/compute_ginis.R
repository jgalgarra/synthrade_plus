# Performs the Lilliefors normality test and computes the Kolmogorov-Smirnov distance between the
# strength filtered empirical network and the synthetic network. Results are stored at results/BestKS.txt and
# results/BestLillies.txt
#
# Computes the Gini coefficients and store them in results/Ginis.txt
#
# Author: Javier Garcia Algarra
#
# Invocation: Rscript compute_gof_distrbutions iniseq finseq 

library(nortest)
library(ineq)
source("read_filter_condition.R")
source("aux_functions_matrix.R")

if (nchar(filtered_string)>1) {
  fcond <- "YES"
} else
  fcond <- "NO"

source("parse_command_line_args.R")


   ini_seq = 2017
   end_seq = 2017

anyos <- seq(ini_seq,end_seq)

dfginis <- data.frame("File"=c(),"Experiment"=c(),"Gini_import"=c(),"Gini_export"=c())

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
    for (i in seq(1,length(experiment_files))){
      print(experiment_files[i])
      other_sim_matrix <- read_and_remove_zeroes(experiment_files[i])
      other_hm_sim <- crea_lista_heatmap(MPack(other_sim_matrix,normalize = TRUE))
      hm_sim_exp <- other_hm_sim[other_hm_sim$type == "EXP",]$cuenta
      hm_sim_imp <- other_hm_sim[other_hm_sim$type == "IMP",]$cuenta
      hm_filt_exp <- hm_filt[hm_filt$type == "EXP",]$cuenta
      hm_filt_imp <- hm_filt[hm_filt$type == "IMP",]$cuenta
      
      # Gini
      Gini_imp <- ineq(hm_sim_imp)
      Gini_exp <- ineq(hm_sim_exp)
      dfginis <- rbind(dfginis,data.frame("File"=experiment_files[i],
                                                "Gini_export" = Gini_exp,
                                                "Gini_import" = Gini_imp))
    }
    # Original Matrix
    hm_filt_exp <- hm_filt[hm_filt$type == "EXP",]$cuenta
    hm_filt_imp <- hm_filt[hm_filt$type == "IMP",]$cuenta
    Gini_imp <- ineq(hm_filt_imp)
    Gini_exp <- ineq(hm_filt_exp)
    dfginis <- rbind(dfginis,data.frame("File"=paste0("RedAdyCom",year,filtered_string,"_W_EMPIRICAL"),
                                        "Gini_export" = Gini_exp,
                                        "Gini_import" = Gini_imp))

    write.table(dfginis,"../results/Ginis.txt",sep="\t",row.names = FALSE)
  }

}
