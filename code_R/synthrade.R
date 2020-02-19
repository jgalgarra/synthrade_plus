# Synthetic trade network creation
#
# Author: Javier Garcia Algarra
#
# This is the main script of the synthetic trade network simulator
# Invocation Rscript synthetic_model iniseq finseq numexper
#                    iniseq : Initial year
#                    finseq : Final year
#                    numexper: Number of experiments
#
# Filtering conditions at filtered_condition.txt file
#            First number: 1 (Use filtered file)/ 0 (use raw file)
#            Second number: 1 (Append log to symlog file) / 0 (Do not write log data)
#            Third number: 0 (Write links/tokens/probs evolution at numlinks folder. It slows down simulation)/ 0 do not append
#
# ROWS: Exporters, COLUMNS: Importers


source("parse_command_line_args.R")
source("read_filter_condition.R")


ReadMatrix <- function(year){
  original_file <- read.delim(paste0("../data/RedAdyCom",year,filtered_string,".txt"), header=FALSE)
  or_matrix <- as.matrix(original_file)
  # Clean rows and cols full of zeroes
  clean_matrix <- or_matrix[,colSums(or_matrix) > 0]
  clean_matrix <- clean_matrix[rowSums(clean_matrix) > 0,]
  return(clean_matrix)
}

NodeAttachment <- function(vecprob,lvec)
{
  listanodes = c()
  for (i in 1:lvec){
    if (vecprob[i] != 0)
      if (rbinom(1,1,vecprob[i])>0)
        listanodes <- append(listanodes,i)
  }
  return(listanodes)
}

UpdatableLinks <- function(matrixprob){  
  links_found = FALSE
  listaedges = c()
  tam = c(nrow(matrixprob),ncol(matrixprob))
  newlinks <- 0
  randunif <- runif(tam[1]*tam[2],0,1)
  randunif = matrix(randunif,nrow=tam[1],ncol=tam[2])
  newlinks = randunif < matrixprob
  positions <- which(newlinks !=0, arr.ind = T)
  if (sum(newlinks) == 0)
    return(c())
  else
    return(positions)
}

SynthMatrix <- function(matrixemp, year){
  n_imp <- ncol(matrixemp)
  n_exp <- nrow(matrixemp)
  numlinks <- sum(matrixemp > 0)
  totweight <- sum(matrixemp)
  print(paste("exporters",n_exp,"importers",n_imp,"numlinks",numlinks))
  
  # Create a synthetic matrix full of zeroes
  msynth <- matrix(rep(0.0,n_imp*n_exp), nrow = n_exp, byrow = TRUE)
  seed_size <- 3
  exp_max <- seed_size
  imp_max <- seed_size
  min_token <- 1
  msynth[1,2] <- min_token 
  msynth[1,3] <- min_token
  msynth[2,1] <- min_token
  msynth[2,3] <- min_token
  msynth[3,1] <- min_token
  msynth[3,2] <- min_token
  cuenta_token <- sum(msynth)
  cuenta_links <- sum(msynth > 0)
  min_links <- cuenta_links
  texp <- n_exp*(n_exp-seed_size)/(2*seed_size)
  timp <- n_imp*(n_imp-seed_size)/(2*seed_size)
  
  tf <- max(texp,timp)

  lambda_exp = n_exp*(n_exp-seed_size)/(2*tf)
  lambda_imp = n_imp*(n_imp-seed_size)/(2*tf)
  
  print(paste("lambda imp:",lambda_imp,"lambda_exp",lambda_exp))
  Pr_E <- rowSums(msynth)/sum(msynth)
  Pr_I <- colSums(msynth)/sum(msynth)
  void_prob <- matrix(rep(0.0,exp_max*imp_max), nrow = imp_max, byrow = TRUE)
  prob_new_links <- void_prob
  morenewnodes <- TRUE
  cuenta_antciclo <- 0
  last_logged_step <- 0
  tf <- 0
  sim_step <- 0
  dir.create("../results", showWarnings = FALSE)
  dir.create("../results/numlinks", showWarnings = FALSE)
  fich_links <- paste0("../results/numlinks/numlinks_",lyear,filtered_string,"_W_",nexper,".txt")
  if (cuenta_links)
    unlink(fich_links)
  
  while ((morenewnodes)|| (cuenta_links < numlinks))
  {
    sim_step <- sim_step + 1
    if ((!morenewnodes) && (tf == 0)){
      tf <- cuenta_links
      print(paste("Build up time",sim_step,"numlinks",tf,100*cuenta_links/numlinks))
      if (append_log){
        con <- file("../results/symlog.txt", "a")
        cat(paste0("FT;",lyear,";",nexper,";",sim_step,";",cuenta_token,";",cuenta_links,";",numlinks,"\n"), file=con)
        close(con)
      }
      # Write probability matrix at that instant
      dir.create("../results/probs", showWarnings = FALSE)
      write.table(prob_new_links,paste0("../results/probs/PR_TF_RedAdyCom",lyear,filtered_string,"_W_",nexper,".txt"),
                  row.names = FALSE, col.names = FALSE, sep = "\t")
      # Write weight matrix at that instant
      dir.create("../results/TFMatrix", showWarnings = FALSE)
      write.table(msynth,paste0("../results/TFMatrix/TF_RedAdyCom",lyear,filtered_string,"_W_",nexper,".txt"),
                  row.names = FALSE, col.names = FALSE, sep = "\t")
    }
    new_node <- FALSE
    # Write number of links file. 
    if (write_num_links){
      if ((((morenewnodes) & (sim_step - last_logged_step >= 20)) | (sim_step %% 500 == 0))){
          fn <- fivenum(log(prob_new_links[prob_new_links>0]))
          ultprob <- min(log(prob_new_links[which(msynth==min(msynth[msynth>0]),arr.ind=TRUE)]))
          prob_vacios <- log(1-sum(prob_new_links[which(msynth>0,arr.ind=TRUE)]))
          varsigma <- exp(sd(log(prob_new_links)))
          meanprob <- mean(log(prob_new_links))
          con <- file(fich_links, "a")
          cat(paste0(sim_step,";",cuenta_links,";",cuenta_token,";",exp_max,";",imp_max,";",
                     fn[1],";",fn[2],";",fn[3],";",fn[4],";",fn[5],";",
                     ultprob,";",prob_vacios,";",meanprob,";",varsigma,"\n"), file=con)
          close(con)
          last_logged_step <- sim_step
      }
    }
    if (cuenta_antciclo != cuenta_links){
      cuenta_antciclo <- cuenta_links
      if (cuenta_links %% 1000 == 0){
        print(paste(cuenta_links,"links out of",numlinks,
                    "exporters",exp_max,"out of",n_exp,"importers",
                    imp_max,"out of",n_imp,"tokens",cuenta_token))
        }
    }
    if (exp_max < n_exp)
      if (rbinom(1,1,min(1,lambda_exp/exp_max))>0)
      {
        linkstoI <- c()
        while (length(linkstoI) == 0)
          linkstoI <- NodeAttachment(Pr_I,imp_max)
        for (i in linkstoI)
          if ((cuenta_links < numlinks) && (exp_max < n_exp)){
            exp_max <- exp_max + 1
            msynth[exp_max,i] <- 1#/length(linkstoI)
            cuenta_links <-  cuenta_links + 1
            cuenta_token <- cuenta_token + 1
            new_node <- TRUE
          }
      }
    
    if (imp_max < n_imp)
      if (rbinom(1,1,min(1,lambda_imp/imp_max))>0)
      {
        linkstoE <- c()
        while (length(linkstoE) == 0)
          linkstoE <- NodeAttachment(Pr_E,exp_max)
        for (i in linkstoE)
          if ((cuenta_links < numlinks) && (imp_max < n_imp)){
            imp_max <- imp_max + 1
            msynth[i,imp_max] <- 1
            cuenta_token <- cuenta_token + 1
            cuenta_links <-  cuenta_links + 1
            new_node <- TRUE
          }
      }
    
    if (new_node){
      smat <- sum(msynth)
      Pr_E <- rowSums(msynth)/smat
      Pr_I <- colSums(msynth)/smat
      prob_new_links <- t(Pr_I[1:imp_max] %o% Pr_E[1:exp_max])
      sim_step <- sim_step + 1    # Modification to avoid throwing a token when there is a new node
    }
    else if (cuenta_links > min_links) {
      update_links <- UpdatableLinks(prob_new_links)
      while(length(update_links) == 0) {
        sim_step = sim_step + 1
        update_links <- UpdatableLinks(prob_new_links)
      } 
      lupdate <- (length(update_links)/2)
      for (m in 1:lupdate)
        if (cuenta_links < numlinks) {
          rowl <- update_links[m,1]
          coll <- update_links[m,2]
          msynth[rowl,coll] <- msynth[rowl,coll] + 1
          cuenta_token <- cuenta_token + 1
        }
    }
    
    cuenta_links <- sum(msynth > 0)
    smat <- sum(msynth)
    Pr_E <- rowSums(msynth)/smat
    Pr_I <- colSums(msynth)/smat
    prob_new_links <- t(Pr_I[1:imp_max] %o% Pr_E[1:exp_max])
    morenewnodes <- (exp_max < n_exp) || (imp_max < n_imp)
  }
  # Write probability matrix at that instant
  write.table(prob_new_links,paste0("../results/probs/PR_TT_RedAdyCom",lyear,filtered_string,"_W_",nexper,".txt"),
              row.names = FALSE, col.names = FALSE, sep = "\t")
  if (append_log){
    con <- file("../results/symlog.txt", "a")
    cat(paste0("TT;",lyear,";",nexper,";",sim_step,";",cuenta_token,";",cuenta_links,";",numlinks,"\n"), file=con)
    close(con)
  }
  return(msynth)
}

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0){
  maxexper <- 1
} else{
  ini_seq <- as.numeric(args[1])
  end_seq <- as.numeric(args[2])
  maxexper <- as.numeric(args[3]) 
}

years <- seq(ini_seq,end_seq)

for (lyear in years)
  for (nexper in seq(1,maxexper)){
    print(paste(lyear,"Experiment",nexper))
    matrix_emp <- ReadMatrix(lyear)
    nlinks <- sum(matrix_emp>0)
    matrix_experiment <- SynthMatrix(matrix_emp,lyear)
    write.table(matrix_experiment,paste0("../results/RedAdyCom",lyear,filtered_string,"_W_",nexper,".txt"),
                row.names = FALSE, col.names = FALSE, sep = "\t")
  }