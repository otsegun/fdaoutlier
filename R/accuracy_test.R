source("muod_cpp.R") 
# load model  functions
source("sim_models_w_plots.R")
# load other methods
source("competencia.R") 

model_analysis <- function(reps = 400, model  = paste0("model", 1:6)){
  n <- c(100, 300, 1000)
  p <- c(10, 50, 100)
  alpha <-  .05
  options("mc.cores" = 8) # Let's use 4 cores
  res_data <- data.frame()
  
  model <- match.arg(model)
  
  # initialize model
  if (model == "model1"){
    modelt <- model1
  } else if(model == "model2") {
    modelt <- model2
  } else if(model == "model3") {
    modelt <- model3
  } else if(model == "model4") {
    modelt <- model4
  } else if(model == "model5") {
    modelt <- model5
  } else if(model == "model6") {
    modelt <- model6
  }
  
  cat("Creating folder for model", model, "\n")
  dir.create(paste0(model,"_data"))
  
  for(i in n){
    for(j in p){
      for(k in alpha){
        cat("Model:", model, "working on:- ", "n = ", i, "p = ", j, "alpha = ", k,  "\n")
        #initialize sentivities
        sens_fast <- c()
        sens_semifast <- c()
        sens_semifast25 <- c()
        sens_normal <- c()
        
        # sens_fast_log <- c()
        # sens_semifast_log <- c()
        # sens_semifast25_log <- c()
        # sens_normal_log <- c()
        
        sens_fbplot <- c()
        sens_outlgrm <- c()
        sens_msplot <- c()
        sens_fom <- c()
        
        
        # initialize false positive rates
        fprs_fast <- c()
        fprs_semifast <- c()
        fprs_semifast25 <- c()
        fprs_normal <- c()
        
        # fprs_fast_log <- c()
        # fprs_semifast_log <- c()
        # fprs_semifast25_log <- c()
        # fprs_normal_log <- c()
        # 
        fprs_fbplot <- c()
        fprs_outlgrm <- c()
        fprs_msplot <- c()
        fprs_fom <- c()
        
        
        # initliaze f1 scores
        f1s_fast <- c()
        f1s_semifast <- c()
        f1s_semifast25 <- c()
        f1s_normal <- c()
        
        # f1s_fast_log <- c()
        # f1s_semifast_log <- c()
        # f1s_semifast25_log <- c()
        # f1s_normal_log <- c()
        
        #        f1s_normal_box <- c()
        f1s_fbplot <- c()
        f1s_outlgrm <- c()
        f1s_msplot <- c()
        f1s_fom <- c()
        
        
        for (o in 1 : reps) {
          cat(":-:-:-:- Working on rep :-", o, "\n")
          # generate data
          dt_tmp <- modelt(n = i, p = j, out.rate = k)
          dt <- dt_tmp$data
          # cache
          #use methods on data
          muod_fast <- get_outliers(t(dt), method = "fast", cut_method = "boxplot",  n_core = 1)
          muod_semifast <- get_outliers(t(dt), method = "semifast", cut_method = "boxplot", n_core = 8)
          muod_semifast25 <- get_outliers(t(dt), method = "semifast", cut_method = "boxplot", n_core = 8, sample_prop = 0.25)
          muod_normal <- get_outliers (t(dt), method = "rcpp", cut_method = "boxplot",  n_core = 8)
          
          # muod_fast_log <- get_outliers(t(dt), method = "fast", cut_method = "log_transform",  n_core = 1)
          # muod_semifast_log <- get_outliers(t(dt), method = "semifast", cut_method = "log_transform", n_core = 8)
          # muod_semifast25_log <- get_outliers(t(dt), method = "semifast", cut_method = "log_transform",
          #                                     n_core = 8, sample_prop = 0.25)
          # muod_normal_log <- get_outliers (t(dt), method = "rcpp", cut_method = "log_transform",  n_core = 8)
          
          outl_fbplot <-  get_fbplot_outliers(t(dt))
          outl_outlgrm <-  get_outliergram_outliers(dt)
          outl_msplot <- get_msplot_outliers(dt)
          outl_fom <- get_fom_outliers(dt)
          
          
          
          # Compute  accuracy metrics for all methods
          
          ## create real positions of outliers
          outl_pos_real <- rep(0, i)
          outl_pos_real[dt_tmp$true_outliers] <- 1
          outl_pos_real <- factor(outl_pos_real, levels = c("0","1"))
          
          ## get outlier coordinates by muod methods
          outl_fast <- unlist(unname(muod_fast))
          outl_semifast <- unlist(unname(muod_semifast))
          outl_semifast25 <- unlist(unname(muod_semifast25))
          outl_normal <- unlist(unname(muod_normal))
          
          # outl_fast_log <- unlist(unname(muod_fast_log))
          # outl_semifast_log <- unlist(unname(muod_semifast_log))
          # outl_semifast25_log <- unlist(unname(muod_semifast25_log))
          # outl_normal_log <- unlist(unname(muod_normal_log))
          
          #          outl_normal_box <- unlist(unname(muod_normal_box))
          ###remove duplicates
          outl_fast <- outl_fast[!duplicated(outl_fast)]
          outl_semifast <- outl_semifast[!duplicated(outl_semifast)]
          outl_semifast25 <- outl_semifast25[!duplicated(outl_semifast25)]
          outl_normal <- outl_normal[!duplicated(outl_normal)]
          
          # outl_fast_log <- outl_fast_log[!duplicated(outl_fast_log)]
          # outl_semifast_log <- outl_semifast_log[!duplicated(outl_semifast_log)]
          # outl_semifast25_log <- outl_semifast25_log[!duplicated(outl_semifast25_log)]
          # outl_normal_log <- outl_normal_log[!duplicated(outl_normal_log)]
          
          
          ## create predicted target by muod methods
          outl_pos_pred_fast <- rep(0, i)
          outl_pos_pred_fast[outl_fast] <- 1
          outl_pos_pred_fast <- factor(outl_pos_pred_fast, levels = c("0","1"))
          
          # outl_pos_pred_fast_log <- rep(0, i)
          # outl_pos_pred_fast_log[outl_fast_log] <- 1
          # outl_pos_pred_fast_log <- factor(outl_pos_pred_fast_log, levels = c("0","1"))
          
          outl_pos_pred_semifast <- rep(0, i)
          outl_pos_pred_semifast[outl_semifast] <- 1
          outl_pos_pred_semifast <- factor(outl_pos_pred_semifast, levels = c("0","1"))
          
          # outl_pos_pred_semifast_log <- rep(0, i)
          # outl_pos_pred_semifast_log[outl_semifast_log] <- 1
          # outl_pos_pred_semifast_log <- factor(outl_pos_pred_semifast_log, levels = c("0","1"))
          
          
          outl_pos_pred_semifast25 <- rep(0, i)
          outl_pos_pred_semifast25[outl_semifast25] <- 1
          outl_pos_pred_semifast25 <- factor(outl_pos_pred_semifast25, levels = c("0","1"))
          
          # outl_pos_pred_semifast25_log <- rep(0, i)
          # outl_pos_pred_semifast25_log[outl_semifast25_log] <- 1
          # outl_pos_pred_semifast25_log <- factor(outl_pos_pred_semifast25_log, levels = c("0","1"))
          
          
          outl_pos_pred_normal <- rep(0, i)
          outl_pos_pred_normal[outl_normal] <- 1
          outl_pos_pred_normal <- factor(outl_pos_pred_normal, levels = c("0","1"))
          
          # outl_pos_pred_normal_log <- rep(0, i)
          # outl_pos_pred_normal_log[outl_normal_log] <- 1
          # outl_pos_pred_normal_log <- factor(outl_pos_pred_normal_log, levels = c("0","1"))
          
          
          ## create predicted target by competition
          outl_pos_pred_fbplot <- rep(0, i)
          outl_pos_pred_fbplot[outl_fbplot] <- 1
          outl_pos_pred_fbplot <- factor(outl_pos_pred_fbplot, levels = c("0","1"))
          
          outl_pos_pred_outlgrm <- rep(0, i)
          outl_pos_pred_outlgrm[outl_outlgrm] <- 1
          outl_pos_pred_outlgrm <- factor(outl_pos_pred_outlgrm, levels = c("0","1"))
          
          outl_pos_pred_msplot <- rep(0, i)
          outl_pos_pred_msplot[outl_msplot] <- 1
          outl_pos_pred_msplot <- factor(outl_pos_pred_msplot, levels = c("0","1"))
          
          outl_pos_pred_fom <- rep(0, i)
          outl_pos_pred_fom[outl_fom] <- 1
          outl_pos_pred_fom <- factor(outl_pos_pred_fom, levels = c("0","1"))
          
          
          ## compute confusion matrices
          m_fast <- caret::confusionMatrix(outl_pos_pred_fast, outl_pos_real, positive = "1")
          m_semifast <- caret::confusionMatrix(outl_pos_pred_semifast, outl_pos_real, positive = "1")
          m_semifast25 <- caret::confusionMatrix(outl_pos_pred_semifast25, outl_pos_real, positive = "1")
          m_normal <- caret::confusionMatrix(outl_pos_pred_normal, outl_pos_real, positive = "1")
          
          
          # m_fast_log <- caret::confusionMatrix(outl_pos_pred_fast_log, outl_pos_real, positive = "1")
          # m_semifast_log <- caret::confusionMatrix(outl_pos_pred_semifast_log, outl_pos_real, positive = "1")
          # m_semifast25_log <- caret::confusionMatrix(outl_pos_pred_semifast25_log, outl_pos_real, positive = "1")
          # m_normal_log <- caret::confusionMatrix(outl_pos_pred_normal_log, outl_pos_real, positive = "1")
          
          m_fbplot <- caret::confusionMatrix(outl_pos_pred_fbplot, outl_pos_real, positive = "1")
          m_outlgrm <- caret::confusionMatrix(outl_pos_pred_outlgrm, outl_pos_real, positive = "1")
          m_msplot <- caret::confusionMatrix(outl_pos_pred_msplot, outl_pos_real, positive = "1")
          m_fom <- caret::confusionMatrix(outl_pos_pred_fom, outl_pos_real, positive = "1")
          
          ## record sensitivities
          sens_fast <- c(sens_fast, m_fast$byClass[1])
          sens_semifast <- c(sens_semifast, m_semifast$byClass[1])
          sens_semifast25 <- c(sens_semifast25, m_semifast25$byClass[1])
          sens_normal <- c(sens_normal, m_normal$byClass[1])
          
          # sens_fast_log <- c(sens_fast_log, m_fast_log$byClass[1])
          # sens_semifast_log <- c(sens_semifast_log, m_semifast_log$byClass[1])
          # sens_semifast25_log <- c(sens_semifast25_log, m_semifast25_log$byClass[1])
          # sens_normal_log <- c(sens_normal_log, m_normal_log$byClass[1])
          
          sens_fbplot <- c(sens_fbplot, m_fbplot$byClass[1])
          sens_outlgrm <- c(sens_outlgrm, m_outlgrm$byClass[1])
          sens_msplot <- c(sens_msplot, m_msplot$byClass[1])
          sens_fom <- c(sens_fom, m_fom$byClass[1])
          
          ## record false positive rates (of the negativs, how much was misclassified), = 1 - specificity
          fprs_fast <- c(fprs_fast, 1 - m_fast$byClass[2])
          fprs_semifast <- c(fprs_semifast, 1 - m_semifast$byClass[2])
          fprs_semifast25 <- c(fprs_semifast25, 1 - m_semifast25$byClass[2])
          fprs_normal <- c(fprs_normal, 1 - m_normal$byClass[2])
          
          # fprs_fast_log <- c(fprs_fast_log, 1 - m_fast_log$byClass[2])
          # fprs_semifast_log <- c(fprs_semifast_log, 1 - m_semifast_log$byClass[2])
          # fprs_semifast25_log <- c(fprs_semifast25_log, 1 - m_semifast25_log$byClass[2])
          # fprs_normal_log <- c(fprs_normal_log, 1 - m_normal_log$byClass[2])
          
          fprs_fbplot <- c(fprs_fbplot, 1 - m_fbplot$byClass[2])
          fprs_outlgrm <- c(fprs_outlgrm, 1 - m_outlgrm$byClass[2])
          fprs_msplot <- c(fprs_msplot, 1 - m_msplot$byClass[2])
          fprs_fom <- c(fprs_fom, 1 - m_fom$byClass[2])
          
          ## record F1 scores
          f1s_fast <- c(f1s_fast, m_fast$byClass["F1"])
          f1s_semifast <- c(f1s_semifast, m_semifast$byClass["F1"])
          f1s_semifast25 <- c(f1s_semifast25, m_semifast25$byClass["F1"])
          f1s_normal <- c(f1s_normal, m_normal$byClass["F1"])
          
          # f1s_fast_log <- c(f1s_fast_log, m_fast_log$byClass["F1"])
          # f1s_semifast_log <- c(f1s_semifast_log, m_semifast_log$byClass["F1"])
          # f1s_semifast25_log <- c(f1s_semifast25_log, m_semifast25_log$byClass["F1"])
          # f1s_normal_log <- c(f1s_normal_log, m_normal_log$byClass["F1"])
          
          
          f1s_fbplot <- c(f1s_fbplot, m_fbplot$byClass["F1"])
          f1s_outlgrm <- c(f1s_outlgrm, m_outlgrm$byClass["F1"])
          f1s_msplot <- c(f1s_msplot, m_msplot$byClass["F1"])
          f1s_fom <- c(f1s_fom, m_fom$byClass["F1"])
        }
        # mean sensitivities
        mean_sens_fast <- mean(sens_fast, na.rm = T)
        mean_sens_semifast <- mean(sens_semifast, na.rm = T) 
        mean_sens_semifast25 <- mean(sens_semifast25, na.rm = T) 
        mean_sens_normal <- mean(sens_normal, na.rm = T)
        
        # mean_sens_fast_log <- mean(sens_fast_log, na.rm = T)
        # mean_sens_semifast_log <- mean(sens_semifast_log, na.rm = T) 
        # mean_sens_semifast25_log <- mean(sens_semifast25_log, na.rm = T) 
        # mean_sens_normal_log <- mean(sens_normal_log, na.rm = T)
        
        mean_sens_fbplot <- mean(sens_fbplot, na.rm = T)
        mean_sens_outlgrm <- mean(sens_outlgrm, na.rm = T)
        mean_sens_msplot <- mean(sens_msplot, na.rm = T)
        mean_sens_fom <- mean(sens_fom, na.rm = T)
        
        
        # median sensitivities
        median_sens_fast <- median(sens_fast, na.rm = T)
        median_sens_semifast <- median(sens_semifast, na.rm = T)
        median_sens_semifast25 <- median(sens_semifast25, na.rm = T)
        median_sens_normal <- median(sens_normal, na.rm = T)
        
        # median_sens_fast_log <- median(sens_fast_log, na.rm = T)
        # median_sens_semifast_log <- median(sens_semifast_log, na.rm = T)
        # median_sens_semifast25_log <- median(sens_semifast25_log, na.rm = T)
        # median_sens_normal_log <- median(sens_normal_log, na.rm = T)
        
        median_sens_fbplot <- median(sens_fbplot, na.rm = T)
        median_sens_outlgrm <- median(sens_outlgrm, na.rm = T)
        median_sens_msplot <- median(sens_msplot, na.rm = T)
        median_sens_fom <- median(sens_fom, na.rm = T)
        
        # sd sensitivities
        sd_sens_fast <- sd(sens_fast, na.rm = T)
        sd_sens_semifast <- sd(sens_semifast, na.rm = T)
        sd_sens_semifast25 <- sd(sens_semifast25, na.rm = T)
        sd_sens_normal <- sd(sens_normal, na.rm = T)
        
        # sd_sens_fast_log <- sd(sens_fast_log, na.rm = T)
        # sd_sens_semifast_log <- sd(sens_semifast_log, na.rm = T)
        # sd_sens_semifast25_log <- sd(sens_semifast25_log, na.rm = T)
        # sd_sens_normal_log <- sd(sens_normal_log, na.rm = T)
        
        
        sd_sens_fbplot <- sd(sens_fbplot, na.rm = T)
        sd_sens_outlgrm <- sd(sens_outlgrm, na.rm = T)
        sd_sens_msplot <- sd(sens_msplot, na.rm = T)
        sd_sens_fom <- sd(sens_fom, na.rm = T)
        
        
        # mean fpr
        mean_fprs_fast <- mean(fprs_fast, na.rm = T)
        mean_fprs_semifast <- mean(fprs_semifast, na.rm = T)
        mean_fprs_semifast25 <- mean(fprs_semifast25, na.rm = T)
        mean_fprs_normal <- mean(fprs_normal, na.rm = T)
        
        # mean_fprs_fast_log <- mean(fprs_fast_log, na.rm = T)
        # mean_fprs_semifast_log <- mean(fprs_semifast_log, na.rm = T)
        # mean_fprs_semifast25_log <- mean(fprs_semifast25_log, na.rm = T)
        # mean_fprs_normal_log <- mean(fprs_normal_log, na.rm = T)
        
        mean_fprs_fbplot <- mean(fprs_fbplot, na.rm = T);
        mean_fprs_outlgrm <- mean(fprs_outlgrm, na.rm = T);
        mean_fprs_msplot <- mean(fprs_msplot, na.rm = T);
        mean_fprs_fom <- mean(fprs_fom, na.rm = T);
        
        # median fpr
        median_fprs_fast <- median(fprs_fast, na.rm = T)
        median_fprs_semifast <- median(fprs_semifast, na.rm = T)
        median_fprs_semifast25 <- median(fprs_semifast25, na.rm = T)
        median_fprs_normal <- median(fprs_normal, na.rm = T)
        
        # median_fprs_fast_log <- median(fprs_fast_log, na.rm = T)
        # median_fprs_semifast_log <- median(fprs_semifast_log, na.rm = T)
        # median_fprs_semifast25_log <- median(fprs_semifast25_log, na.rm = T)
        # median_fprs_normal_log <- median(fprs_normal_log, na.rm = T)
        
        median_fprs_fbplot <- median(fprs_fbplot, na.rm = T)
        median_fprs_outlgrm <- median(fprs_outlgrm, na.rm = T)
        median_fprs_msplot <- median(fprs_msplot, na.rm = T)
        median_fprs_fom <- median(fprs_fom, na.rm = T)
        
        # sd fpr
        sd_fprs_fast <- sd(fprs_fast, na.rm = T)
        sd_fprs_semifast <- sd(fprs_semifast, na.rm = T)
        sd_fprs_semifast25 <- sd(fprs_semifast25, na.rm = T)
        sd_fprs_normal <- sd(fprs_normal, na.rm = T)
        
        # sd_fprs_fast_log <- sd(fprs_fast_log, na.rm = T)
        # sd_fprs_semifast_log <- sd(fprs_semifast_log, na.rm = T)
        # sd_fprs_semifast25_log <- sd(fprs_semifast25_log, na.rm = T)
        # sd_fprs_normal_log <- sd(fprs_normal_log, na.rm = T)
        
        sd_fprs_fbplot <- sd(fprs_fbplot, na.rm = T)
        sd_fprs_outlgrm <- sd(fprs_outlgrm, na.rm = T)
        sd_fprs_msplot <- sd(fprs_msplot, na.rm = T)
        sd_fprs_fom <- sd(fprs_fom, na.rm = T)
        
        
        # mean f1 scores
        mean_f1s_fast <- mean(f1s_fast, na.rm = T)
        mean_f1s_semifast <- mean(f1s_semifast, na.rm = T) 
        mean_f1s_semifast25 <- mean(f1s_semifast25, na.rm = T)
        mean_f1s_normal <- mean(f1s_normal, na.rm = T)
        
        # mean_f1s_fast_log <- mean(f1s_fast_log, na.rm = T)
        # mean_f1s_semifast_log <- mean(f1s_semifast_log, na.rm = T) 
        # mean_f1s_semifast25_log <- mean(f1s_semifast25_log, na.rm = T) 
        # mean_f1s_normal_log <- mean(f1s_normal_log, na.rm = T)
        
        mean_f1s_fbplot <- mean(f1s_fbplot, na.rm = T)
        mean_f1s_outlgrm <- mean(f1s_outlgrm, na.rm = T)
        mean_f1s_msplot <- mean(f1s_msplot, na.rm = T)
        mean_f1s_fom <- mean(f1s_fom, na.rm = T)
        
        
        # median f1 scores
        median_f1s_fast <- median(f1s_fast, na.rm = T)
        median_f1s_semifast <- median(f1s_semifast, na.rm = T)
        median_f1s_semifast25 <- median(f1s_semifast25, na.rm = T)
        median_f1s_normal <- median(f1s_normal, na.rm = T)
        
        # median_f1s_fast_log <- median(f1s_fast_log, na.rm = T)
        # median_f1s_semifast_log <- median(f1s_semifast_log, na.rm = T)
        # median_f1s_semifast25_log <- median(f1s_semifast25_log, na.rm = T)
        # median_f1s_normal_log <- median(f1s_normal_log, na.rm = T)
        
        
        median_f1s_fbplot <- median(f1s_fbplot, na.rm = T)
        median_f1s_outlgrm <- median(f1s_outlgrm, na.rm = T)
        median_f1s_msplot <- median(f1s_msplot, na.rm = T)
        median_f1s_fom <- median(f1s_fom, na.rm = T)
        
        
        # sd f1 scores
        sd_f1s_fast <- sd(f1s_fast, na.rm = T)
        sd_f1s_semifast <- sd(f1s_semifast, na.rm = T)
        sd_f1s_semifast25 <- sd(f1s_semifast25, na.rm = T)
        sd_f1s_normal <- sd(f1s_normal, na.rm = T)
        
        # sd_f1s_fast_log <- sd(f1s_fast_log, na.rm = T)
        # sd_f1s_semifast_log <- sd(f1s_semifast_log, na.rm = T)
        # sd_f1s_semifast25_log <- sd(f1s_semifast25_log, na.rm = T)
        # sd_f1s_normal_log <- sd(f1s_normal_log, na.rm = T)
        
        sd_f1s_fbplot <- sd(f1s_fbplot, na.rm = T)
        sd_f1s_outlgrm <- sd(f1s_outlgrm, na.rm = T)
        sd_f1s_msplot <- sd(f1s_msplot, na.rm = T)
        sd_f1s_fom <- sd(f1s_fom, na.rm = T)
        
        
        
        
        
        ####create data#####
        out_data <- data.frame(model = model, n = i, p = j, out.rate = k, 
                               sens_fast = sens_fast,
                               sens_semifast = sens_semifast,
                               sens_semifast25 = sens_semifast25,
                               sens_normal = sens_normal,
                               # sens_fast_log = sens_fast_log,
                               # sens_semifast_log = sens_semifast_log,
                               # sens_semifast25_log = sens_semifast25_log,
                               # sens_normal_log = sens_normal_log,
                               sens_fbplot = sens_fbplot,
                               sens_outlgrm = sens_outlgrm,
                               sens_msplot = sens_msplot, 
                               sens_fom = sens_fom, 
                               
                               # fprs
                               fprs_fast = fprs_fast,
                               fprs_semifast = fprs_semifast,
                               fprs_semifast25 = fprs_semifast25,
                               fprs_normal = fprs_normal,
                               # fprs_fast_log = fprs_fast_log,
                               # fprs_semifast_log = fprs_semifast_log,
                               # fprs_semifast25_log = fprs_semifast25_log,
                               # fprs_normal_log = fprs_normal_log,
                               fprs_fbplot = fprs_fbplot,
                               fprs_outlgrm = fprs_outlgrm,
                               fprs_msplot = fprs_msplot, 
                               fprs_fom = fprs_fom, 
                               
                               
                               # f1 score
                               f1s_fast = f1s_fast,
                               f1s_semifast = f1s_semifast,
                               f1s_semifast25 = f1s_semifast25,
                               f1s_normal = f1s_normal,
                               # f1s_fast_log = f1s_fast_log,
                               # f1s_semifast_log = f1s_semifast_log,
                               # f1s_semifast25_log = f1s_semifast25_log,
                               # f1s_normal_log = f1s_normal_log,
                               f1s_fbplot = f1s_fbplot,
                               f1s_outlgrm = f1s_outlgrm, 
                               f1s_msplot = f1s_msplot, 
                               f1s_fom = f1s_fom)
        cat("Writing file for this combination....\n")
        write.csv(out_data, paste0(model,"_data", "/raw_score_n",i,"_p",j,"_alpha",k,".csv") )
        
        
        
        ####package  results into row#####
        res_row <- data.frame(n = i, p = j, alpha = k, 
                              #sens
                              mean_sens_fast = mean_sens_fast,
                              mean_sens_semifast = mean_sens_semifast,
                              mean_sens_semifast25 = mean_sens_semifast25,
                              mean_sens_normal = mean_sens_normal,
                              # mean_sens_fast_log = mean_sens_fast_log,
                              # mean_sens_semifast_log = mean_sens_semifast_log,
                              # mean_sens_semifast25_log = mean_sens_semifast25_log,
                              # mean_sens_normal_log = mean_sens_normal_log,
                              mean_sens_fbplot = mean_sens_fbplot,
                              mean_sens_outlgrm = mean_sens_outlgrm, 
                              mean_sens_msplot = mean_sens_msplot, 
                              mean_sens_fom = mean_sens_fom, 
                              median_sens_fast = median_sens_fast,
                              median_sens_semifast = median_sens_semifast,
                              median_sens_semifast25 = median_sens_semifast25,
                              median_sens_normal = median_sens_normal,
                              # median_sens_fast_log = median_sens_fast_log,
                              # median_sens_semifast_log = median_sens_semifast_log,
                              # median_sens_semifast25_log = median_sens_semifast25_log,
                              # median_sens_normal_log = median_sens_normal_log,
                              median_sens_fbplot = median_sens_fbplot,
                              median_sens_outlgrm = median_sens_outlgrm, 
                              median_sens_msplot = median_sens_msplot, 
                              median_sens_fom = median_sens_fom, 
                              sd_sens_fast = sd_sens_fast,
                              sd_sens_semifast = sd_sens_semifast,
                              sd_sens_semifast25 = sd_sens_semifast25,
                              sd_sens_normal = sd_sens_normal,
                              # sd_sens_fast_log = sd_sens_fast_log,
                              # sd_sens_semifast_log = sd_sens_semifast_log,
                              # sd_sens_semifast25_log = sd_sens_semifast25_log,
                              # sd_sens_normal_log = sd_sens_normal_log,
                              sd_sens_fbplot = sd_sens_fbplot,
                              sd_sens_outlgrm = sd_sens_outlgrm, 
                              sd_sens_msplot = sd_sens_msplot, 
                              sd_sens_fom = sd_sens_fom, 
                              # fprs
                              mean_fprs_fast = mean_fprs_fast,
                              mean_fprs_semifast = mean_fprs_semifast,
                              mean_fprs_semifast25 = mean_fprs_semifast25,
                              mean_fprs_normal = mean_fprs_normal,
                              # mean_fprs_fast_log = mean_fprs_fast_log,
                              # mean_fprs_semifast_log = mean_fprs_semifast_log,
                              # mean_fprs_semifast25_log = mean_fprs_semifast25_log,
                              # mean_fprs_normal_log = mean_fprs_normal_log,
                              mean_fprs_fbplot = mean_fprs_fbplot,
                              mean_fprs_outlgrm = mean_fprs_outlgrm, 
                              mean_fprs_msplot = mean_fprs_msplot, 
                              mean_fprs_fom = mean_fprs_fom, 
                              median_fprs_fast = median_fprs_fast,
                              median_fprs_semifast = median_fprs_semifast,
                              median_fprs_semifast25 = median_fprs_semifast25,
                              median_fprs_normal = median_fprs_normal,
                              # median_fprs_fast_log = median_fprs_fast_log,
                              # median_fprs_semifast_log = median_fprs_semifast_log,
                              # median_fprs_semifast25_log = median_fprs_semifast25_log,
                              # median_fprs_normal_log = median_fprs_normal_log,
                              median_fprs_fbplot = median_fprs_fbplot,
                              median_fprs_outlgrm = median_fprs_outlgrm, 
                              median_fprs_msplot = median_fprs_msplot, 
                              median_fprs_fom = median_fprs_fom, 
                              sd_fprs_fast = sd_fprs_fast,
                              sd_fprs_semifast = sd_fprs_semifast,
                              sd_fprs_semifast25 = sd_fprs_semifast25,
                              sd_fprs_normal = sd_fprs_normal,
                              # sd_fprs_fast_log = sd_fprs_fast_log,
                              # sd_fprs_semifast_log = sd_fprs_semifast_log,
                              # sd_fprs_semifast25_log = sd_fprs_semifast25_log,
                              # sd_fprs_normal_log = sd_fprs_normal_log,
                              sd_fprs_fbplot = sd_fprs_fbplot,
                              sd_fprs_outlgrm = sd_fprs_outlgrm, 
                              sd_fprs_msplot = sd_fprs_msplot, 
                              sd_fprs_fom = sd_fprs_fom, 
                              
                              # f1 score
                              mean_f1s_fast = mean_f1s_fast,
                              mean_f1s_semifast = mean_f1s_semifast,
                              mean_f1s_semifast25 = mean_f1s_semifast25,
                              mean_f1s_normal = mean_f1s_normal,
                              # mean_f1s_fast_log = mean_f1s_fast_log,
                              # mean_f1s_semifast_log = mean_f1s_semifast_log,
                              # mean_f1s_semifast25_log = mean_f1s_semifast25_log,
                              # mean_f1s_normal_log = mean_f1s_normal_log,
                              mean_f1s_fbplot = mean_f1s_fbplot,
                              mean_f1s_outlgrm = mean_f1s_outlgrm, 
                              mean_f1s_msplot = mean_f1s_msplot, 
                              mean_f1s_fom = mean_f1s_fom, 
                              
                              median_f1s_fast = median_f1s_fast,
                              median_f1s_semifast = median_f1s_semifast,
                              median_f1s_semifast25 = median_f1s_semifast25,
                              median_f1s_normal = median_f1s_normal,
                              # median_f1s_fast_log = median_f1s_fast_log,
                              # median_f1s_semifast_log = median_f1s_semifast_log,
                              # median_f1s_semifast25_log = median_f1s_semifast25_log,
                              # median_f1s_normal_log = median_f1s_normal_log,
                              median_f1s_fbplot = median_f1s_fbplot,
                              median_f1s_outlgrm = median_f1s_outlgrm, 
                              median_f1s_msplot = median_f1s_msplot, 
                              median_f1s_fom = median_f1s_fom, 
                              
                              sd_f1s_fast = sd_f1s_fast,
                              sd_f1s_semifast = sd_f1s_semifast,
                              sd_f1s_semifast25 = sd_f1s_semifast25,
                              sd_f1s_normal = sd_f1s_normal,
                              # sd_f1s_fast_log = sd_f1s_fast_log,
                              # sd_f1s_semifast_log = sd_f1s_semifast_log,
                              # sd_f1s_semifast25_log = sd_f1s_semifast25_log,
                              # sd_f1s_normal_log = sd_f1s_normal_log,
                              sd_f1s_fbplot = sd_f1s_fbplot,
                              sd_f1s_outlgrm = sd_f1s_outlgrm, 
                              sd_f1s_msplot = sd_f1s_msplot, 
                              sd_f1s_fom = sd_f1s_fom)
        # bind row to resuts data
        res_data <- rbind(res_data, res_row) 
      }
    }
  }
  
  return(res_data)
}


test_model1_results <- model_analysis (model = "model1") 
write.csv(test_model1_results, "test_model1_results.csv")

test_model2_results <- model_analysis(model = "model2")
write.csv(test_model2_results, "test_model2_results.csv")


test_model3_results <- model_analysis (model = "model3")
write.csv(test_model3_results, "test_model3_results.csv")

test_model4_results <- model_analysis (model  = "model4")
write.csv(test_model4_results, "test_model4_results.csv")

test_model5_results <- model_analysis (model  = "model5")
write.csv(test_model5_results, "test_model5_results.csv")

test_model6_results <- model_analysis (model  = "model6")
write.csv(test_model6_results, "test_model6_results.csv")


model_analysis <- function(reps = 400, model  = paste0("model", 1:6)){
  #n <- c(100, 300)
  n <- 1000
  p <- 50
  alpha <-  c(0, 0.10, 0.15, .2)
  options("mc.cores" = 8) # Let's use 4 cores
  res_data <- data.frame()
  
  model <- match.arg(model)
  
  # initialize model
  if (model == "model1"){
    modelt <- model1
  } else if(model == "model2") {
    modelt <- model2
  } else if(model == "model3") {
    modelt <- model3
  } else if(model == "model4") {
    modelt <- model4
  } else if(model == "model5") {
    modelt <- model5
  } else if(model == "model6") {
    modelt <- model6
  }
  
  cat("Creating folder for model", model, "\n")
  dir.create(paste0(model,"_data"))
  
  for(i in n){
    for(j in p){
      for(k in alpha){
        cat("Model:", model, "working on:- ", "n = ", i, "p = ", j, "alpha = ", k,  "\n")
        #initialize sentivities
        sens_fast <- c()
        sens_semifast <- c()
        sens_semifast25 <- c()
        sens_normal <- c()
        
        sens_fast_log <- c()
        sens_semifast_log <- c()
        sens_semifast25_log <- c()
        sens_normal_log <- c()
        
        sens_fbplot <- c()
        sens_outlgrm <- c()
        sens_msplot <- c()
        sens_fom <- c()
        
        
        # initialize false positive rates
        fprs_fast <- c()
        fprs_semifast <- c()
        fprs_semifast25 <- c()
        fprs_normal <- c()
        
        fprs_fast_log <- c()
        fprs_semifast_log <- c()
        fprs_semifast25_log <- c()
        fprs_normal_log <- c()
        
        fprs_fbplot <- c()
        fprs_outlgrm <- c()
        fprs_msplot <- c()
        fprs_fom <- c()
        
        
        # initliaze f1 scores
        f1s_fast <- c()
        f1s_semifast <- c()
        f1s_semifast25 <- c()
        f1s_normal <- c()
        
        f1s_fast_log <- c()
        f1s_semifast_log <- c()
        f1s_semifast25_log <- c()
        f1s_normal_log <- c()
        
        #        f1s_normal_box <- c()
        f1s_fbplot <- c()
        f1s_outlgrm <- c()
        f1s_msplot <- c()
        f1s_fom <- c()
        
        
        for (o in 1 : reps) {
          cat(":-:-:-:- Working on rep :-", o, "\n")
          # generate data
          dt_tmp <- modelt(n = i, p = j, out.rate = k)
          dt <- dt_tmp$data
          # cache
          #use methods on data
          muod_fast <- get_outliers(t(dt), method = "fast", cut_method = "boxplot",  n_core = 1)
          muod_semifast <- get_outliers(t(dt), method = "semifast", cut_method = "boxplot", n_core = 8)
          muod_semifast25 <- get_outliers(t(dt), method = "semifast", cut_method = "boxplot", n_core = 8, sample_prop = 0.25)
          muod_normal <- get_outliers (t(dt), method = "rcpp", cut_method = "boxplot",  n_core = 8)
          
          muod_fast_log <- get_outliers(t(dt), method = "fast", cut_method = "log_transform",  n_core = 1)
          muod_semifast_log <- get_outliers(t(dt), method = "semifast", cut_method = "log_transform", n_core = 8)
          muod_semifast25_log <- get_outliers(t(dt), method = "semifast", cut_method = "log_transform",
                                              n_core = 8, sample_prop = 0.25)
          muod_normal_log <- get_outliers (t(dt), method = "rcpp", cut_method = "log_transform",  n_core = 8)
          
          outl_fbplot <-  get_fbplot_outliers(t(dt))
          outl_outlgrm <-  get_outliergram_outliers(dt)
          outl_msplot <- get_msplot_outliers(dt)
          outl_fom <- get_fom_outliers(dt)
          
          
          
          # Compute  accuracy metrics for all methods
          
          ## create real positions of outliers
          outl_pos_real <- rep(0, i)
          outl_pos_real[dt_tmp$true_outliers] <- 1
          outl_pos_real <- factor(outl_pos_real, levels = c("0","1"))
          
          ## get outlier coordinates by muod methods
          outl_fast <- unlist(unname(muod_fast))
          outl_semifast <- unlist(unname(muod_semifast))
          outl_semifast25 <- unlist(unname(muod_semifast25))
          outl_normal <- unlist(unname(muod_normal))
          
          outl_fast_log <- unlist(unname(muod_fast_log))
          outl_semifast_log <- unlist(unname(muod_semifast_log))
          outl_semifast25_log <- unlist(unname(muod_semifast25_log))
          outl_normal_log <- unlist(unname(muod_normal_log))
          
          #          outl_normal_box <- unlist(unname(muod_normal_box))
          ###remove duplicates
          outl_fast <- outl_fast[!duplicated(outl_fast)]
          outl_semifast <- outl_semifast[!duplicated(outl_semifast)]
          outl_semifast25 <- outl_semifast25[!duplicated(outl_semifast25)]
          outl_normal <- outl_normal[!duplicated(outl_normal)]
          
          outl_fast_log <- outl_fast_log[!duplicated(outl_fast_log)]
          outl_semifast_log <- outl_semifast_log[!duplicated(outl_semifast_log)]
          outl_semifast25_log <- outl_semifast25_log[!duplicated(outl_semifast25_log)]
          outl_normal_log <- outl_normal_log[!duplicated(outl_normal_log)]
          
          
          ## create predicted target by muod methods
          outl_pos_pred_fast <- rep(0, i)
          outl_pos_pred_fast[outl_fast] <- 1
          outl_pos_pred_fast <- factor(outl_pos_pred_fast, levels = c("0","1"))
          
          outl_pos_pred_fast_log <- rep(0, i)
          outl_pos_pred_fast_log[outl_fast_log] <- 1
          outl_pos_pred_fast_log <- factor(outl_pos_pred_fast_log, levels = c("0","1"))
          
          outl_pos_pred_semifast <- rep(0, i)
          outl_pos_pred_semifast[outl_semifast] <- 1
          outl_pos_pred_semifast <- factor(outl_pos_pred_semifast, levels = c("0","1"))
          
          outl_pos_pred_semifast_log <- rep(0, i)
          outl_pos_pred_semifast_log[outl_semifast_log] <- 1
          outl_pos_pred_semifast_log <- factor(outl_pos_pred_semifast_log, levels = c("0","1"))
          
          
          outl_pos_pred_semifast25 <- rep(0, i)
          outl_pos_pred_semifast25[outl_semifast25] <- 1
          outl_pos_pred_semifast25 <- factor(outl_pos_pred_semifast25, levels = c("0","1"))
          
          outl_pos_pred_semifast25_log <- rep(0, i)
          outl_pos_pred_semifast25_log[outl_semifast25_log] <- 1
          outl_pos_pred_semifast25_log <- factor(outl_pos_pred_semifast25_log, levels = c("0","1"))
          
          
          outl_pos_pred_normal <- rep(0, i)
          outl_pos_pred_normal[outl_normal] <- 1
          outl_pos_pred_normal <- factor(outl_pos_pred_normal, levels = c("0","1"))
          
          outl_pos_pred_normal_log <- rep(0, i)
          outl_pos_pred_normal_log[outl_normal_log] <- 1
          outl_pos_pred_normal_log <- factor(outl_pos_pred_normal_log, levels = c("0","1"))
          
          
          ## create predicted target by competition
          outl_pos_pred_fbplot <- rep(0, i)
          outl_pos_pred_fbplot[outl_fbplot] <- 1
          outl_pos_pred_fbplot <- factor(outl_pos_pred_fbplot, levels = c("0","1"))
          
          outl_pos_pred_outlgrm <- rep(0, i)
          outl_pos_pred_outlgrm[outl_outlgrm] <- 1
          outl_pos_pred_outlgrm <- factor(outl_pos_pred_outlgrm, levels = c("0","1"))
          
          outl_pos_pred_msplot <- rep(0, i)
          outl_pos_pred_msplot[outl_msplot] <- 1
          outl_pos_pred_msplot <- factor(outl_pos_pred_msplot, levels = c("0","1"))
          
          outl_pos_pred_fom <- rep(0, i)
          outl_pos_pred_fom[outl_fom] <- 1
          outl_pos_pred_fom <- factor(outl_pos_pred_fom, levels = c("0","1"))
          
          
          ## compute confusion matrices
          m_fast <- caret::confusionMatrix(outl_pos_pred_fast, outl_pos_real, positive = "1")
          m_semifast <- caret::confusionMatrix(outl_pos_pred_semifast, outl_pos_real, positive = "1")
          m_semifast25 <- caret::confusionMatrix(outl_pos_pred_semifast25, outl_pos_real, positive = "1")
          m_normal <- caret::confusionMatrix(outl_pos_pred_normal, outl_pos_real, positive = "1")
          
          
          m_fast_log <- caret::confusionMatrix(outl_pos_pred_fast_log, outl_pos_real, positive = "1")
          m_semifast_log <- caret::confusionMatrix(outl_pos_pred_semifast_log, outl_pos_real, positive = "1")
          m_semifast25_log <- caret::confusionMatrix(outl_pos_pred_semifast25_log, outl_pos_real, positive = "1")
          m_normal_log <- caret::confusionMatrix(outl_pos_pred_normal_log, outl_pos_real, positive = "1")
          
          m_fbplot <- caret::confusionMatrix(outl_pos_pred_fbplot, outl_pos_real, positive = "1")
          m_outlgrm <- caret::confusionMatrix(outl_pos_pred_outlgrm, outl_pos_real, positive = "1")
          m_msplot <- caret::confusionMatrix(outl_pos_pred_msplot, outl_pos_real, positive = "1")
          m_fom <- caret::confusionMatrix(outl_pos_pred_fom, outl_pos_real, positive = "1")
          
          ## record sensitivities
          sens_fast <- c(sens_fast, m_fast$byClass[1])
          sens_semifast <- c(sens_semifast, m_semifast$byClass[1])
          sens_semifast25 <- c(sens_semifast25, m_semifast25$byClass[1])
          sens_normal <- c(sens_normal, m_normal$byClass[1])
          
          sens_fast_log <- c(sens_fast_log, m_fast_log$byClass[1])
          sens_semifast_log <- c(sens_semifast_log, m_semifast_log$byClass[1])
          sens_semifast25_log <- c(sens_semifast25_log, m_semifast25_log$byClass[1])
          sens_normal_log <- c(sens_normal_log, m_normal_log$byClass[1])
          
          sens_fbplot <- c(sens_fbplot, m_fbplot$byClass[1])
          sens_outlgrm <- c(sens_outlgrm, m_outlgrm$byClass[1])
          sens_msplot <- c(sens_msplot, m_msplot$byClass[1])
          sens_fom <- c(sens_fom, m_fom$byClass[1])
          
          ## record false positive rates (of the negativs, how much was misclassified), = 1 - specificity
          fprs_fast <- c(fprs_fast, 1 - m_fast$byClass[2])
          fprs_semifast <- c(fprs_semifast, 1 - m_semifast$byClass[2])
          fprs_semifast25 <- c(fprs_semifast25, 1 - m_semifast25$byClass[2])
          fprs_normal <- c(fprs_normal, 1 - m_normal$byClass[2])
          
          fprs_fast_log <- c(fprs_fast_log, 1 - m_fast_log$byClass[2])
          fprs_semifast_log <- c(fprs_semifast_log, 1 - m_semifast_log$byClass[2])
          fprs_semifast25_log <- c(fprs_semifast25_log, 1 - m_semifast25_log$byClass[2])
          fprs_normal_log <- c(fprs_normal_log, 1 - m_normal_log$byClass[2])
          
          fprs_fbplot <- c(fprs_fbplot, 1 - m_fbplot$byClass[2])
          fprs_outlgrm <- c(fprs_outlgrm, 1 - m_outlgrm$byClass[2])
          fprs_msplot <- c(fprs_msplot, 1 - m_msplot$byClass[2])
          fprs_fom <- c(fprs_fom, 1 - m_fom$byClass[2])
          
          ## record F1 scores
          f1s_fast <- c(f1s_fast, m_fast$byClass["F1"])
          f1s_semifast <- c(f1s_semifast, m_semifast$byClass["F1"])
          f1s_semifast25 <- c(f1s_semifast25, m_semifast25$byClass["F1"])
          f1s_normal <- c(f1s_normal, m_normal$byClass["F1"])
          
          f1s_fast_log <- c(f1s_fast_log, m_fast_log$byClass["F1"])
          f1s_semifast_log <- c(f1s_semifast_log, m_semifast_log$byClass["F1"])
          f1s_semifast25_log <- c(f1s_semifast25_log, m_semifast25_log$byClass["F1"])
          f1s_normal_log <- c(f1s_normal_log, m_normal_log$byClass["F1"])
          
          
          f1s_fbplot <- c(f1s_fbplot, m_fbplot$byClass["F1"])
          f1s_outlgrm <- c(f1s_outlgrm, m_outlgrm$byClass["F1"])
          f1s_msplot <- c(f1s_msplot, m_msplot$byClass["F1"])
          f1s_fom <- c(f1s_fom, m_fom$byClass["F1"])
        }
        # mean sensitivities
        mean_sens_fast <- mean(sens_fast, na.rm = T)
        mean_sens_semifast <- mean(sens_semifast, na.rm = T) 
        mean_sens_semifast25 <- mean(sens_semifast25, na.rm = T) 
        mean_sens_normal <- mean(sens_normal, na.rm = T)
        
        mean_sens_fast_log <- mean(sens_fast_log, na.rm = T)
        mean_sens_semifast_log <- mean(sens_semifast_log, na.rm = T) 
        mean_sens_semifast25_log <- mean(sens_semifast25_log, na.rm = T) 
        mean_sens_normal_log <- mean(sens_normal_log, na.rm = T)
        
        mean_sens_fbplot <- mean(sens_fbplot, na.rm = T)
        mean_sens_outlgrm <- mean(sens_outlgrm, na.rm = T)
        mean_sens_msplot <- mean(sens_msplot, na.rm = T)
        mean_sens_fom <- mean(sens_fom, na.rm = T)
        
        
        # median sensitivities
        median_sens_fast <- median(sens_fast, na.rm = T)
        median_sens_semifast <- median(sens_semifast, na.rm = T)
        median_sens_semifast25 <- median(sens_semifast25, na.rm = T)
        median_sens_normal <- median(sens_normal, na.rm = T)
        
        median_sens_fast_log <- median(sens_fast_log, na.rm = T)
        median_sens_semifast_log <- median(sens_semifast_log, na.rm = T)
        median_sens_semifast25_log <- median(sens_semifast25_log, na.rm = T)
        median_sens_normal_log <- median(sens_normal_log, na.rm = T)
        
        median_sens_fbplot <- median(sens_fbplot, na.rm = T)
        median_sens_outlgrm <- median(sens_outlgrm, na.rm = T)
        median_sens_msplot <- median(sens_msplot, na.rm = T)
        median_sens_fom <- median(sens_fom, na.rm = T)
        
        # sd sensitivities
        sd_sens_fast <- sd(sens_fast, na.rm = T)
        sd_sens_semifast <- sd(sens_semifast, na.rm = T)
        sd_sens_semifast25 <- sd(sens_semifast25, na.rm = T)
        sd_sens_normal <- sd(sens_normal, na.rm = T)
        
        sd_sens_fast_log <- sd(sens_fast_log, na.rm = T)
        sd_sens_semifast_log <- sd(sens_semifast_log, na.rm = T)
        sd_sens_semifast25_log <- sd(sens_semifast25_log, na.rm = T)
        sd_sens_normal_log <- sd(sens_normal_log, na.rm = T)
        
        
        sd_sens_fbplot <- sd(sens_fbplot, na.rm = T)
        sd_sens_outlgrm <- sd(sens_outlgrm, na.rm = T)
        sd_sens_msplot <- sd(sens_msplot, na.rm = T)
        sd_sens_fom <- sd(sens_fom, na.rm = T)
        
        
        # mean fpr
        mean_fprs_fast <- mean(fprs_fast, na.rm = T)
        mean_fprs_semifast <- mean(fprs_semifast, na.rm = T)
        mean_fprs_semifast25 <- mean(fprs_semifast25, na.rm = T)
        mean_fprs_normal <- mean(fprs_normal, na.rm = T)
        
        mean_fprs_fast_log <- mean(fprs_fast_log, na.rm = T)
        mean_fprs_semifast_log <- mean(fprs_semifast_log, na.rm = T)
        mean_fprs_semifast25_log <- mean(fprs_semifast25_log, na.rm = T)
        mean_fprs_normal_log <- mean(fprs_normal_log, na.rm = T)
        
        mean_fprs_fbplot <- mean(fprs_fbplot, na.rm = T);
        mean_fprs_outlgrm <- mean(fprs_outlgrm, na.rm = T);
        mean_fprs_msplot <- mean(fprs_msplot, na.rm = T);
        mean_fprs_fom <- mean(fprs_fom, na.rm = T);
        
        # median fpr
        median_fprs_fast <- median(fprs_fast, na.rm = T)
        median_fprs_semifast <- median(fprs_semifast, na.rm = T)
        median_fprs_semifast25 <- median(fprs_semifast25, na.rm = T)
        median_fprs_normal <- median(fprs_normal, na.rm = T)
        
        median_fprs_fast_log <- median(fprs_fast_log, na.rm = T)
        median_fprs_semifast_log <- median(fprs_semifast_log, na.rm = T)
        median_fprs_semifast25_log <- median(fprs_semifast25_log, na.rm = T)
        median_fprs_normal_log <- median(fprs_normal_log, na.rm = T)
        
        median_fprs_fbplot <- median(fprs_fbplot, na.rm = T)
        median_fprs_outlgrm <- median(fprs_outlgrm, na.rm = T)
        median_fprs_msplot <- median(fprs_msplot, na.rm = T)
        median_fprs_fom <- median(fprs_fom, na.rm = T)
        
        # sd fpr
        sd_fprs_fast <- sd(fprs_fast, na.rm = T)
        sd_fprs_semifast <- sd(fprs_semifast, na.rm = T)
        sd_fprs_semifast25 <- sd(fprs_semifast25, na.rm = T)
        sd_fprs_normal <- sd(fprs_normal, na.rm = T)
        
        sd_fprs_fast_log <- sd(fprs_fast_log, na.rm = T)
        sd_fprs_semifast_log <- sd(fprs_semifast_log, na.rm = T)
        sd_fprs_semifast25_log <- sd(fprs_semifast25_log, na.rm = T)
        sd_fprs_normal_log <- sd(fprs_normal_log, na.rm = T)
        
        sd_fprs_fbplot <- sd(fprs_fbplot, na.rm = T)
        sd_fprs_outlgrm <- sd(fprs_outlgrm, na.rm = T)
        sd_fprs_msplot <- sd(fprs_msplot, na.rm = T)
        sd_fprs_fom <- sd(fprs_fom, na.rm = T)
        
        
        # mean f1 scores
        mean_f1s_fast <- mean(f1s_fast, na.rm = T)
        mean_f1s_semifast <- mean(f1s_semifast, na.rm = T) 
        mean_f1s_semifast25 <- mean(f1s_semifast25, na.rm = T)
        mean_f1s_normal <- mean(f1s_normal, na.rm = T)
        
        mean_f1s_fast_log <- mean(f1s_fast_log, na.rm = T)
        mean_f1s_semifast_log <- mean(f1s_semifast_log, na.rm = T) 
        mean_f1s_semifast25_log <- mean(f1s_semifast25_log, na.rm = T) 
        mean_f1s_normal_log <- mean(f1s_normal_log, na.rm = T)
        
        mean_f1s_fbplot <- mean(f1s_fbplot, na.rm = T)
        mean_f1s_outlgrm <- mean(f1s_outlgrm, na.rm = T)
        mean_f1s_msplot <- mean(f1s_msplot, na.rm = T)
        mean_f1s_fom <- mean(f1s_fom, na.rm = T)
        
        
        # median f1 scores
        median_f1s_fast <- median(f1s_fast, na.rm = T)
        median_f1s_semifast <- median(f1s_semifast, na.rm = T)
        median_f1s_semifast25 <- median(f1s_semifast25, na.rm = T)
        median_f1s_normal <- median(f1s_normal, na.rm = T)
        
        median_f1s_fast_log <- median(f1s_fast_log, na.rm = T)
        median_f1s_semifast_log <- median(f1s_semifast_log, na.rm = T)
        median_f1s_semifast25_log <- median(f1s_semifast25_log, na.rm = T)
        median_f1s_normal_log <- median(f1s_normal_log, na.rm = T)
        
        
        median_f1s_fbplot <- median(f1s_fbplot, na.rm = T)
        median_f1s_outlgrm <- median(f1s_outlgrm, na.rm = T)
        median_f1s_msplot <- median(f1s_msplot, na.rm = T)
        median_f1s_fom <- median(f1s_fom, na.rm = T)
        
        
        # sd f1 scores
        sd_f1s_fast <- sd(f1s_fast, na.rm = T)
        sd_f1s_semifast <- sd(f1s_semifast, na.rm = T)
        sd_f1s_semifast25 <- sd(f1s_semifast25, na.rm = T)
        sd_f1s_normal <- sd(f1s_normal, na.rm = T)
        
        sd_f1s_fast_log <- sd(f1s_fast_log, na.rm = T)
        sd_f1s_semifast_log <- sd(f1s_semifast_log, na.rm = T)
        sd_f1s_semifast25_log <- sd(f1s_semifast25_log, na.rm = T)
        sd_f1s_normal_log <- sd(f1s_normal_log, na.rm = T)
        
        sd_f1s_fbplot <- sd(f1s_fbplot, na.rm = T)
        sd_f1s_outlgrm <- sd(f1s_outlgrm, na.rm = T)
        sd_f1s_msplot <- sd(f1s_msplot, na.rm = T)
        sd_f1s_fom <- sd(f1s_fom, na.rm = T)
        
        
        
        
        
        ####create data#####
        out_data <- data.frame(model = model, n = i, p = j, out.rate = k, 
                               sens_fast = sens_fast,
                               sens_semifast = sens_semifast,
                               sens_semifast25 = sens_semifast25,
                               sens_normal = sens_normal,
                               sens_fast_log = sens_fast_log,
                               sens_semifast_log = sens_semifast_log,
                               sens_semifast25_log = sens_semifast25_log,
                               sens_normal_log = sens_normal_log,
                               sens_fbplot = sens_fbplot,
                               sens_outlgrm = sens_outlgrm,
                               sens_msplot = sens_msplot, 
                               sens_fom = sens_fom, 
                               
                               # fprs
                               fprs_fast = fprs_fast,
                               fprs_semifast = fprs_semifast,
                               fprs_semifast25 = fprs_semifast25,
                               fprs_normal = fprs_normal,
                               fprs_fast_log = fprs_fast_log,
                               fprs_semifast_log = fprs_semifast_log,
                               fprs_semifast25_log = fprs_semifast25_log,
                               fprs_normal_log = fprs_normal_log,
                               fprs_fbplot = fprs_fbplot,
                               fprs_outlgrm = fprs_outlgrm,
                               fprs_msplot = fprs_msplot, 
                               fprs_fom = fprs_fom, 
                               
                               
                               # f1 score
                               f1s_fast = f1s_fast,
                               f1s_semifast = f1s_semifast,
                               f1s_semifast25 = f1s_semifast25,
                               f1s_normal = f1s_normal,
                               f1s_fast_log = f1s_fast_log,
                               f1s_semifast_log = f1s_semifast_log,
                               f1s_semifast25_log = f1s_semifast25_log,
                               f1s_normal_log = f1s_normal_log,
                               f1s_fbplot = f1s_fbplot,
                               f1s_outlgrm = f1s_outlgrm, 
                               f1s_msplot = f1s_msplot, 
                               f1s_fom = f1s_fom)
        cat("Writing file for this combination....\n")
        write.csv(out_data, paste0(model,"_data", "/raw_score_n",i,"_p",j,"_alpha",k,".csv") )
        
        
        
        ####package  results into row#####
        res_row <- data.frame(n = i, p = j, alpha = k, 
                              #sens
                              mean_sens_fast = mean_sens_fast,
                              mean_sens_semifast = mean_sens_semifast,
                              mean_sens_semifast25 = mean_sens_semifast25,
                              mean_sens_normal = mean_sens_normal,
                              mean_sens_fast_log = mean_sens_fast_log,
                              mean_sens_semifast_log = mean_sens_semifast_log,
                              mean_sens_semifast25_log = mean_sens_semifast25_log,
                              mean_sens_normal_log = mean_sens_normal_log,
                              mean_sens_fbplot = mean_sens_fbplot,
                              mean_sens_outlgrm = mean_sens_outlgrm, 
                              mean_sens_msplot = mean_sens_msplot, 
                              mean_sens_fom = mean_sens_fom, 
                              median_sens_fast = median_sens_fast,
                              median_sens_semifast = median_sens_semifast,
                              median_sens_semifast25 = median_sens_semifast25,
                              median_sens_normal = median_sens_normal,
                              median_sens_fast_log = median_sens_fast_log,
                              median_sens_semifast_log = median_sens_semifast_log,
                              median_sens_semifast25_log = median_sens_semifast25_log,
                              median_sens_normal_log = median_sens_normal_log,
                              median_sens_fbplot = median_sens_fbplot,
                              median_sens_outlgrm = median_sens_outlgrm, 
                              median_sens_msplot = median_sens_msplot, 
                              median_sens_fom = median_sens_fom, 
                              sd_sens_fast = sd_sens_fast,
                              sd_sens_semifast = sd_sens_semifast,
                              sd_sens_semifast25 = sd_sens_semifast25,
                              sd_sens_normal = sd_sens_normal,
                              sd_sens_fast_log = sd_sens_fast_log,
                              sd_sens_semifast_log = sd_sens_semifast_log,
                              sd_sens_semifast25_log = sd_sens_semifast25_log,
                              sd_sens_normal_log = sd_sens_normal_log,
                              sd_sens_fbplot = sd_sens_fbplot,
                              sd_sens_outlgrm = sd_sens_outlgrm, 
                              sd_sens_msplot = sd_sens_msplot, 
                              sd_sens_fom = sd_sens_fom, 
                              # fprs
                              mean_fprs_fast = mean_fprs_fast,
                              mean_fprs_semifast = mean_fprs_semifast,
                              mean_fprs_semifast25 = mean_fprs_semifast25,
                              mean_fprs_normal = mean_fprs_normal,
                              mean_fprs_fast_log = mean_fprs_fast_log,
                              mean_fprs_semifast_log = mean_fprs_semifast_log,
                              mean_fprs_semifast25_log = mean_fprs_semifast25_log,
                              mean_fprs_normal_log = mean_fprs_normal_log,
                              mean_fprs_fbplot = mean_fprs_fbplot,
                              mean_fprs_outlgrm = mean_fprs_outlgrm, 
                              mean_fprs_msplot = mean_fprs_msplot, 
                              mean_fprs_fom = mean_fprs_fom, 
                              median_fprs_fast = median_fprs_fast,
                              median_fprs_semifast = median_fprs_semifast,
                              median_fprs_semifast25 = median_fprs_semifast25,
                              median_fprs_normal = median_fprs_normal,
                              median_fprs_fast_log = median_fprs_fast_log,
                              median_fprs_semifast_log = median_fprs_semifast_log,
                              median_fprs_semifast25_log = median_fprs_semifast25_log,
                              median_fprs_normal_log = median_fprs_normal_log,
                              median_fprs_fbplot = median_fprs_fbplot,
                              median_fprs_outlgrm = median_fprs_outlgrm, 
                              median_fprs_msplot = median_fprs_msplot, 
                              median_fprs_fom = median_fprs_fom, 
                              sd_fprs_fast = sd_fprs_fast,
                              sd_fprs_semifast = sd_fprs_semifast,
                              sd_fprs_semifast25 = sd_fprs_semifast25,
                              sd_fprs_normal = sd_fprs_normal,
                              sd_fprs_fast_log = sd_fprs_fast_log,
                              sd_fprs_semifast_log = sd_fprs_semifast_log,
                              sd_fprs_semifast25_log = sd_fprs_semifast25_log,
                              sd_fprs_normal_log = sd_fprs_normal_log,
                              sd_fprs_fbplot = sd_fprs_fbplot,
                              sd_fprs_outlgrm = sd_fprs_outlgrm, 
                              sd_fprs_msplot = sd_fprs_msplot, 
                              sd_fprs_fom = sd_fprs_fom, 
                              
                              # f1 score
                              mean_f1s_fast = mean_f1s_fast,
                              mean_f1s_semifast = mean_f1s_semifast,
                              mean_f1s_semifast25 = mean_f1s_semifast25,
                              mean_f1s_normal = mean_f1s_normal,
                              mean_f1s_fast_log = mean_f1s_fast_log,
                              mean_f1s_semifast_log = mean_f1s_semifast_log,
                              mean_f1s_semifast25_log = mean_f1s_semifast25_log,
                              mean_f1s_normal_log = mean_f1s_normal_log,
                              mean_f1s_fbplot = mean_f1s_fbplot,
                              mean_f1s_outlgrm = mean_f1s_outlgrm, 
                              mean_f1s_msplot = mean_f1s_msplot, 
                              mean_f1s_fom = mean_f1s_fom, 
                              
                              median_f1s_fast = median_f1s_fast,
                              median_f1s_semifast = median_f1s_semifast,
                              median_f1s_semifast25 = median_f1s_semifast25,
                              median_f1s_normal = median_f1s_normal,
                              median_f1s_fast_log = median_f1s_fast_log,
                              median_f1s_semifast_log = median_f1s_semifast_log,
                              median_f1s_semifast25_log = median_f1s_semifast25_log,
                              median_f1s_normal_log = median_f1s_normal_log,
                              median_f1s_fbplot = median_f1s_fbplot,
                              median_f1s_outlgrm = median_f1s_outlgrm, 
                              median_f1s_msplot = median_f1s_msplot, 
                              median_f1s_fom = median_f1s_fom, 
                              
                              sd_f1s_fast = sd_f1s_fast,
                              sd_f1s_semifast = sd_f1s_semifast,
                              sd_f1s_semifast25 = sd_f1s_semifast25,
                              sd_f1s_normal = sd_f1s_normal,
                              sd_f1s_fast_log = sd_f1s_fast_log,
                              sd_f1s_semifast_log = sd_f1s_semifast_log,
                              sd_f1s_semifast25_log = sd_f1s_semifast25_log,
                              sd_f1s_normal_log = sd_f1s_normal_log,
                              sd_f1s_fbplot = sd_f1s_fbplot,
                              sd_f1s_outlgrm = sd_f1s_outlgrm, 
                              sd_f1s_msplot = sd_f1s_msplot, 
                              sd_f1s_fom = sd_f1s_fom)
        # bind row to resuts data
        res_data <- rbind(res_data, res_row) 
      }
    }
  }
  
  return(res_data)
}


test_model1_results <- model_analysis (model = "model1") 
write.csv(test_model1_results, "test_model1_results.csv")

test_model2_results <- model_analysis(model = "model2")
write.csv(test_model2_results, "test_model2_results.csv")


test_model3_results <- model_analysis (model = "model3")
write.csv(test_model3_results, "test_model3_results.csv")

test_model4_results <- model_analysis (model  = "model4")
write.csv(test_model4_results, "test_model4_results.csv")

test_model5_results <- model_analysis (model  = "model5")
write.csv(test_model5_results, "test_model5_results.csv")

test_model6_results <- model_analysis (model  = "model6")
write.csv(test_model6_results, "test_model6_results.csv")




