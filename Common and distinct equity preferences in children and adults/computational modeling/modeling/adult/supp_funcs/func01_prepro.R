#1.1. pre-processing function
prepro_func <- function(d_df, general_info) {
  # Currently class(d_df) == "data.table"
  
  # Use general_info of d_df
  subjs   <- general_info$subjs
  n_subj  <- general_info$n_subj
  t_subjs <- general_info$t_subjs # number of trials for each sub and block
  t_max   <- general_info$t_max
  
  # Initialize (model-specific) data arrays
  choice          <- array(-1, c(n_subj, t_max)) # choice, -1 for missing data
  rate_s          <- array(0, c(n_subj, t_max)) # rates of self, 0 for missing data
  rate_o          <- array(0, c(n_subj, t_max)) # rates of other, 0 for missing data
  pool            <- array(0, c(n_subj, t_max)) # targets, 0 for missing data
  party           <- array(-1, c(n_subj, t_max)) # party, -1 for missing data
  frame           <- array(-1, c(n_subj, t_max)) # frame, -1 for missing data

  data_new <- data.frame('subid'=rep(-1,n_subj*t_max),'trial'=rep(-1,n_subj*t_max),"choice"=rep(-1,n_subj*t_max),"rate_s"=rep(0,n_subj*t_max),
                         "rate_o"=rep(0,n_subj*t_max),"pool"=rep(0,n_subj*t_max),"party"=rep(-1,n_subj*t_max),"frame"=rep(-1,n_subj*t_max))

  
  # Write from d_df to the data arrays
  for (i in 1:n_subj) {
    subj                        <- subjs[i]
    t                           <- t_subjs[i] #trial number of the current subject
    DT_subj                     <- d_df[d_df$subid == subj]
    
    choice[i, 1:t]              <- DT_subj$choice
    rate_s[i, 1:t]              <- DT_subj$rate_s
    rate_o[i, 1:t]              <- DT_subj$rate_o
    pool[i, 1:t]                 <- DT_subj$pool
    party[i, 1:t]               <- DT_subj$party
    frame[i, 1:t]               <- DT_subj$frame
    
  }
  
  # Wrap into a list for Stan
  data_list <- list(
    Ns             = n_subj,
    Ts             = t_max,
    Tsubj          = t_subjs,
    choice         = choice,
    rate_s         = rate_s,
    rate_o         = rate_o,
    pool           = pool,
    party          = party,
    frame          = frame
  )
  
  
  # write into a data frame for PPC
  data_new$subid                 <- rep(subjs,each=t_max)
  data_new$trial                 <- rep(rep(1:t_max),times=n_subj)
  data_new$choice                <- as.vector(aperm(choice, c(2,1))) #change the order of arrary and then transfer to a vector
  data_new$rate_s                <- as.vector(aperm(rate_s, c(2,1)))
  data_new$rate_o                <- as.vector(aperm(rate_o, c(2,1)))
  data_new$pool                  <- as.vector(aperm(pool, c(2,1)))
  data_new$party                 <- as.vector(aperm(party, c(2,1)))
  data_new$frame                 <- as.vector(aperm(frame, c(2,1)))
 
  
  if (general_info$gen_file==1){  #only generate the data file for the main analysis, not for simulation analysis
    write.csv(data_new,file=paste0('data_for_cm',general_info$pfix,'.csv'),row.names = FALSE)
  }
  
  # Returned data_list will directly be passed to Stan
  return(data_list)
}
