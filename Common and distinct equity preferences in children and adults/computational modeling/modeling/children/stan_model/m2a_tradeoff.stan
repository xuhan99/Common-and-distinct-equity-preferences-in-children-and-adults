//model 2

data {//these variable names should be consistent with input data names
int<lower=1> Ns; // define sub number
int<lower=1> Ts; // define maximum trial number
int<lower=1, upper=Ts> Tsubj[Ns]; //trial number for each sub
int   choice[Ns, Ts];        //choice, choice is input as an integer.
real  rate_s[Ns, Ts];   //rate of self
real  rate_o[Ns, Ts]; //rate of other
int   pool[Ns, Ts];  //current pool,as an integer
int   party[Ns, Ts];  //party:1=first party;2=third party
int   frame[Ns, Ts];  //frame:1=pos;2=neg
}

transformed data {
  //nothing to transform
}

parameters {
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[5] mu_pr; //mean of the parameters,4 disadvantageous inequality aversion and a inverse temperature.
  vector<lower=0>[5] sigma; //variance of the parameters, 2 paras
  
  // Subject-level raw parameters (for Matt trick)
  vector[Ns] beta_pr11;  // beta: disadvantageous inequality aversion
  vector[Ns] beta_pr12;  // beta: disadvantageous inequality aversion
  vector[Ns] beta_pr21;  // beta: disadvantageous inequality aversion
  vector[Ns] beta_pr22;  // beta: disadvantageous inequality aversion
  vector[Ns] tau_pr;    // tau: Inverse temperature
}

transformed parameters {
  // Transform subject-level raw parameters
  real beta11[Ns];
  real beta12[Ns];
  real beta21[Ns];
  real beta22[Ns];
  real<lower=0, upper=20> tau[Ns];
  
  for (i in 1:Ns) {
    beta11[i]  = mu_pr[1] + sigma[1] * beta_pr11[i];
    beta12[i]  = mu_pr[2] + sigma[2] * beta_pr12[i];
    beta21[i]  = mu_pr[3] + sigma[3] * beta_pr21[i];
    beta22[i]  = mu_pr[4] + sigma[4] * beta_pr22[i];
    tau[i]    = Phi_approx(mu_pr[5] + sigma[5] * tau_pr[i]) * 20;
  }
}

model {
  // Hyperparameters
  mu_pr  ~ normal(0, 1);
  sigma[1:4] ~ cauchy(0, 2.0);
  sigma[5] ~ normal(0, 1);
  
  // individual parameters
  beta_pr11  ~ normal(0, 1.0);
  beta_pr12  ~ normal(0, 1.0);
  beta_pr21  ~ normal(0, 1.0);
  beta_pr22  ~ normal(0, 1.0);
  tau_pr    ~ normal(0, 1.0);
  
  for (i in 1:Ns) {
    // Define values
    real Xself; //outcome of self
    real Xother;//outcome of other
    real beta_temp; // beta for current condotion
    
    for (t in 1:Tsubj[i]) { //loop over the trial number of each subject. Therefore, skipping the non-responsed trials.
    
    // utility
    vector[(pool[i,t]+1)] util; // Utility for each option for the current pool
    
    if(party[i,t]==1 && frame[i,t]==1){
      beta_temp = beta11[i];
    } else if(party[i,t]==1 && frame[i,t]==2) {
      beta_temp = beta12[i];
    } else if(party[i,t]==2 && frame[i,t]==1) {
      beta_temp = beta21[i];
    } else {
      beta_temp = beta22[i];
    }
    
    for (cn in 1: ((pool[i,t]+1))){ //possible choices
    Xself    = (cn-1) *rate_s[i,t]; //outcome of self for each possible choices. Stan promote integer (cn) to real number in this case, so wer are good.
    Xother   = (pool[i,t]-(cn-1)) *rate_o[i,t];//outcome of other for each possible choices. Stan promote integer (pool,cn) to real number in this case, so wer are good.
    
    util[cn] = Xself - beta_temp*fmax(Xother-Xself,0); // utility of each possible choices
    }
    
    // Sampling statement,computing the likelihood
    choice[i, t] ~ categorical_logit(util*tau[i]);
    
    } // end of t loop
  } // end of i loop
}

generated quantities {
  // For group level parameters
  real mu_beta11;
  real mu_beta12;
  real mu_beta21;
  real mu_beta22;
  real<lower=0, upper=20> mu_tau;
  
  //real Uc[Ns, Ts];   //value of choosen option; only for the winning model
  //real prob_c[Ns, Ts]; // probability of the choice; only for the winning model
  
  // For log likelihood calculation
  real log_lik[Ns];
  
  // For posterior predictive check
  //real y_pred[Ns, Ts];  // only for the winning model.
  
  // Set all posterior predictions to 0 (avoids NULL values); only for winning model
  // for (i in 1:Ns) {
    //   for (t in 1:Ts) {
      //     Uc[i, t] = -999;
      //      prob_c[i, t] = -999;
      //     y_pred[i, t] = -1;
      //   }
      // }
      
      mu_beta11  = mu_pr[1];
      mu_beta12  = mu_pr[2];
      mu_beta21  = mu_pr[3];
      mu_beta22  = mu_pr[4];
      mu_tau    = Phi_approx(mu_pr[5]) * 20;
      
      
      { // local section, this saves time and space
      for (i in 1:Ns) {
        // Define values
        real Xself; //outcome of self
        real Xother;//outcome of other
        real beta_temp; // alpha for current condotion
        
        log_lik[i] = 0.0;
        
        for (t in 1:Tsubj[i]) {
          // utility
          vector[(pool[i,t]+1)] util; // Utility for each option for the current pool
          
          if(party[i,t]==1 && frame[i,t]==1){
            beta_temp = beta11[i];
          } else if(party[i,t]==1 && frame[i,t]==2) {
            beta_temp = beta12[i];
          } else if(party[i,t]==2 && frame[i,t]==1) {
            beta_temp = beta21[i];
          } else {
            beta_temp = beta22[i];
          }
          
          for (cn in 1: ((pool[i,t]+1))){ //possible choices
          Xself    = (cn-1) *rate_s[i,t]; //outcome of self for each possible choices. Stan promote integer (cn) to real number in this case, so wer are good.
          Xother   = (pool[i,t]-(cn-1)) *rate_o[i,t];//outcome of other for each possible choices. Stan promote integer (pool,cn) to real number in this case, so wer are good.
          
          util[cn] = Xself - beta_temp*fmax(Xother-Xself,0); // utility of each possible choices
          }
          
          //prob_c[i,t] = exp(tau[i]*util[choice[i,t]])/(exp(tau[i]*util[1]) + exp(tau[i]*util[2]) + exp(tau[i]*util[3]) + exp(tau[i]*util[4])); // only for winning model
          
          //Uc[i,t]  =offer_recipt[i,t]+6-c_points[choice[i,t]]-alpha[i]*(fmax(offer_propoer[i,t]+6-3*c_points[choice[i,t]]-(offer_recipt[i,t]+6-c_points[choice[i,t]]),0));// only for winning model
          
          // Calculate log likelihood
          log_lik[i] += categorical_logit_lpmf(choice[i,t] | util*tau[i]);
          
          // generate posterior prediction for current trial
          // y_pred[i, t]  = categorical_rng(softmax(util*tau[i])); //only for winning model
          
        } // end of t loop
      } // end of i loop
      } // end of local section
}


