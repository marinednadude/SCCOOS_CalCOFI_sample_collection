data {
  // species
  int<lower=0> I_main;
  int<lower=0> I_count;
  int<lower=0> I_mifish;

  //sites
  int<lower=0> K_main ;
  int<lower=0> K_mifish ;
  int<lower=0> K_count ;

  //replicates
  int<lower=0> J_mifish;
  int<lower=0> J_mifish_vec[K_mifish];

  // Number of species-station combinations to estimate
  int<lower=0> N_station_species_main;

  // Observations
  int<lower=0> N_count_obs;
  int<lower=0> N_mifish_obs;

  int<lower=0> D_mifish_obs[N_mifish_obs];
  int<lower=0> D_count_obs[N_count_obs];

  //  covariates / offsets
  int<lower=0> N_mifish_station_rep_idx;
  vector[N_mifish_station_rep_idx] log_r_mifish;
  real N_pcr_mifish; // Number of PCR cycles - mifish

  //Count covariates / offsets
  vector[N_count_obs] log_V; // Volume of water sampled relative to the reference volume.
  vector[N_count_obs] log_P; // Proportion of jar identified

  // Indexes
  int<lower=0> count_station_idx[N_count_obs];
  int<lower=0> count_sp_idx[N_count_obs];

  int<lower=0> mifish_station_idx[N_mifish_obs];
  int<lower=0> mifish_sp_idx[N_mifish_obs]; // this is the index to the list of species found by mifish primer
  int<lower=0> mifish_station_rep_idx[N_mifish_obs];


  int<lower=0> main_station_idx[N_station_species_main] ;
  int<lower=0> main_sp_idx[N_station_species_main] ;

  // Mapping matrices
  matrix[I_count,I_main] M_to_count ;
  matrix[I_mifish,I_main] M_to_mifish ;

  real log_eta_prior[2];
  real beta_prior_mifish[2];

}

transformed data{
}

parameters {
   real tau_mifish_0;
   real tau_mifish_1;

   vector [N_mifish_station_rep_idx] log_eta_mifish ; // This is the fraction of total amplicons that is read by the sequencer (a scalar)
   vector<lower=0,upper=1>[I_mifish] a_mifish ; // amp efficiency of primer x species for the mifish primers

  // main list log-intercept parameters (includes all species, all communities with non-zero observations)
  vector[N_station_species_main] b_main ;// This is a vector possible species / species groups.
}

transformed parameters{
   vector[I_main] b_main_grid[K_main];
   vector[I_mifish] b_mifish[K_mifish] ;
   vector[I_count] b_count[K_count] ;

  for(k in 1:K_count){
    b_main_grid[k] = rep_vector(0,I_main);
  }

  for(j in 1:N_station_species_main){
      b_main_grid[main_station_idx[j],main_sp_idx[j]] = exp(b_main[j]);
  }

  for(k in 1:K_count){
    b_count[k] = M_to_count * b_main_grid[k] ;
  }

  for(k in 1:K_mifish){
    b_mifish[k] = (M_to_mifish * b_main_grid[k]) /
                  sum((M_to_mifish * b_main_grid[k])) ;
  }

}

model {
  { // Local variables declaration for making the Stan program less Memory hungry
    vector[N_mifish_obs] log_lambda_mifish ;
    vector[N_count_obs] log_theta ;

    for(q in 1:N_mifish_obs){
      log_lambda_mifish[q] =  -1.609438 + //log(0.2); the fraction of each sample subsampled, pipetting.
                              log(b_mifish[mifish_station_idx[q],mifish_sp_idx[q]]) +
                              N_pcr_mifish*log(1.0 + a_mifish[mifish_sp_idx[q]]) +
                              log_r_mifish[mifish_station_rep_idx[q]] + // add a vector of known sample fractions
                              log_eta_mifish[mifish_station_rep_idx[q]] // fraction of amplicons getting sequenced into reads
                              ;
    }


    for(q in 1:N_count_obs){
        log_theta[q] = log(b_count[count_station_idx[q],count_sp_idx[q]]);
    }
        log_theta = log_theta + log_V + log_P;

    // Likelihoods.
    D_mifish_obs ~ neg_binomial_2(exp(log_lambda_mifish), exp(tau_mifish_0 + tau_mifish_1*log_lambda_mifish));  //tau
    D_count_obs ~ poisson_log(log_theta);
    log_eta_mifish ~ normal(-4, 4) ;
    } // end local variable declaration

    b_main ~ normal(0,4) ;
    a_mifish ~ beta(beta_prior_mifish[1],beta_prior_mifish[2]);
    tau_mifish_0 ~ normal(0, 2);
    tau_mifish_1 ~ normal(0, 2);
}
generated quantities{

vector[N_mifish_obs] log_lik;
vector[N_mifish_obs] log_lambda_mifish ;

    for(q in 1:N_mifish_obs){
      log_lambda_mifish[q] =  -1.609438 + //log(0.2); the fraction of each sample subsampled, pipetting.
                              log(b_mifish[mifish_station_idx[q],mifish_sp_idx[q]]) +
                              N_pcr_mifish*log(1.0 + a_mifish[mifish_sp_idx[q]]) +
                              log_r_mifish[mifish_station_rep_idx[q]] + // add a vector of known sample fractions
                              log_eta_mifish[mifish_station_rep_idx[q]] // fraction of amplicons getting sequenced into reads
                              ;

    log_lik[q] = neg_binomial_2_lpmf(D_mifish_obs[q] | exp(log_lambda_mifish[q]), exp(tau_mifish_0 + tau_mifish_1*log_lambda_mifish[q]));
    }


}

