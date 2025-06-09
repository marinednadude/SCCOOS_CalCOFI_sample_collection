data {
  // species
  int<lower=0> I_main;
  int<lower=0> I_count;
  int<lower=0> I_CO1;

  //sites
  int<lower=0> K_main ;
  int<lower=0> K_CO1 ;
  int<lower=0> K_count ;

  //replicates
  int<lower=0> J_CO1;
  array[K_CO1] int<lower=0> J_CO1_vec;

  // Number of species-station combinations to estimate
  int<lower=0> N_station_species_main;

  // Observations
  int<lower=0> N_count_obs;
  int<lower=0> N_CO1_obs;

  array[N_CO1_obs] int<lower=0> D_CO1_obs;
  array[N_count_obs] int<lower=0> D_count_obs;

  //  covariates / offsets
  int<lower=0> N_CO1_station_rep_idx;
  vector[N_CO1_station_rep_idx] log_r_CO1;
  real N_pcr_CO1; // Number of PCR cycles - CO1

  //Count covariates / offsets
  vector[N_count_obs] log_V; // Volume of water sampled relative to the reference volume.
  vector[N_count_obs] log_P; // Proportion of jar identified

  // Indexes
  array[N_count_obs] int<lower=0> count_station_idx;
  array[N_count_obs] int<lower=0> count_sp_idx;

  array[N_CO1_obs] int<lower=0> CO1_station_idx;
  array[N_CO1_obs] int<lower=0> CO1_sp_idx; // this is the index to the list of species found by CO1 primer
  array[N_CO1_obs] int<lower=0> CO1_station_rep_idx;


  array[N_station_species_main] int<lower=0> main_station_idx;
  array[N_station_species_main] int<lower=0> main_sp_idx ;

  // Mapping matrices
  matrix[I_count,I_main] M_to_count ;
  matrix[I_CO1,I_main] M_to_CO1 ;

  array[2] real log_eta_prior;
  array[2] real beta_prior_CO1;

}

transformed data{
}

parameters {
   real tau_CO1_0;
   real tau_CO1_1;

   vector [N_CO1_station_rep_idx] log_eta_CO1 ; // This is the fraction of total amplicons that is read by the sequencer (a scalar)
   vector<lower=0,upper=1>[I_CO1] a_CO1 ; // amp efficiency of primer x species for the CO1 primers

  // main list log-intercept parameters (includes all species, all communities with non-zero observations)
  vector[N_station_species_main] b_main ;// This is a vector possible species / species groups.
}

transformed parameters{
   array[K_main] vector[I_main] b_main_grid;
   array[K_CO1] vector[I_CO1] b_CO1;
   array[K_count] vector[I_count] b_count;

  for(k in 1:K_count){
    b_main_grid[k] = rep_vector(0,I_main);

  }

  for(j in 1:N_station_species_main){
      b_main_grid[main_station_idx[j],main_sp_idx[j]] = exp(b_main[j]);
  }

  for(k in 1:K_count){
    b_count[k] = M_to_count * b_main_grid[k] ;
  }

  for(k in 1:K_CO1){
    b_CO1[k] = (M_to_CO1 * b_main_grid[k]) /
                  sum((M_to_CO1 * b_main_grid[k])) ;
  }

}

model {
  { // Local variables declaration for making the Stan program less Memory hungry
    vector[N_CO1_obs] log_lambda_CO1 ;
    vector[N_count_obs] log_theta ;

    for(q in 1:N_CO1_obs){ 
      log_lambda_CO1[q] =  -1.609438 + //log(0.2); the fraction of each sample subsampled, pipetting.
                              log(b_CO1[CO1_station_idx[q],CO1_sp_idx[q]]) +
                              N_pcr_CO1*log(1.0 + a_CO1[CO1_sp_idx[q]]) +
                              log_r_CO1[CO1_station_rep_idx[q]] + // add a vector of known sample fractions
                              log_eta_CO1[CO1_station_rep_idx[q]] // fraction of amplicons getting sequenced into reads
                              ;

    }


    for(q in 1:N_count_obs){
        log_theta[q] = log(b_count[count_station_idx[q],count_sp_idx[q]]);
    }
        log_theta = log_theta + log_V + log_P;

    // Likelihoods.
    D_CO1_obs ~ neg_binomial_2(exp(log_lambda_CO1), exp(tau_CO1_0 + tau_CO1_1*log_lambda_CO1));  //tau

  log_theta = log_theta + log_V + log_P;

    D_count_obs ~ poisson_log(log_theta);


    log_eta_CO1 ~ normal(-4, 4) ;


    } // end local variable declaration

    b_main ~ normal(0,4) ;
    a_CO1 ~ beta(beta_prior_CO1[1],beta_prior_CO1[2]);

    tau_CO1_0 ~ normal(0, 2);
    tau_CO1_1 ~ normal(0, 2);
}
generated quantities{

vector[N_CO1_obs] log_lik;
vector[N_CO1_obs] log_lambda_CO1 ;

    for(q in 1:N_CO1_obs){
      log_lambda_CO1[q] =  -1.609438 + //log(0.2); the fraction of each sample subsampled, pipetting.
                              log(b_CO1[CO1_station_idx[q],CO1_sp_idx[q]]) +
                              N_pcr_CO1*log(1.0 + a_CO1[CO1_sp_idx[q]]) +
                              log_r_CO1[CO1_station_rep_idx[q]] + // add a vector of known sample fractions
                              log_eta_CO1[CO1_station_rep_idx[q]] // fraction of amplicons getting sequenced into reads
                              ;

    log_lik[q] = neg_binomial_2_lpmf(D_CO1_obs[q] | exp(log_lambda_CO1[q]), exp(tau_CO1_0 + tau_CO1_1*log_lambda_CO1[q]));
    }


}

