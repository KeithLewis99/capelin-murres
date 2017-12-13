
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
    
    // Data
    DATA_VECTOR(y);   // observations to be averaged by group
    DATA_VECTOR(x);   // independent variable for group means
    DATA_IVECTOR(g);  // index of group for each observation
    DATA_INTEGER(Ny); // number of observations
    DATA_INTEGER(Ng); // number of groups

    // Parameters
    PARAMETER(alpha);                   // regression parameters
    PARAMETER(beta);
    PARAMETER(log_sigma);
    PARAMETER_VECTOR(mu);               // group-specific parameters
    PARAMETER_VECTOR(log_sigma_mu);
    Type sigma = exp(log_sigma);
    vector<Type> sigma_mu = exp(log_sigma_mu);
    
    // Model
    Type nll = 0.0;
    for(int i = 0; i < Ny; i++){
        nll -= dnorm(y(i), mu(g(i)), sigma_mu(g(i)), true);     // nll for group means
    }
    vector<Type> fits(Ng);
    for(int i = 0; i < Ng; i++){
        fits(i) = alpha + beta * x(i);
        nll -= dnorm(mu(i), fits(i), sigma, true);  // nll for linear regression of means
    }

    // Report
    ADREPORT(mu);
    ADREPORT(sigma_mu);
    ADREPORT(fits);
    
    return nll;
    
}
