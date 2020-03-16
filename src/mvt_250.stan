data{
  int N;
  matrix[N, N] A;
}

transformed data{
  vector[N] mu;
  for(i in 1:N)
    mu[i] = 0.0;
}

parameters{
  vector[N] theta;
}

model{
  target += multi_normal_prec_lpdf(theta|mu, A);
}
