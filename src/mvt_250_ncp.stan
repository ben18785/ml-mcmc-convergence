data {
  int N;
  matrix[N, N] A;
}
transformed data {
  vector[N] mu;
  matrix[N, N] L;
  L = inverse(L);
  L = cholesky_decompose(A);
  for(i in 1:N)
    mu[i] = 0.0;
}
parameters {
  vector[N] z;
}
transformed parameters {
  vector[N] theta;
  theta = mu + L * z;
}
model {
  z ~ std_normal();
}
