data {
  int N;
}

parameters {
  vector[N] x;
}

model {
  x ~ std_normal();
}
