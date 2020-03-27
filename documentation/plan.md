* Add visualisation of R* by quantile of each parameter:
  * [x] Done for eight schools $\tau$ and $\mu$ parameters

  * [x] Add function that can plot this for a given parameter

* Separate out examples, each into its own file, using common r_star and r_star_distribution files
  * [x] Eight schools
  * [ ] Ar1
  * [ ] Bivariate normal
  * [ ] Wide datasets
  * [x] Trends
  * [ ] Cauchy

* For some datasets, include how far the distributions are from the truth:
  * Plot of $R^*$ vs KL divergence: doesn't work for Cauchy because the tails are so poorly estimated, but can try with normal case
  * Plot of Anderson-Darling test statistic seems ok for Cauchy and is based on differences in eCDF (but is better than KS statistic which is biased towards the middle of a distribution)
  * Same but with $\hat{R}$
  * [x] Include plot of Cauchy_convergence which plots R^2, R* and Rhat each verses distribution

* $R^*$ vs iterations for each of the problems (where there is a known truth):

  * [x] Done for Cauchy problem

* Change colours to monochrome:
  * [ ] Cauchy convergence
  * [ ] MVT normal