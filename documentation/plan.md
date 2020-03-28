* Add visualisation of R* by quantile of each parameter:
  * [x] Done for eight schools $\tau$ and $\mu$ parameters

  * [x] Add function that can plot this for a given parameter

* Separate out examples, each into its own file, using common r_star and r_star_distribution files
  * [x] Eight schools
  * [x] Ar1
  * [x] Bivariate normal
  * [x] Wide datasets
  * [x] Trends
  * [x] Cauchy

* Add these examples:
  * [x] Trends
  * [x] Wide

* For some datasets, include how far the distributions are from the truth:
  * Plot of $R^*$ vs KL divergence: doesn't work for Cauchy because the tails are so poorly estimated, but can try with normal case
  * Plot of Anderson-Darling test statistic seems ok for Cauchy and is based on differences in eCDF (but is better than KS statistic which is biased towards the middle of a distribution)
  * Same but with $\hat{R}$
  * [x] Include plot of Cauchy_convergence which plots R^2, R* and Rhat each verses distribution
* $R^*$ vs iterations for each of the problems (where there is a known truth):
  * [x] Done for Cauchy problem

* Change colours to monochrome:
  * [x] Cauchy convergence
  * [x] MVT normal

* Check "non-centered" vs "noncentered" throughout MS:
  * [ ] Text
  * [ ] Figure legends

* [ ] Check for " vs `` throughout the MS
* [ ] Remove any reference to training and testing sample size and add in 70%
* [ ] Change all results to past tense