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



## Aki's comments

* [ ] In Figure 3, could we have also histogram for Rhat? It takes time to search the
  comment about Rhat from the text, which makes it harder for the reader and
  especially we don't want to make it harder for the reviewers.
* [ ] I don't understand what is going on in Figure 3C. Instead of "unconverged" and "converged", maybe "un-mixed" and "mixed" as in figures 1 and 2?
* [x] In Figure 4, could we have also histogram for max Rhat? I guess max would be
  appropriate although it may emphasize the multiple comparison issue in higher
  dimensional examples.
  * [x] No because there are only two Rhat values.
* [x] Figure 4, distribution for R∗ across 1000 draws -> distribution for R∗ across
  1000 simulations
  * [x] No, this is based on 1000 draws *not* 1000 simulations.
* [ ] "In the first of these, we used the 250-dimensional multivariate normal of
  eq. 2 and used 250 post-warm-up iterations (after 250 warm-up iterations)"
  * [ ] How many chains?
* [ ] Figure 5:
  * [ ]  "# of samples" -> "# of draws" or "sample size"
  * [ ] could we have also histograms for max Rhat?
  * [ ] mention the number of dimensions
  * [ ] distributions -> distributions across 1000 simulations
    * [ ] No because this is the R* distribution from a single simulation
* [ ] Figure 6:
  * [ ] what are the Models 400 and 1000 in legend?
  * [ ] could we have also histograms for max Rhat?
  * [ ] distribution -> distribution across 1000 simulations
    * [ ] No because this is the R* distribution from a single simulation
* [ ] Figure 7:
  * [ ] is this just for one simulation?
  * [ ] "using 250 post-warm-up draws" how many chains for getting total of
    20 draws?
* [ ] Figure 8:
  * [ ] could we have also histograms for max Rhat?
* [ ] Cauchy example: is R* classification with x? I assume that would be unstable
  compared to making R* classification with ranks of x. You could test this even
  with independent exact draws from Cauchy vs. ranked ones. I assume you would
  see there also that ranking will make R* go faster to 1. If the ranking doesn't
  matter we should explicitly state that with justification.
* [ ] Replace quantile R-2 with Cramer-Von Mises if it works
* [ ] Figure 9:
  * [ ] Replace quantile R-2
* [ ] Figure 13:
  * [ ] could we have also histograms for max Rhat?
* [ ] Introduction: mention rank plots, which are univariate and classification is made by eye
* [ ] Figure 14:
  * [ ] What happens if long chains are used but thinning is employed?
* [ ] Show how robust R* is to hyperparameter choice
* [ ] Reference ABC forest paper
* [ ] Run:
  * [ ] Ovarian
  * [ ] Prostate

## Clean up example notebooks

- [ ] s_ar1
- [x] s_bivariate_normal
- [ ] s_mvt_normal
- [ ] s_cauchy
- [ ] s_trends
- [ ] s_eight_schools
- [ ] s_ovarian_prostate