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
  * [x] Text
  * [ ] Figure legends
* [x] Check for " vs `` throughout the MS
* [ ] Remove any reference to training and testing sample size and add in 70%
* [ ] Change all results to past tense



## Aki's comments

* [x] In Figure 3, could we have also histogram for Rhat? It takes time to search the
  comment about Rhat from the text, which makes it harder for the reader and
  especially we don't want to make it harder for the reviewers.
* [x] I don't understand what is going on in Figure 3C. Instead of "unconverged" and "converged", maybe "un-mixed" and "mixed" as in figures 1 and 2?
* [x] In Figure 4, could we have also histogram for max Rhat? I guess max would be
  appropriate although it may emphasize the multiple comparison issue in higher
  dimensional examples.
  * [x] No because there are only two Rhat values.
* [x] Figure 4, distribution for R∗ across 1000 draws -> distribution for R∗ across
  1000 simulations
  * [x] No, this is based on 1000 draws *not* 1000 simulations.
* [x] "In the first of these, we used the 250-dimensional multivariate normal of
  eq. 2 and used 250 post-warm-up iterations (after 250 warm-up iterations)"
  * [x] How many chains? 4 — added
* [x] Figure 5:
  * [x] "# of samples" -> "# of draws" or "sample size"
  * [x] could we have also histograms for max Rhat?
    * Not for max Rhat but for Rhat across all dims
  * [x] mention the number of dimensions
  * [x] distributions -> distributions across 1000 simulations
    - No because this is the R* distribution from a single simulation
* [x] Figure 6:
  * [x] what are the Models 400 and 1000 in legend? Changed to show that these correspond to # of draws
  * [x] could we have also histograms for max Rhat? Shown in Fig. 7 A
  * [x] distribution -> distribution across 1000 simulations
    * [x] No because this is the R* distribution from a single simulation
* [x] Figure 7:
  * [x] is this just for one simulation? Yep, added this
  * [x] "using 250 post-warm-up draws" how many chains for getting total of
    20 draws? Yep added this info
* [x] Figure 8:
  * [x] could we have also histograms for max Rhat? Done for individual Rhat values
* [x] Cauchy example: is R* classification with x? I assume that would be unstable
  compared to making R* classification with ranks of x. You could test this even
  with independent exact draws from Cauchy vs. ranked ones. I assume you would
  see there also that ranking will make R* go faster to 1. If the ranking doesn't
  matter we should explicitly state that with justification.
  * [x] Doesn't seem to be affected by ranking (see cauchy_ranked_unranked.pdf)
* [x] Replace quantile R-2 with Cramer-Von Mises if it works
  * [x] It doesn't really work as is too noisy and, I think, less interpretable than quantile R-2
  * [x] Tried Anderson-Darling test too but equally problematic
* [x] Figure 9:
  * [x] Replace quantile R-2. Not done for reason above.
* [x] Figure 13:
  * [x] could we have also histograms for max Rhat? Done for the individual Rhat parameters.
* [ ] Introduction: mention rank plots, which are univariate and classification is made by eye
* [ ] Figure 14:
  * [ ] What happens if long chains are used but thinning is employed?
* [ ] Show how robust R* is to hyperparameter choice
* [ ] Reference ABC forest paper
* [ ] Run:
  * [ ] Ovarian
  * [ ] Prostate

## Clean up example notebooks

- [x] s_ar1
  - [x] Runs monitor to calculate Rhat?
  - [x] Split Rhat?
  - [x] Rank-normaled split-Rhat?
- [x] s_bivariate_normal
  - [x] Runs monitor to calculate Rhat? NA
- [x] s_mvt_normal
  - [x] Runs monitor to calculate Rhat?
  - [x] Split Rhat?
  - [x] Rank-normaled split-Rhat?
- [x] s_wide_distributions
  - [x] Runs monitor to calculate Rhat?
  - [x] Split Rhat?
  - [x] Rank-normaled split-Rhat?
- [x] s_cauchy
  - [x] Runs monitor to calculate Rhat? Yep done
  - [x] Split Rhat?
  - [x] Rank-normaled split-Rhat?
- [x] s_trends
  - [x] Runs monitor to calculate Rhat?
  - [x] Split Rhat? NA
  - [x] Rank-normaled split-Rhat? NA
- [x] s_eight_schools
  - [x] Runs monitor to calculate Rhat? Done
  - [x] Split Rhat?
  - [x] Rank-normaled split-Rhat?
- [ ] s_ovarian_prostate
  - [ ] Runs monitor to calculate Rhat?
  - [ ] Rank-normaled split-Rhat?



* [x] Say somewhere that the R* values are comparable for smaller models to Rhat but can be more computationally intensive for larger ones.