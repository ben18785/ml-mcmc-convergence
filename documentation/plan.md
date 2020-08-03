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
* [x] Figure 14:
  * [x] What happens if long chains are used but thinning is employed?
* [x] Show how robust R* is to hyperparameter choice: done for AR1 model
* [ ] Reference ABC forest paper
* [x] Run:
  * [x] Ovarian
  * [x] Prostate

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
- [x] s_ovarian_prostate
  - [x] Runs monitor to calculate Rhat?
  - [x] Rank-normaled split-Rhat?



* [x] Say somewhere that the R* values are comparable for smaller models to Rhat but can be more computationally intensive for larger ones.

# Addressing Aki's comments in email on 22nd May

* [x] Create table summarising examples
* [x] Move some of the examples to an appendix:
  * [x] Wide multivariate normal
  * [x] Nonstationary marginals
  * [x] Ovarian example
* [x] Write about ovarian dataset following: <https://arxiv.org/pdf/1906.08850.pdf>. "Ovarian example has multimodal posterior and it's likely that joint diagnostic
  can see more differences than marginal diagnostics. Several short chains are
  likely to visit more different modes, but short chains are then also likely to
  show bigger difference in joint classification."
* [x] "By evaluating its performance on an independent test set"
  * For autocorrelated draws, the train and tests, created by Algorithm 1,
    are not independent. Even if this sentence would be removed, a hypothetical
    reviewer could ask how the dependency from the autocorrelation affects the
    performance. In split-Rhat, the first and second half of the chain are also not
    independent, but but only weakly dependent unless the lag when autocorrelation
    goes close to zero is relatively big compared to the size of the split chain.
* [x] How well does GBM scale with the number of classes (ie chains in this case)?
* [x] In section 3.1 text refers to first and second paragraph refer to fig 3. Figure 3 caption starts with text "Autoregressive example" and A-C
  illustrate the case with one chain being different. While I'm looking
  at the figure, I continued checking also what is figure D, and caption
  says "D shows the R∗ distributions for the two series described in the
  tex". Now this reference saying just "the two series" causes cognitive
  overload, as there is need to go to find the spot in the text
  describing these, and the description is in the middle of a
  paragraph. It would be better to say also in the caption that what
  these two series are, e.g. "for two series, first being the same as
  used for figures A-C and the second being a series with series having
  equal distributions." It would be also useful to have subsubsection or
  paragraph titles in the text to make it easier to find each
  sub-example. For example 3.1 has three examples, 1) 4 chains with one
  being different, 2) comparison of first example series to 4 equal
  distributed chains, 3) robustness to the hyperparameter choices.
* [x] For figure 3D, what is the interpretation for R*<1? For Rhat<1 the
  interpretation is that is just due to use finite number of draws to compute
  variances and thus by luck we may have something like 0.99, but not usually
  less. Here the R* values go to values less than 0.9.
* [x] Figure 6 captions says "shows R ∗ distributions obtained for two MCMC samples
  (of differing numbers of draws: 400 and 1000) from the centered param-
  eterisation (“cp”) and one from the non-centered version (“ncp”; with 1000 draws)" but the plots have 10000
* [x] The text says "Even in the 10,000 iteration case, the R ∗ distribution remains
  stubbornly shifted a little rightwards of R ∗ = 1 (its mean is 1.06):" But this is not possible to see in 6A as it's just two histogram bars. More accuracy is needed in the plot. Same for 6B
* [x] Related to the Figure 7 the text says "Overall, the examples in this section
  suggest that R ∗ is robust to wide datasets; we also note that the statistic
  took comparable time to calculate relative to existing convergence diagnostics
  on a desktop computer." Is this an appropriate conclusion? I would say instead of robust, that it's
  conservative so that if there are not enough draws it will tend to say no
  convergence. Then the natural question would be, how many draws per dimension
  are needed to get R* close to 1?
* [ ] This and Cauchy example illustrate that Rhat working on marginals has a bit of
  multiple comparison problem. Each Rhat has noise and thus when we have many
  more the distribution of Rhats gets wider, making it likely that we need quite
  many draws to get all Rhats below some threshold. On the other hand when there
  are convergence problems, jointly looking at all variables uses more
  information. It would be good to remind about marginals vs. joint and multiple
  testing vs. single test around here.
  * [ ] Not sure what Aki means here. Ask him.
* [x] Figure 8: Wide data 250-dimensional example: established diagnostics. The
  top row shows the results for the centered parameterisation; the bottom row for
  the non-centered. Column A shows b columns B and C show the bulk- and tail-ESS;
  in each case the statistics are displayed split- R; for all model parameters
  and were calculated using 250 post-warm-up draws. It might be good to mention why Bulk-ESS can be larger than 250 draws times 4
  chains.
* [x] Specifically, we calculate the R 2 for the regression of actual quantile
  values on sample-estimated quantiles. Did you look at Kolmogorov-Smirnov and Wasserstein (Earth mover's) distances? I
  think quantile R^2 is sensible, but I'm just worried that a hypothetical
  reviewer would request something more common. I think R* is more sensitive to tails than Rhat.
* [x] Figure 15 "Column A shows plots for mu; Column B for tau"
  Could you label these columns directly with mu and tau?
  * [x] This is figure 11: eight_schools_r_star_quantiles.pdf
* [ ] Figure 16: "one with 4 chains with 10,000 iterations each (1000 discarded as
  warm-up and thinned by a factor of 10); another with 16 chains run with 1500
  iterations each (500 discarded as warm-up and no thinning)".
  * It would be good to tell the total number of post-warmup draws, so that the
    reader doesn't need to try to do the calculation. Bulk-ESS and Tail-ESS depend
    on the total number of post-warmup draws and comparison of 4 long chains vs 16
    short chains is confusing if the total number of post-warmup draws are different.
    Same for Fig 17.

## Ben's thoughts

* [x] Clean up bulk-ESS and tail-ESS across all figures:
  * [x] Figure 9: cauchy_convergence.
  * [x] S7: prostate
  * [x] S6: ovarian
  * [x] S2: wide_both_diagnostics
* [x] Update references to subsections in table (and possibly provide more information about sub examples)
* [x] How much stochasticity is there in $R^*$? That is, when we use the same dataset, how much variation do we get in $R^*$ due to the stochastic nature of splitting into training and testing and, to some degree, the training of the machine learning algorithm.
  * [x] Does this provide a rationale for using an uncertainty distribution?
* [ ] Finish supplementary experiments section
* [x] Fig 3D: ar1.pdf: change to "converged" and "unconverged" either in text or figure legend
* [x] Add in autocorrelation example
* [ ] Ensure stochasticity experiment has 100 replicates for 32 chain case