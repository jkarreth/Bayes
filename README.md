Applied Bayesian Modeling at ICPSR
=========

R and JAGS code accompanying my ICPSR summer program course on Applied Bayesian Modeling. See <http://www.jkarreth.net/bayes-icpsr.html> for more information. Please feel free to fork and develop any code you see in this repository. If you notice a problem or have a question, please email me at <jkarreth@albany.edu> and/or create an issue with the respective file.

Modeling examples
-----------------

-   [angell.instructions.R](https://github.com/jkarreth/Bayes/blob/master/angell.instructions.R): code 	   for a Bayesian linear regression model, using the Angell data from
    John Fox's [R and S-PLUS Companion to Applied
    Regression](http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/index.html)
    in R using R2jags. This example contains code for data preparation,
    model fitting, and diagnostics.
-   [angell.jags.zip](http://www.jkarreth.net/files/angell.jags.zip): code for the same model directly 	   in JAGS from the Terminal and analyze the output using the `coda` package in R.
-   [beer.instructions.R](https://github.com/jkarreth/Bayes/blob/master/beer.instructions.R): code for 	   a Bayesian ordered logit model, using data on beer ratings
-   [econvote.instructions.R](https://github.com/jkarreth/Bayes/blob/master/econvote.instructions.R): 	  code for a Bayesian multinomial logit model, using Austrian voting data.
-   [undervote.instructions.R](https://github.com/jkarreth/Bayes/blob/master/undervote.instructions.R)	  : code for [Simon Jackman's Undervote example](http://jackman.stanford.edu/mcmc/mainFrameWinBugs.php#Undervote) using R2jags. [Difference of two proportions]
-   [turnout.instructions.R](https://github.com/jkarreth/Bayes/blob/master/turnout.instructions.R): 	code for (a slightly modified version of) [Simon Jackman's Turnout example](http://jackman.stanford.edu/mcmc/mainFrameWinBugs.php#Turnout) using R2jags. [Logit model]
-   [legislators.instructions.R](https://github.com/jkarreth/Bayes/blob/master/legislators.instructions.R): code for [Simon Jackman's Legislators example](http://jackman.stanford.edu/mcmc/mainFrameWinBugs.php#Legislators) using R2jags. [Bayesian IRT model]
-   [pumps.instructions.R](https://github.com/jkarreth/Bayes/blob/master/pumps.instructions.R): code 	 for the Pumps demo from the WinBUGS examples folder using R2jags. [Conjugate gamma-Poisson hierarchical model]
-   [rats.instructions.R](https://github.com/jkarreth/Bayes/blob/master/rats.instructions.R): code for 	   the Rats demo from the WinBUGS examples folder using R2jags. [Normal hierarchical model]

Multilevel models
-----------------

-   [wvs.instructions.R](https://github.com/jkarreth/Bayes/blob/master/wvs.instructions.R): 
    Linear model with group-level coefficients using an old subsample of World Values Survey.
-   [mlm.state.instructions.R](https://github.com/jkarreth/Bayes/blob/master/mlm.state.instructions.R): Step-by-step instructions to set up hierarchical data in R and fit a multilevel logit model using R2Jags, following the example in chapter 17 of Gelman and Hill's [Data Analysis Using Regression and Multilevel/Hierarchical Models](http://www.stat.columbia.edu/~gelman/arm/). Data:
    [polls.subset.merged.dta](http://www.jkarreth.net/files/polls.subset.merged.dta),
    [polls.subset.JK.dta](http://www.jkarreth.net/files/polls.subset.JK.dta),
    [presvote.dta](http://www.jkarreth.net/files/presvote.dta).

Diagnostics
-----------

-   [diagnostics.R](https://github.com/jkarreth/Bayes/blob/master/diagnostics.R): 
    This script shows a variety of ways to obtain diagnostics
    (traceplots, density plots, BGR, etc.) of JAGS/MCMC objects in R
    using the `coda`, `superdiag`, `ggmcmc`, and `mcmcplots` packages.

Model presentation
------------------

-   [regression.table.R](https://github.com/jkarreth/Bayes/blob/master/regression.table.R): example code to easily export JAGS/BUGS results to LaTeX or HTML. Based on my [mcmctab](https://github.com/jkarreth/JKmisc/blob/master/mcmctab.R) function.
-   [regression.dotplot.R](https://github.com/jkarreth/Bayes/blob/master/regression.dotplot.R): example code to easily make regression coefficient plots from JAGS/BUGS results.
-	[Posterior-Plots.R](https://github.com/reuning/EVDebs/blob/master/Bayesian/Posterior-Plots.R): a function written by [Kevin Reuning](http://www.kevinreuning.com) (participant in the 2015 Applied Bayes workshop at ICPSR) to create a coefficient dot plot with added posterior density.
-   [interaction.instructions.R](https://github.com/jkarreth/Bayes/blob/master/interaction.instructions.R): code to plot marginal effects from a Bayesian linear model with an
    interaction term across the range of a moderating variable.
-   [logit.pp.plot.instructions.R](https://github.com/jkarreth/Bayes/blob/master/logit.pp.plot.instructions.R): step-by-step example code to plot from a Bayesian logit model over a simulated range of values for an explanatory variable of interest.
-	[limited.dep.vars.funcs.R](https://github.com/edunford/bayes.functions/blob/master/limited.dep.vars.funcs.R): a set of functions written by [Eric Dunford](http://gvpt.umd.edu/gradprofile/Dunford/Eric%20) (participant in the 2015 Applied Bayes workshop at ICPSR) to calculate and visualize predicted probabilities from Bayesian logit or probit models for observed and simulated data.
-   [ologit.pp.plot.instructions.R](https://github.com/jkarreth/Bayes/blob/master/ologit.pp.plot.instructions.R): step-by-step example code to plot predicted probabilities over the simulated range of
    explanatory variables after a Bayesian ordered (or multinomial)
    logit model. Partially based on code by [Dave
    Armstrong](http://www.quantoid.net) (UW-Milwaukee).
-   [factor.dotplot.R](https://github.com/jkarreth/Bayes/blob/master/factor.dotplot.R): code to make a dot plot (with credible intervals) of a Bayesian
    factor score.