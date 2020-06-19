hbandit
=======

Safe multi-armed bandit implementations:

-   Eps-Greedy (fixed rate, inverse squared rate)
-   UCB family (UCB1, *α*-UCB, (*α*, *ϕ*)-UCB)
    \[[1](#ref-bubeck2012regret)\]
-   Exp3 (hyperparameter-free rate from \[[1](#ref-bubeck2012regret)\])
-   Exp4.R \[[2](#ref-sun2017safety)\]

documentation
-------------

      nix-build /path/to/hbandit/or/url/to/tarball -A hbandit.doc

<!-- vim: set ft=markdown.pandoc cole=0: -->

\[1\] Bubeck, S. et al. 2012. Regret analysis of stochastic and
nonstochastic multi-armed bandit problems. *Foundations and Trends in
Machine Learning*. 5, 1 (2012), 1–122.

\[2\] Sun, W. et al. 2017. Safety-aware algorithms for adversarial
contextual bandit. *Proceedings of the 34th international conference on
machine learning-volume 70* (2017), 3280–3288.
