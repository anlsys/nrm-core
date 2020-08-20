---
bibliography: extras/refs.bib
csl: extras/acm.csl
link-citations: true
---

# hbandit

Safe multi-armed bandit implementations.

- Eps-Greedy (fixed rate, inverse squared rate)
- UCB family (UCB1, $\alpha$-UCB, $(\alpha,\phi)$-UCB) @bubeck2012regret
- Exp3 (hyperparameter-free rate from @bubeck2012regret)
- Exp4.R @sun2017safety

## documentation

```
  nix-build /path/to/hbandit/or/url/to/tarball -A hbandit.doc
```

<!-- vim: set ft=markdown.pandoc cole=0: -->
