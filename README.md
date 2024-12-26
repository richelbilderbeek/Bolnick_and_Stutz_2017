# Bolnick_and_Stutz_2017

Notes for `[Bolnick & Stutz, 2017]`.

## Claim of the paper

Extreme body sizes fare better, when taking the *absolute* of the relative
normalized body mass:

![Figure 2, taking the absolute of the relative body size, in the paper](figure_2_in_paper.png)

> Figure 2, taking the absolute of the relative body size, in the paper

![Figure 2, taking the absolute of the relative body size, by running fig2.R](fig2.png)

> Figure 2, taking the absolute of the relative body size, by running [fig2.R](fig2.R)

When taking just the relative
normalized body mass, this pattern breaks down:
 
 * in the lake: smaller individuals thrive
 * in the stream: bigger individuals thrive.

![Figure 2, taking the relative body size](fig2_non_absolute.png)

With 95% confidence interval added:

![Figure 2, taking the relative body size, with confidence intervals](fig2_non_absolute_ggplot.png)

Merge all four treatments:

![](fig2_non_absolute_ggplot_all.png)

## Is there evidence that the extreme phenotypes survive better?

With a parabolic fit: yes

![](pre_mass_survival_1.png)

With a LOWESS fit: no

![](pre_mass_survival_2.png)

# Do minority fish have a higher fitness at the extremes?

No.

![](minority_fish_fitnesses.png)

## References

- `[Bolnick & Stutz, 2017]` Bolnick, Daniel I., and William E. Stutz.
  "Frequency dependence limits divergent evolution by favouring rare
  immigrants over residents." Nature (2017).
