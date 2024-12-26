# Bolnick_and_Stutz_2017

Notes for `[Bolnick & Stutz, 2017]`.

The conclusion of the paper is that extreme body sizes fare better.
This can be shown in a plot that uses the *absolute* of the relative
normalized body mass, as shown in plot versions A (from the paper)
and B (reproduced from data) below.

When taking just the relative
normalized body mass, this pattern breaks down,
as shown in plot version C shown below.

Instead, the conclusion should be:
 
- in the lake: smaller individuals thrive
- in the stream: bigger individuals thrive.

Version|Figure 2 version|Description
-------|---------------------------------------------------|---------------------------------------------------
A      |![Figure 2, in the paper](figure_2_in_paper.png)   |As in paper
B      |![Figure 2, reproduced](fig2_reproduced.png)       |Reproduced using auhor's code, see [fig2_complete.R](fig2_complete.R)
C      |![Figure 2, reproduced](fig2_reproduced_no_abs.png)|Reproduced using the real valus, see [fig2_complete.R](fig2_complete.R)


## Extra figures

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
