# Longitudinal Exploratory Data Analysis 
# ID529 

# Outline: 
# We have a simulated dataset that we'll create (done in make_simulated_data.R).
# We will have to do some cleaning to it to tidy it into a format we can use 
# with ggplot2. We'll use ggplot2 to make some inferences about the patterns in
# the data by strata and by gender.
# 
# finally, we'll show some examples with the 

# dependencies ------------------------------------------------------------

library(tidyverse)
library(here)
library(colorblindr)
library(ggdist)
library(lme4)
library(broom.mixed)
library(gt)
library(ggcorrplot)

# setup -------------------------------------------------------------------

analysis_dir <- here("analysis")

# simulated dataset -------------------------------------------------------

# let's start out with some simulated data
source(here("example_data/make_simulated_data.R"))

# take a look at our dataset ----------------------------------------------

glimpse(df)

# or 
View(df)

# pivoting longer ---------------------------------------------------------

# from my perspective, the largest and first problem to tackle is that the 
# data is in a "wide" format, and not a "tidy" or "long" format -- meaning
# right now we have multiple observations per row instead of one observation
# per row. 
# 
# recall the three rules from here: https://r4ds.had.co.nz/tidy-data.html
# 
# Each variable must have its own column.
# Each observation must have its own row.
# Each value must have its own cell.

# before we can pivot_longer, it will be really helpful to us to have an id 
# column to keep track of which observations are supposed to be for 
# the same individual
#
# since our dataset didn't provide us one, i'll do something very simple and
# assign them ids according to the order in which they appear in the data.
df$id <- 1:nrow(df)

# i really prefer when the id column is first, so i'll reorder the dataframe
df <- select(df, id, everything()) 

# we'll use pivot_longer to move data of the same kind from being contained in
# multiple columns into one column with an additional indicator column for the
# year variable
df_tidy <- tidyr::pivot_longer(
  data = df,
  cols = contains("X"), # or I could have written c(X2005, X2010, X2015, X2020)
  values_to = "rate",
  names_to = "year"
)

# you might look at the data again, and compare df_tidy to df now
# View(df_tidy)


# clean the year column ---------------------------------------------------

df_tidy$year <- stringr::str_remove_all(df_tidy$year, "X")


# start plotting ----------------------------------------------------------

ggplot(df_tidy, aes(x = year, y = rate)) + 
  geom_line()

# woof, that went badly --
# looks like we need some grouping variables

ggplot(df_tidy, aes(x = year, y = rate, group = id)) + 
  geom_line()

# still bad, but at least individuals are connected; 
# a few ideas for what's next add transparency, and try faceting

ggplot(df_tidy, aes(x = year, y = rate, group = id)) + 
  geom_line(alpha = 0.25) 

ggplot(df_tidy, aes(x = year, y = rate, group = id, color = strata)) + 
  geom_line(alpha = 0.25) +
  facet_wrap(~strata)

# oh great, the faceting actually looked to be helpful. 
# that said, it's kind of annoying that the average trend is only 
# kind of / sort of visible to us and not made obvious by the figure.

# let's try adding stat_summary

ggplot(df_tidy, aes(x = year, y = rate, group = id, color = strata)) + 
  geom_line(alpha = 0.25) +
  stat_summary(fun = mean, geom = 'line', mapping = aes(group = strata), color = 'black') + 
  facet_wrap(~strata)

# that's pretty neat! 
# i feel like I learned something from that: 
# A trends slightly upwards
# B looks pretty flat
# C seems to have gone up, down, up
# D seems slightly downward in trend 

# are there more differences by gender? 

# i might try reformulating my facet_grid
ggplot(df_tidy, aes(x = year, y = rate, group = id, color = strata)) + 
  geom_line(alpha = 0.25) +
  stat_summary(fun = mean, geom = 'line', mapping = aes(group = strata), color = 'black') + 
  facet_grid(gender~strata)

# well, hmm, it's still hard to compare across genders since they're in separate
# facets. 
# i see two options: put the gender facets side-by-side instead, or 
# use color for gender instead.
ggplot(df_tidy, aes(x = year, y = rate, group = id, color = strata)) + 
  geom_line(alpha = 0.25) +
  stat_summary(fun = mean, geom = 'line', mapping = aes(group = strata), color = 'black') + 
  facet_grid(strata~gender)

# or 
ggplot(df_tidy, aes(x = year, y = rate, group = id, color = gender)) + 
  geom_line(alpha = 0.25) +
  stat_summary(fun = mean, geom = 'line', mapping = aes(group = interaction(strata, gender)), color = 'black') + 
  facet_grid(~strata)

# that second option is becoming quite clear, but I think I need some 
# way to distinguish the genders in the stat_summary;  I would add shapes to it
ggplot(df_tidy, aes(x = year, y = rate, group = id, color = gender)) + 
  geom_line(alpha = 0.25) +
  stat_summary(fun = mean, geom = 'line', mapping = aes(group = interaction(strata, gender)), color = 'black') + 
  stat_summary(fun = mean, geom = 'point', mapping = aes(group = interaction(strata, gender), shape = gender), color = 'black') + 
  facet_grid(~strata)

# i hate those colors, so i'd try to improve them 
ggplot(df_tidy, aes(x = year, y = rate, group = id, color = gender)) + 
  geom_line(alpha = 0.25) +
  stat_summary(fun = mean, geom = 'line', mapping = aes(group = interaction(strata, gender)), color = 'black') + 
  stat_summary(fun = mean, geom = 'point', mapping = aes(group = interaction(strata, gender), shape = gender), color = 'black') + 
  facet_grid(~strata) + 
  scale_color_manual(values = c("#ff9f43", "#0abde3"))

# check that it's colorblind friendly
colorblindr::cvd_grid()

# ok, so maybe if i wanted it to be black&white printer friendly, i'd
# do the version with gender in the panels
ggplot(df_tidy, aes(x = year, y = rate, group = id, color = gender)) + 
  geom_line(alpha = 0.25) +
  stat_summary(fun = mean, geom = 'line', mapping = aes(group = interaction(strata, gender)), color = 'black') + 
  stat_summary(fun = mean, geom = 'point', mapping = aes(group = interaction(strata, gender), shape = gender), color = 'black') + 
  facet_grid(strata~gender) + 
  scale_color_manual(values = c("#ff9f43", "#0abde3"))

# finally, there's some theme-stuff i'd fix
# 
# start with some minor fixes: 
#   - add title
#   - improve legend position + opacity
#   - use theme_bw()
ggplot(df_tidy, aes(x = year, y = rate, group = id, color = gender)) + 
  geom_line(alpha = 0.25) +
  stat_summary(fun = mean, geom = 'line', mapping = aes(group = interaction(strata, gender)), color = 'black') + 
  stat_summary(fun = mean, geom = 'point', mapping = aes(group = interaction(strata, gender), shape = gender), color = 'black') + 
  facet_grid(strata~gender) + 
  scale_color_manual(values = c("#ff9f43", "#0abde3")) + 
  ggtitle("Comparing Rates Across Strata and Gender", "2005-2020") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + # i had to google this: "adjust alpha in legend ggplot2"
  theme_bw() + 
  theme(legend.position = 'bottom')

# okay, but i still want the facets to be more clearly labeled
gender_labels <- c(M = 'Male', F = 'Female')
strata_labels <-
  c(
    A = 'Group A',
    B = 'Group B',
    C = 'Group C',
    D = 'Group D'
  )

ggplot(df_tidy, aes(x = year, y = rate, group = id, color = gender)) + 
  geom_line(alpha = 0.25) +
  stat_summary(
    fun = mean,
    geom = 'line',
    mapping = aes(group = interaction(strata, gender)),
    color = 'black'
  ) + 
  stat_summary(
    fun = mean,
    geom = 'point',
    mapping = aes(group = interaction(strata, gender), shape = gender),
    color = 'black'
  ) + 
  facet_grid(
    strata ~ gender,
    labeller = labeller(
      strata = strata_labels,
      gender = gender_labels
    )
  ) + 
  scale_color_manual(values = c("#ff9f43", "#0abde3")) + 
  ggtitle("Comparing Rates Across Strata and Gender", "2005-2020") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + # i had to google this: "adjust alpha in legend ggplot2"
  theme_bw() + 
  theme(legend.position = 'bottom')

# now if i were really serious, i would probably use ggdist 

ggplot(df_tidy, aes(x = year, y = rate, group = interaction(strata, gender), fill = gender)) + 
  ggdist::stat_lineribbon(aes(fill_ramp = after_stat(level))) + 
  facet_grid(
    strata ~ gender,
    labeller = labeller(
      strata = strata_labels,
      gender = gender_labels
    )
  ) + 
  scale_fill_manual(values = c("#ff9f43", "#0abde3")) +
  ggtitle("Comparing Rates Across Strata and Gender", "2005-2020") + 
  labs(fill = "Quartile Range") + 
  guides(fill = guide_none()) + 
  theme_bw() + 
  theme(legend.position = 'bottom')

# now an example with the sleepstudy data ----------------------------------

View(sleepstudy)

ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point() + 
  facet_wrap(~Subject)

ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~Subject)

ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(~Subject)


# of course you could fit mixed effects models ----------------------------

# but this is not the class is not where you're going to learn about random
# effects models -- 

reaction_times_model <-
  lmer(Reaction ~ Days + (Days | Subject), sleepstudy, subset = Days >= 2)

broom.mixed::tidy(reaction_times_model)



# one quick set of stratified models --------------------------------------

reaction_time_table <- 
  sleepstudy %>% 
  nest_by(Subject) %>% 
  mutate(model = list(lm(Reaction ~ Days, data = data %>% filter(Days >= 2)))) %>% 
  mutate(model_coefs = list(broom::tidy(model))) %>% 
  rowwise() %>%
  mutate(day_coef = model_coefs %>% filter(term == 'Days') %>% pull(estimate)) %>% 
  select(Subject, day_coef) %>% 
  gt() %>% 
  tab_header("Increase in Reaction Time associated with Sleep Deprivation",
             subtitle = "Results by Study Participant") %>% 
  fmt_number(
    day_coef, n_sigfig = 3,
  ) %>% 
  cols_label(
    day_coef = "Increase in Reaction Time [ms]"
  ) %>% 
  tab_source_note(
    md(
      "Source: Belenky et al., Patterns of performance degradation and restoration during sleep restriction and subsequent recovery: a sleep dose-response study. (2003) *Journal of Sleep Research*"
    )) %>% 
  tab_source_note(
    source_note = md("Reference: Chapter 25, Many Models, *R for Data Science* by Hadley Wickham and Garrett Grolemund. <https://r4ds.had.co.nz/many-models.html>")
  )

gtsave(reaction_time_table, here(analysis_dir, "tab1.html"))

