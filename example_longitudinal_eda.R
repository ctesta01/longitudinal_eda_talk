library(tidyverse)
library(lme4)

# a first, simple, crude plot
ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point()

# a plot with lines
ggplot(sleepstudy, aes(x = Days, y = Reaction, group = Subject)) + 
  geom_line()

# adding geom_smooth to show models
ggplot(sleepstudy, aes(x = Days, y = Reaction, group = Subject)) + 
  geom_smooth(method = 'lm', alpha = 0.25, se = FALSE) + 
  geom_point(size = .5) + 
  facet_wrap(~Subject) + 
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8)) + 
  theme_bw()

# let's create some example data where we might 
# have multiple observations for each year
df <- tibble::tribble(
  ~id, ~ldl_2005, ~hdl_2005, ~ldl_2010, ~hdl_2010,
  1,    150,       155,     175,       135,
  2,    125,       135,     180,       146)

# we can pivot longer into a tidy format where 
# each observation is in one row
df <- df %>% tidyr::pivot_longer(
  cols = 2:5,
  names_to = "variable",
  values_to = "value")

# but we'd like to split the "hdl_" or "ldl_" prefixes
# away from the year part in the column called "variable"
df <- df %>% tidyr::separate(
  col = 'variable',
  into = c('type', 'year'))

# now we can pivot the hdl and ldl observations into
# their own columns
df %>% tidyr::pivot_wider(id_cols = c('id', 'year'),
                          values_from = 'value', 
                          names_from = 'type')


# attempting to do this shorter -------------------------------------------

df <- tibble::tribble(
  ~id, ~ldl_2005, ~hdl_2005, ~ldl_2010, ~hdl_2010,
  1,    150,       155,     175,       135,
  2,    125,       135,     180,       146)

# we could have pivoted longer _and_ done the 
# column separate step in one go using some obscure
# parameters from tidyr::pivot_longer, but 
# we still would have to pivot_wider again at the end. 
df %>% tidyr::pivot_longer(
  cols = 2:5,
  names_to = c('type', 'year'),
  names_sep = '_',
  values_to = 'value')
