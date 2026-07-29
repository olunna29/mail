## -----------------------------------------------------------------------------
#| message: false
library(tidyverse)
library(primer.data)
library(tidymodels)
library(broom)
library(gt)
library(marginaleffects)


## -----------------------------------------------------------------------------
#| cache: true
x <- mail |>
  select(treatment, applied_mail, party, age, sex) |>
  drop_na() |>
  mutate(applied_mail = as.factor(applied_mail),
         treatment = fct_relevel(treatment, "No Postcard"))


## -----------------------------------------------------------------------------
x |>
  group_by(party, treatment) |>
  summarize(share = mean(applied_mail == "Yes"), .groups = "drop") |>
  ggplot(aes(x = treatment, y = share)) +
  geom_col() +
  facet_wrap(~ party) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage")


## -----------------------------------------------------------------------------
#| cache: true
fit_mail <- logistic_reg(engine = "glm") |>
  fit(applied_mail ~ treatment + party + age + sex, data = x)


## -----------------------------------------------------------------------------
tidy(fit_mail, conf.int = TRUE) |>
  gt() |>
  tab_header(
    title = "Logistic Regression Model of Mail Ballot Applications"
  ) |>
  tab_source_note(
    source_note = "Data source: 2020 Philadelphia mail ballot field experiment"
  ) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  )


## -----------------------------------------------------------------------------
predictions(extract_fit_engine(fit_mail),
            newdata = datagrid(treatment = c("No Postcard", "Self", "Neighborhood")))

