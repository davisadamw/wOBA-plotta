library(tidyverse)

wOBA_numer_weights <- tribble(
  ~stat, ~numer_weight, ~denom_weight,
  # the method only counts unintentional walks, so they'll need to be extracted first
  "uBB",  0.69,          1,
  "HBP",  0.72,          1,
  "SF",   0,             1,
  "AB",   0,             1,
  "1B",   0.89,          0,
  "2B",   1.27,          0,
  "3B",   1.62,          0,
  "HR",   2.10,          0
)

# this function will prep a data table pulled from baseball-reference for 
prep_stats <- function(br_data, identifier_cols = c("Year", "Age", "Tm", "Lg", "G")) {
  weight_cols <- wOBA_numer_weights$stat
  
  br_data %>% 
    mutate(uBB = BB - IBB,
           `1B` = H - `2B` - `3B` - HR) %>% 
    # we only want to keep columns that matter for wOBA calculation plus a few identifiers
    select(all_of(identifier_cols), all_of(weight_cols)) %>% 
    pivot_longer(all_of(weight_cols), names_to = "stat", values_to = "total")
}
  
# this function will compute 
compute_wOBA_components <- function(prepped_data, identifier_cols = c("Year", "Age", "Tm", "Lg", "G")) {
  prepped_data %>% 
    # attach the weights to the stat dataset
    left_join(wOBA_numer_weights, by = "stat") %>% 
    # apply the weights to calculate the numerator and denominator component of each outcome
    mutate(numer_component = total * numer_weight,
           denom_component = total * denom_weight) %>% 
    # and compute the total denominator (~ plate appearances)
    with_groups(Year, mutate, 
                denom_year = sum(denom_component),
                wOBA_year  = sum(numer_component) / denom_year) %>% 
    mutate(wOBA_component = numer_component / denom_year) %>% 
    select(all_of(identifier_cols), stat, total, 
           wOBA_component, wOBA_year, numer_component, denom_component, denom_year)
}

# this function will take components and give you annual wOBA
compute_wOBA <- function(prepped_data, identifier_cols = c("Year", "Age", "Tm", "Lg", "G")) {
  prepped_data %>% 
    compute_wOBA_components(identifier_cols = identifier_cols) %>% 
    with_groups(all_of(identifier_cols), summarize,
                numer = sum(numer_component),
                denom = sum(denom_component)) %>% 
    mutate(wOBA = numer / denom, .keep = "unused")
}
