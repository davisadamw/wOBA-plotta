
<!-- README.md is generated from README.Rmd. Please edit that file -->

Inspired by [Emma Baccellieri’s](https://twitter.com/emmabaccellieri)
article [“White Sox’ Yasmani Grandal Is Doing the Most With the Least
but How Is He Making It
Work?”](https://www.si.com/mlb/2021/06/02/yasmani-grandal-the-opener).
Here’s my attempt to visualize Yasmani Grandal’s absurd 2021 stat line.

# Load the stats

We’re going to grab Yasmani Grandal’s stats from baseball-reference. You
could do this with an html handler, but it’s also pretty easy to just go
to [his br
page](https://www.baseball-reference.com/players/g/grandya01.shtml) and
grab his stat line in csv format. Conveniently, he’s never played for
multiple teams in a single season.

``` r
library(tidyverse)

# a.o 6/1
# https://www.baseball-reference.com/players/g/grandya01.shtml
yasmani_stats <- read_csv(
  "Year,Age,Tm,Lg,G,PA,AB,R,H,2B,3B,HR,RBI,SB,CS,BB,SO,BA,OBP,SLG,OPS,OPS+,TB,GDP,HBP,SH,SF,IBB,Pos,Awards
2012,23,SDP,NL,60,226,192,28,57,7,1,8,36,0,0,31,39,.297,.394,.469,.863,143,90,8,1,0,2,1,2/H,
2013,24,SDP,NL,28,108,88,13,19,8,0,1,9,0,0,18,18,.216,.352,.341,.693,102,30,1,1,0,1,2,2/H3,
2014,25,SDP,NL,128,443,377,47,85,19,1,15,49,3,0,58,115,.225,.327,.401,.728,111,151,7,2,0,6,1,23H/D,
2015,26,LAD,NL,115,426,355,43,83,12,0,16,47,0,1,65,92,.234,.353,.403,.756,112,143,16,2,1,3,1,2H/3,AS
2016,27,LAD,NL,126,457,390,49,89,14,1,27,72,1,3,64,116,.228,.339,.477,.816,118,186,11,2,0,1,1,*2H/3,MVP-22
2017,28,LAD,NL,129,482,438,50,108,27,0,22,58,0,1,40,130,.247,.308,.459,.767,101,201,10,0,1,3,0,*2H,
2018,29,LAD,NL,140,518,440,65,106,23,2,24,68,2,1,72,124,.241,.349,.466,.815,121,205,12,3,0,3,1,*2H/3,
2019,30,MIL,NL,153,632,513,79,126,26,2,28,77,5,1,109,139,.246,.380,.468,.848,119,240,16,5,0,5,2,*23/HD,ASMVP-15
2020,31,CHW,AL,46,194,161,27,37,7,0,8,27,0,0,30,58,.230,.351,.422,.773,112,68,4,1,0,2,0,2/D3H,
2021,32,CHW,AL,37,144,99,22,13,2,0,6,18,0,0,42,40,.131,.385,.333,.718,107,33,6,0,1,2,0,*2/3H,")
```

# Calculate wOBA

I’ve decided to do this analysis using [Fangraphs’s
wOBA](https://library.fangraphs.com/offense/woba/) in order to see how
different plate outcomes contribute to the total. In simple terms, the
formula includes positive outcomes in the numerator and negative
outcomes in the denominator.

``` r
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
```

A small amount of prep is needed to get the source data into the right
format for wOBA computation. Specifically, we’ll need to first compute
unintentional walks (uBB = BB - IBB) and singles (1B = H - 2B - 3B -
HR), and then pivot the data into a longer format, which will make
computation easier.

``` r
identifier_cols <- c("Year", "Age", "Tm", "Lg", "G")
weight_cols <- wOBA_numer_weights$stat

yasmani_stats_prepped <- yasmani_stats %>% 
  mutate(uBB = BB - IBB,
         `1B` = H - `2B` - `3B` - HR) %>% 
  # we only want to keep columns that matter for wOBA calculation plus a few identifiers
  select(all_of(identifier_cols), all_of(weight_cols)) %>% 
  pivot_longer(all_of(weight_cols), names_to = "stat", values_to = "total")

yasmani_stats_prepped
#> # A tibble: 80 x 7
#>     Year   Age Tm    Lg        G stat  total
#>    <dbl> <dbl> <chr> <chr> <dbl> <chr> <dbl>
#>  1  2012    23 SDP   NL       60 uBB      30
#>  2  2012    23 SDP   NL       60 HBP       1
#>  3  2012    23 SDP   NL       60 SF        2
#>  4  2012    23 SDP   NL       60 AB      192
#>  5  2012    23 SDP   NL       60 1B       41
#>  6  2012    23 SDP   NL       60 2B        7
#>  7  2012    23 SDP   NL       60 3B        1
#>  8  2012    23 SDP   NL       60 HR        8
#>  9  2013    24 SDP   NL       28 uBB      16
#> 10  2013    24 SDP   NL       28 HBP       1
#> # ... with 70 more rows
```

Once we’ve got the data in the right format, we can compute the
numerator and denominator components of each stat, each stat’s
contribution to the overall wOBA, and overall value of wOBA itself.

``` r
yasmani_components <- yasmani_stats_prepped %>% 
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
  select(all_of(identifier_cols), total, wOBA_component, wOBA_year, numer_component, denom_component, denom_year)

yasmani_components
#> # A tibble: 80 x 11
#>     Year   Age Tm    Lg        G total wOBA_component wOBA_year numer_component
#>    <dbl> <dbl> <chr> <chr> <dbl> <dbl>          <dbl>     <dbl>           <dbl>
#>  1  2012    23 SDP   NL       60    30        0.092       0.379           20.7 
#>  2  2012    23 SDP   NL       60     1        0.0032      0.379            0.72
#>  3  2012    23 SDP   NL       60     2        0           0.379            0   
#>  4  2012    23 SDP   NL       60   192        0           0.379            0   
#>  5  2012    23 SDP   NL       60    41        0.162       0.379           36.5 
#>  6  2012    23 SDP   NL       60     7        0.0395      0.379            8.89
#>  7  2012    23 SDP   NL       60     1        0.0072      0.379            1.62
#>  8  2012    23 SDP   NL       60     8        0.0747      0.379           16.8 
#>  9  2013    24 SDP   NL       28    16        0.104       0.311           11.0 
#> 10  2013    24 SDP   NL       28     1        0.00679     0.311            0.72
#> # ... with 70 more rows, and 2 more variables: denom_component <dbl>,
#> #   denom_year <dbl>
```

# Plotting wOBA components

First, a quick plot of Yasmani Grandal’s wOBA over the years against
some thresholds from [FanGraph’s
glossary](https://library.fangraphs.com/offense/woba/):

``` r
yasmani_wOBA_only <- yasmani_components %>% 
  with_groups(all_of(identifier_cols), summarize,
              numer = sum(numer_component),
              denom = sum(denom_component)) %>% 
  mutate(wOBA = numer / denom, .keep = "unused")


# a few other things
wOBA_thresholds <- tribble(
  ~Rating,         ~wOBA,
  "Excellent",     .400,
  "Great",         .370,
  "Above Average", .340,
  "Average",       .320,
  "Below Average", .310,
  "Poor",          .300,
  "Awful",         .290
)

years <- yasmani_wOBA_only$Year
comp_color <- "grey40"

yasmani_wOBA_only %>% 
  ggplot(aes(Year, wOBA)) + 
  geom_hline(aes(yintercept = wOBA), 
             data = wOBA_thresholds, 
             color = comp_color) +
  geom_text(aes(label = Rating), 
            data = wOBA_thresholds, 
            x = max(years) + 1, 
            nudge_y = 0.003,
            color = comp_color) +
  geom_line() +
  geom_point() +
  expand_limits(x = max(years + 1.5)) +
  scale_x_continuous(breaks = years, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     labels = scales::label_number()) +
  theme_bw()
```

![](README_files/figure-gfm/wOBA%20plot-1.png)<!-- -->

Then a first attempt to plot the impact that each hitting outcome had
each year.
