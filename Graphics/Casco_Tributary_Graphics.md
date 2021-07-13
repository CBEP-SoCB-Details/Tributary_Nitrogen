Tributary Nitrogen Concentrations
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
04/22/2021

-   [Install Libraries](#install-libraries)
-   [Read Data](#read-data)
-   [Summary Statistics](#summary-statistics)
-   [Exploratory Graphics](#exploratory-graphics)
    -   [Pairs Plot](#pairs-plot)
-   [Alternative Graphics](#alternative-graphics)
    -   [Density Plots](#density-plots)
    -   [Histograms](#histograms)
    -   [Dot Plot](#dot-plot)
    -   [Box Plot](#box-plot)
        -   [Add Boxplot Annotation](#add-boxplot-annotation)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Install Libraries

``` r
library(readxl)
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.0.5
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.1.1     v dplyr   1.0.5
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> Warning: package 'tibble' was built under R version 4.0.5
#> Warning: package 'tidyr' was built under R version 4.0.5
#> Warning: package 'dplyr' was built under R version 4.0.5
#> Warning: package 'forcats' was built under R version 4.0.5
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(GGally)
#> Warning: package 'GGally' was built under R version 4.0.5
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Read Data

``` r
sibfldnm <- 'Original_Data'
parent <- dirname(getwd())
sibling <- paste(parent,sibfldnm, sep = '/')
fn <- '2017-18 Casco Bay Tributary Nitrogen Concentrations.xlsx'

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

``` r
the_data <- read_excel(file.path(sibling, fn), 
                       col_types = c("date", "numeric", "numeric", 
                                     "numeric", "numeric"), skip = 1) %>%
  filter( ! is.na(Date)) %>%
  rename(dt = Date,
         tn = `TN (mg/l)`,
         nox = `NOx (mg/L)`,
         nh4 = `NH4 (mg/L)`,
         organic = `Organic (mg/L)`) %>%
  mutate(dt = as.Date(dt))
```

``` r
the_data$tributary <- c(rep('Presumpscot',17), 
                        rep('Royal', 19), 
                        rep('Capisic', 16))
the_data <- the_data %>%
  relocate(tributary) %>%
  mutate(tributary = factor(tributary, levels = c('Capisic', 'Royal', 'Presumpscot')))
```

# Summary Statistics

``` r
tmp <- the_data %>%
  group_by(tributary) %>%
  summarize(across(tn:organic,
                   .fns = list(mean = ~mean(.x, na.rm = TRUE),
                               sd   = ~mean(.x, na.rm = TRUE),
                               n    = ~ sum(!is.na(.x))),
                   .names = '{.col}_{.fn}' ))
tmp
#> # A tibble: 3 x 13
#>   tributary   tn_mean tn_sd  tn_n nox_mean nox_sd nox_n nh4_mean nh4_sd nh4_n
#>   <fct>         <dbl> <dbl> <int>    <dbl>  <dbl> <int>    <dbl>  <dbl> <int>
#> 1 Capisic       0.932 0.932    16   0.415  0.415     16   0.0624 0.0624    16
#> 2 Royal         0.513 0.513    19   0.220  0.220     18   0.0237 0.0237    19
#> 3 Presumpscot   0.229 0.229    16   0.0430 0.0430    17   0.0175 0.0175    17
#> # ... with 3 more variables: organic_mean <dbl>, organic_sd <dbl>,
#> #   organic_n <int>
```

``` r
tmp <- the_data %>%
  pivot_longer(tn:organic, names_to = 'Parameter', values_to = 'Value') %>%
  group_by(Parameter, tributary) %>%
  summarize(mean = round(mean(Value, na.rm = TRUE), 3),
            sd   = round(sd(Value, na.rm = TRUE), 4),
            n    = sum(!is.na(Value)),
            .groups = 'drop')
knitr::kable(tmp, col.names = c('Parameter', 'Tributary', 'Mean', 'Standard Deviation', 'Sample Size'))
```

| Parameter | Tributary   |  Mean | Standard Deviation | Sample Size |
|:----------|:------------|------:|-------------------:|------------:|
| nh4       | Capisic     | 0.062 |             0.0299 |          16 |
| nh4       | Royal       | 0.024 |             0.0119 |          19 |
| nh4       | Presumpscot | 0.018 |             0.0155 |          17 |
| nox       | Capisic     | 0.415 |             0.0969 |          16 |
| nox       | Royal       | 0.220 |             0.1205 |          18 |
| nox       | Presumpscot | 0.043 |             0.0278 |          17 |
| organic   | Capisic     | 0.455 |             0.1627 |          16 |
| organic   | Royal       | 0.280 |             0.1012 |          19 |
| organic   | Presumpscot | 0.172 |             0.0776 |          16 |
| tn        | Capisic     | 0.932 |             0.1503 |          16 |
| tn        | Royal       | 0.513 |             0.0908 |          19 |
| tn        | Presumpscot | 0.229 |             0.0956 |          16 |

# Exploratory Graphics

## Pairs Plot

``` r
plt <- ggpairs(the_data[3:6], 
               mapping = aes(color =the_data$tributary), progress = FALSE)
plt
#> Warning: Removed 1 rows containing non-finite values (stat_density).
#> Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
#> Removed 2 rows containing missing values
#> Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
#> Removing 1 row that contained a missing value

#> Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
#> Removing 1 row that contained a missing value
#> Warning: Removed 2 rows containing missing values (geom_point).
#> Warning: Removed 1 rows containing non-finite values (stat_density).
#> Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
#> Removing 1 row that contained a missing value
#> Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
#> Removed 2 rows containing missing values
#> Warning: Removed 1 rows containing missing values (geom_point).

#> Warning: Removed 1 rows containing missing values (geom_point).
#> Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
#> Removing 1 row that contained a missing value
#> Warning: Removed 1 rows containing missing values (geom_point).
#> Warning: Removed 2 rows containing missing values (geom_point).
#> Warning: Removed 1 rows containing missing values (geom_point).
#> Warning: Removed 1 rows containing non-finite values (stat_density).
```

<img src="Casco_Tributary_Graphics_files/figure-gfm/pairs_plot-1.png" style="display: block; margin: auto;" />

Quickly reviewing that, you see:  
1. TN is quite distinct among all three sources. The Presumpscot has
very low values for all parameters.  
2. Ammonium is lower in the rivers, and higher at Capisic – although
given small sample sizes, that may not be statistically significant.

# Alternative Graphics

## Density Plots

``` r
plt <- ggplot(the_data, aes(x=tn, y = ..density.., fill= tributary)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values=cbep_colors(), name = '') +
  xlab("Total Nitrogen (mg/l)") + 
  ylab("Density") +
  theme(legend.position = c(0.75, 0.75))
plt
#> Warning: Removed 1 rows containing non-finite values (stat_density).
```

<img src="Casco_Tributary_Graphics_files/figure-gfm/density_plot-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/densityplot.pdf', device = cairo_pdf, width = 5, height = 4)
#> Warning: Removed 1 rows containing non-finite values (stat_density).
```

``` r
plt <- ggplot(the_data, aes(x=tn, fill = tributary)) + 
  geom_area(aes(fill = tributary), stat = "bin", alpha = 0.5) +
  scale_fill_manual(values=cbep_colors(), name = '') +
  xlab("Total Nitrogen (mg/l)") + 
  ylab("Observations") +
  theme(legend.position = c(0.75, 0.75))
plt
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> Warning: Removed 1 rows containing non-finite values (stat_bin).
```

<img src="Casco_Tributary_Graphics_files/figure-gfm/area_plot-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/densitypoly.pdf', device = cairo_pdf, width = 5, height = 4)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> Warning: Removed 1 rows containing non-finite values (stat_bin).
```

## Histograms

Histograms are a bit more “honest”, since they don’t bury the tiny
sample size. But neither of these are particularly attractive from a
design perspective.

``` r
plt <- ggplot(the_data, aes(x=tn, fill= tributary)) +
  geom_histogram(position = position_dodge2(preserve = "single", padding = -0.75),
                 alpha = 0.5, 
                 binwidth = 0.1) + 
   scale_fill_manual(values=cbep_colors(), name = '') +
  xlab('Total Nitrogen (mg/l)') + 
  ylab('Observations') +
  theme(legend.position = c(0.8, 0.75))
plt
#> Warning: Removed 1 rows containing non-finite values (stat_bin).
```

<img src="Casco_Tributary_Graphics_files/figure-gfm/histogram_1-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/histogram1.pdf', device = cairo_pdf, width = 5, height = 4)
#> Warning: Removed 1 rows containing non-finite values (stat_bin).
```

``` r
plt <- ggplot(the_data, aes(x=tn, fill= tributary)) +
  geom_histogram(data = subset(the_data, tributary == 'Capisic'),
                 fill = cbep_colors()[1], 
                 alpha = 0.3, 
                 binwidth = 0.1, 
                 color = 'black') +
  geom_histogram(data = subset(the_data, tributary == 'Royal'),
                 fill = cbep_colors()[2], alpha = 0.3, binwidth = 0.1, color = 'black') +
  geom_histogram(data = subset(the_data, tributary == 'Presumpscot'),
                 fill = cbep_colors()[3], alpha = 0.3, binwidth = 0.1, color = 'black') +
  xlab('Total Nitrogen (mg/l)') + ylab('Observations') +
  theme_minimal() +
  theme(panel.grid = element_blank())
plt
#> Warning: Removed 1 rows containing non-finite values (stat_bin).
```

<img src="Casco_Tributary_Graphics_files/figure-gfm/histogram_2-1.png" style="display: block; margin: auto;" />

## Dot Plot

``` r
plt <- ggplot(the_data, aes(x=tributary, y = tn)) +
  geom_dotplot(binaxis='y', stackdir='center', 
               binwidth = 0.025, dotsize = 1.25, 
               fill = cbep_colors()[5], color = cbep_colors()[3]) +
                                                              
  ylab('Total Nitrogen (mg/l)') + 
  xlab('') +
    
 # ylim(c(0,1.5)) +
  theme_cbep(base_size = 12)
plt
#> Warning: Removed 1 rows containing non-finite values (stat_bindot).
```

<img src="Casco_Tributary_Graphics_files/figure-gfm/dot_plot-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/dotplot.pdf', device = cairo_pdf, width = 3.5, height = 2.5)
#> Warning: Removed 1 rows containing non-finite values (stat_bindot).
```

## Box Plot

``` r
plt <- ggplot(the_data, aes(x=tributary, y = tn)) +

  geom_boxplot(width = .75, color = cbep_colors()[3],
               outlier.shape = NA,
               #coef = 0
               ) +
  geom_jitter(width = 0.075,  height = 0,
             # alpha = 0.5,
              color = cbep_colors()[5]) +

  ylab('Total Nitrogen (mg/l)') + 
  xlab('') +
  
  ylim(0, NA) +
  
  theme_cbep(base_size = 12)
plt
#> Warning: Removed 1 rows containing non-finite values (stat_boxplot).
#> Warning: Removed 1 rows containing missing values (geom_point).
```

<img src="Casco_Tributary_Graphics_files/figure-gfm/box_plot-1.png" style="display: block; margin: auto;" />

### Add Boxplot Annotation

``` r
xanchor = 2.25
yanchor = 1
ysize = .075
xsize = .1

plt +
  
  annotate('rect', xmin = xanchor, ymin = yanchor - ysize, 
                   xmax = xanchor + xsize, ymax = yanchor + ysize,
           fill = 'white', color = 'gray30', size = .5) + 
  annotate('segment', x= xanchor, y = yanchor, xend = xanchor + 0.1, yend = yanchor, 
           color = 'gray30') +
  
  annotate('text', x= xanchor + xsize*1.5, y = yanchor - ysize,
           hjust = 0, size = 3, label = '25th percentile') +
  annotate('text', x= xanchor + xsize*1.5, y = yanchor,
           hjust = 0, size = 3, label = 'median') +
  annotate('text', x= xanchor + xsize*1.5, y = yanchor + ysize,
           hjust = 0, size = 3, label = '75th percentile')
#> Warning: Removed 1 rows containing non-finite values (stat_boxplot).
#> Warning: Removed 1 rows containing missing values (geom_point).
```

<img src="Casco_Tributary_Graphics_files/figure-gfm/final_graphic-1.png" style="display: block; margin: auto;" />

``` r

ggsave('figures/boxplot.pdf', device = cairo_pdf, width = 3.5, height = 3.5)
#> Warning: Removed 1 rows containing non-finite values (stat_boxplot).

#> Warning: Removed 1 rows containing missing values (geom_point).
```
