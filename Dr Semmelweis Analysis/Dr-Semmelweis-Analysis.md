Dr Semmelweis Analysis
================
Heng KongkeaOudong
December 30 2023

# Setup and Context

## Introduction

Dr Ignaz Semmelweis was a Hungarian physician born in 1818 who worked in
the Vienna General Hospital. In the past people thought of illness as
caused by “bad air” or evil spirits. But in the 1800s Doctors started
looking more at anatomy, doing autopsies and started making arguments
based on data. Dr Semmelweis suspected that something was going wrong
with the procedures at Vienna General Hospital. Semmelweis wanted to
figure out why so many women in maternity wards were dying from childbed
fever (i.e., [puerperal
fever](https://en.wikipedia.org/wiki/Postpartum_infections)).

Today we will become Dr Semmelweis. We will step into Dr Semmelweis’
shoes and analyse the same data collected from 1841 to 1849.

## The Data Source

Dr Semmelweis published his research in 1861. [full text with the
original tables in
German](http://www.deutschestextarchiv.de/book/show/semmelweis_kindbettfieber_1861),
[English translation can be found
here](http://graphics8.nytimes.com/images/blogs/freakonomics/pdf/the%20etiology,%20concept%20and%20prophylaxis%20of%20childbed%20fever.pdf).

## Libraries

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)  
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.4
    ## ✔ ggplot2   3.4.3     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.2     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.1     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)  
library(ggplot2)
library(readr)
library(stringr)
library(viridis)
```

    ## Warning: package 'viridis' was built under R version 4.3.2

    ## Loading required package: viridisLite

## Read the data

``` r
monthly_df <- read_csv('monthly_deaths.csv')
```

    ## Rows: 98 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (2): births, deaths
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
yearly_df <- read_csv('annual_deaths_by_clinic.csv')
```

    ## Rows: 12 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): clinic
    ## dbl (3): year, births, deaths
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Preliminary Data Exploration

Check out these two DataFrames.

- What is the shape of yearly_df and monthly_df? How many rows and
  columns?
- What are the column names?
- Which years are included in the dataset?
- Are there any NaN values or duplicates?
- What were the average number of births that took place per month?
- What were the average number of deaths that took place per month?

## Yearly dataframe

``` r
yearly_df
```

    ## # A tibble: 12 × 4
    ##     year births deaths clinic  
    ##    <dbl>  <dbl>  <dbl> <chr>   
    ##  1  1841   3036    237 clinic 1
    ##  2  1842   3287    518 clinic 1
    ##  3  1843   3060    274 clinic 1
    ##  4  1844   3157    260 clinic 1
    ##  5  1845   3492    241 clinic 1
    ##  6  1846   4010    459 clinic 1
    ##  7  1841   2442     86 clinic 2
    ##  8  1842   2659    202 clinic 2
    ##  9  1843   2739    164 clinic 2
    ## 10  1844   2956     68 clinic 2
    ## 11  1845   3241     66 clinic 2
    ## 12  1846   3754    105 clinic 2

``` r
is.null(yearly_df)
```

    ## [1] FALSE

``` r
sum(duplicated(yearly_df))
```

    ## [1] 0

``` r
summary(yearly_df)
```

    ##       year          births         deaths         clinic         
    ##  Min.   :1841   Min.   :2442   Min.   : 66.0   Length:12         
    ##  1st Qu.:1842   1st Qu.:2902   1st Qu.:100.2   Class :character  
    ##  Median :1844   Median :3108   Median :219.5   Mode  :character  
    ##  Mean   :1844   Mean   :3153   Mean   :223.3                     
    ##  3rd Qu.:1845   3rd Qu.:3338   3rd Qu.:263.5                     
    ##  Max.   :1846   Max.   :4010   Max.   :518.0

**Insight**: The `yearly_df` is cleaned, with no missing values or
duplicates. The average annual births and deaths are 3153 and 223,
respectively.

## Monthly dataframe

``` r
monthly_df
```

    ## # A tibble: 98 × 3
    ##    date       births deaths
    ##    <date>      <dbl>  <dbl>
    ##  1 1841-01-01    254     37
    ##  2 1841-02-01    239     18
    ##  3 1841-03-01    277     12
    ##  4 1841-04-01    255      4
    ##  5 1841-05-01    255      2
    ##  6 1841-06-01    200     10
    ##  7 1841-07-01    190     16
    ##  8 1841-08-01    222      3
    ##  9 1841-09-01    213      4
    ## 10 1841-10-01    236     26
    ## # ℹ 88 more rows

``` r
is.null(monthly_df)
```

    ## [1] FALSE

``` r
sum(duplicated(monthly_df))
```

    ## [1] 0

``` r
summary(monthly_df)
```

    ##       date                births          deaths     
    ##  Min.   :1841-01-01   Min.   :190.0   Min.   : 0.00  
    ##  1st Qu.:1843-02-08   1st Qu.:242.5   1st Qu.: 8.00  
    ##  Median :1845-02-15   Median :264.0   Median :16.50  
    ##  Mean   :1845-02-11   Mean   :267.0   Mean   :22.47  
    ##  3rd Qu.:1847-02-22   3rd Qu.:292.8   3rd Qu.:36.75  
    ##  Max.   :1849-03-01   Max.   :406.0   Max.   :75.00

**Insight**: The `monthly_df` is also cleaned. In the monthly time
frame, the average births and deaths are 267 and about 22, respectively.

# Percentage of Women Dying in Childbirth

How dangerous was childbirth in the 1840s in Vienna?

- Using the annual data, calculate the percentage of women giving birth
  who died throughout the 1840s at the hospital.

In comparison, the United States recorded 18.5 maternal deaths per
100,000 or 0.018% in 2013
[(source).](https://en.wikipedia.org/wiki/Maternal_death#:~:text=The%20US%20has%20the%20%22highest,17.8%20per%20100%2C000%20in%202009)
