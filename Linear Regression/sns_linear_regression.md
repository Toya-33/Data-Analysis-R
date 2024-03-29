Movie Budget Analysis
================
Heng KongkeaOudong
December 30 2023

# Introduction

Do higher film budgets lead to more box office revenue? Let’s find out
if there’s a relationship using the movie budgets and financial
performance data that were scraped from
[the-numbers.com](https://www.the-numbers.com/movie/budgets) on **May
1st, 2018**.

![](https://i.imgur.com/kq7hrEh.png)

# Import Statements

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

# Read the Data

``` r
df <- read_csv("cost_revenue_dirty.csv")
```

    ## Rows: 5391 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Release_Date, Movie_Title, USD_Production_Budget, USD_Worldwide_Gro...
    ## dbl (1): Rank
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(df)
```

    ## # A tibble: 6 × 6
    ##    Rank Release_Date Movie_Title       USD_Production_Budget USD_Worldwide_Gross
    ##   <dbl> <chr>        <chr>             <chr>                 <chr>              
    ## 1  5293 8/2/1915     The Birth of a N… $110,000              $11,000,000        
    ## 2  5140 5/9/1916     Intolerance       $385,907              $0                 
    ## 3  5230 12/24/1916   20,000 Leagues U… $200,000              $8,000,000         
    ## 4  5299 9/17/1920    Over the Hill to… $100,000              $3,000,000         
    ## 5  5222 1/1/1925     The Big Parade    $245,000              $22,000,000        
    ## 6  4250 12/30/1925   Ben-Hur           $3,900,000            $9,000,000         
    ## # ℹ 1 more variable: USD_Domestic_Gross <chr>

# Cleaning the Data

Checking for NA values and duplicates.

``` r
is.null(df)
```

    ## [1] FALSE

``` r
sum(duplicated(df))
```

    ## [1] 0

``` r
glimpse(df)
```

    ## Rows: 5,391
    ## Columns: 6
    ## $ Rank                  <dbl> 5293, 5140, 5230, 5299, 5222, 4250, 4630, 5141, …
    ## $ Release_Date          <chr> "8/2/1915", "5/9/1916", "12/24/1916", "9/17/1920…
    ## $ Movie_Title           <chr> "The Birth of a Nation", "Intolerance", "20,000 …
    ## $ USD_Production_Budget <chr> "$110,000", "$385,907", "$200,000", "$100,000", …
    ## $ USD_Worldwide_Gross   <chr> "$11,000,000", "$0", "$8,000,000", "$3,000,000",…
    ## $ USD_Domestic_Gross    <chr> "$10,000,000", "$0", "$8,000,000", "$3,000,000",…

**Insight**: We noticed that there’s no NA values or duplicates which is
good, however we need to change the “Release_Date” to datetime and
“USD_Production_Budget”, “USD_Worldwide_Gross”, “USD_Domestic_Gross” to
double data-type.

## Changing the data types

``` r
string_to_rm <- list("[$]", "[,]")

#format data
for (r in string_to_rm){
  df$USD_Production_Budget <- str_replace_all(df$USD_Production_Budget,r,"")
  df$USD_Worldwide_Gross <- str_replace_all(df$USD_Worldwide_Gross, r, "")
  df$USD_Domestic_Gross <- str_replace_all(df$USD_Domestic_Gross, r, "")
}

#change data type
df$Release_Date <- as.Date(df$Release_Date, format = "%m/%d/%Y")
df$USD_Production_Budget <- as.numeric(df$USD_Production_Budget)
df$USD_Worldwide_Gross <- as.numeric(df$USD_Worldwide_Gross)
df$USD_Domestic_Gross <- as.numeric(df$USD_Domestic_Gross)

#check data
head(df)
```

    ## # A tibble: 6 × 6
    ##    Rank Release_Date Movie_Title       USD_Production_Budget USD_Worldwide_Gross
    ##   <dbl> <date>       <chr>                             <dbl>               <dbl>
    ## 1  5293 1915-08-02   The Birth of a N…                110000            11000000
    ## 2  5140 1916-05-09   Intolerance                      385907                   0
    ## 3  5230 1916-12-24   20,000 Leagues U…                200000             8000000
    ## 4  5299 1920-09-17   Over the Hill to…                100000             3000000
    ## 5  5222 1925-01-01   The Big Parade                   245000            22000000
    ## 6  4250 1925-12-30   Ben-Hur                         3900000             9000000
    ## # ℹ 1 more variable: USD_Domestic_Gross <dbl>

Now that the dataframe is cleaned, let’s proceed to the analysis phrase:

# Analysis

## Descriptive Statistics

1.  What is the average production budget of the films in the data set?
2.  What is the average worldwide gross revenue of films?
3.  What is the average domestic gross revenue of films?

``` r
sprintf("Average Production Budget: $%#.2f.",mean(df$USD_Production_Budget))
```

    ## [1] "Average Production Budget: $31113737.58."

``` r
sprintf("Average Worldwide Gross: $%#.2f.",mean(df$USD_Worldwide_Gross))
```

    ## [1] "Average Worldwide Gross: $88855421.96."

``` r
sprintf("Average Domestic Gross: $%#.2f.",mean(df$USD_Domestic_Gross))
```

    ## [1] "Average Domestic Gross: $41235519.44."

### Production Budget

``` r
summary(df$USD_Production_Budget)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##      1100   5000000  17000000  31113738  40000000 425000000

### Worldwide gross

``` r
summary(df$USD_Worldwide_Gross)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## 0.000e+00 3.865e+06 2.745e+07 8.886e+07 9.645e+07 2.784e+09

### Domestic gross

``` r
summary(df$USD_Domestic_Gross)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##         0   1330902  17192205  41235519  52343687 936662225

**Insight:** We observe that the average production cost for each film
is approximately 31 million dollars, with a mean worldwide gross revenue
of around 88 million dollars. Notably, the minimum gross revenue for
both global and domestic figures is 0 dollars, indicating that some
films fail to generate any revenue. Conversely, examining the highest
gross revenue for global and domestic yields staggering figures of 2.78
billion dollars and 936.66 million dollars, respectively, highlighting
the remarkable earning potential of certain films. Additionally, the
bottom 25% of films are profitable but not substantially so, with an
average budget of around 5 million dollars. Their combined revenue
slightly exceeds 5 million dollars, placing them just above the
break-even point.

# Investigating the Zero Revenue Films

How many films grossed \$0 domestically (i.e., in the United States)?
What were the highest budget films that grossed nothing?

``` r
zero_domestic <- df %>% 
  filter(USD_Domestic_Gross == 0)
zero_domestic <- zero_domestic[order(zero_domestic$USD_Production_Budget, decreasing = TRUE),]
zero_domestic
```

    ## # A tibble: 512 × 6
    ##     Rank Release_Date Movie_Title      USD_Production_Budget USD_Worldwide_Gross
    ##    <dbl> <date>       <chr>                            <dbl>               <dbl>
    ##  1    96 2020-12-31   Singularity                  175000000                   0
    ##  2   126 2018-12-18   Aquaman                      160000000                   0
    ##  3   321 2018-09-03   A Wrinkle in Ti…             103000000                   0
    ##  4   366 2018-10-08   Amusement Park               100000000                   0
    ##  5   556 2015-12-31   Don Gato, el in…              80000000             4547660
    ##  6   566 2012-12-31   Astérix et Obél…              77600000            60680125
    ##  7   880 2015-11-12   The Ridiculous 6              60000000                   0
    ##  8   879 2017-04-08   The Dark Tower                60000000                   0
    ##  9  1119 2020-12-31   Hannibal the Co…              50000000                   0
    ## 10  1230 2012-12-31   Foodfight!                    45000000               73706
    ## # ℹ 502 more rows
    ## # ℹ 1 more variable: USD_Domestic_Gross <dbl>

How many films grossed \$0 worldwide? What are the highest budget films
that had no revenue internationally?

``` r
zero_international <- df %>% filter(USD_Worldwide_Gross == 0)
zero_international <- zero_international[order(zero_international$USD_Production_Budget, decreasing = TRUE),]
zero_international
```

    ## # A tibble: 357 × 6
    ##     Rank Release_Date Movie_Title      USD_Production_Budget USD_Worldwide_Gross
    ##    <dbl> <date>       <chr>                            <dbl>               <dbl>
    ##  1    96 2020-12-31   Singularity                  175000000                   0
    ##  2   126 2018-12-18   Aquaman                      160000000                   0
    ##  3   321 2018-09-03   A Wrinkle in Ti…             103000000                   0
    ##  4   366 2018-10-08   Amusement Park               100000000                   0
    ##  5   880 2015-11-12   The Ridiculous 6              60000000                   0
    ##  6   879 2017-04-08   The Dark Tower                60000000                   0
    ##  7  1119 2020-12-31   Hannibal the Co…              50000000                   0
    ##  8  1435 2015-12-31   The Crow                      40000000                   0
    ##  9  1631 2008-12-31   Black Water Tra…              35000000                   0
    ## 10  1656 2015-10-30   Freaks of Nature              33000000                   0
    ## # ℹ 347 more rows
    ## # ℹ 1 more variable: USD_Domestic_Gross <dbl>

**Insight:** we can see that there is 512 films that have 0 gross
domestic revenue and 357 films that have 0 gross global revenue.

How many films were releases exclusively for international audience?

``` r
international_releases <- df %>% filter(USD_Domestic_Gross == 0 & USD_Worldwide_Gross != 0)
international_releases
```

    ## # A tibble: 155 × 6
    ##     Rank Release_Date Movie_Title      USD_Production_Budget USD_Worldwide_Gross
    ##    <dbl> <date>       <chr>                            <dbl>               <dbl>
    ##  1  4310 1956-02-16   Carousel                       3380000                3220
    ##  2  5087 2001-02-11   Everything Put …                500000                7890
    ##  3  3695 2001-12-31   The Hole                       7500000            10834406
    ##  4  4236 2003-12-31   Nothing                        4000000               63180
    ##  5  2513 2004-03-31   The Touch                     20000000             5918742
    ##  6  4623 2004-10-12   Freeze Frame                   2000000              105377
    ##  7  4747 2005-04-11   Wal-Mart: The H…               1500000               58692
    ##  8  2944 2005-12-31   Dungeons & Drag…              15000000              909822
    ##  9  4531 2005-12-31   Chicken Tikka M…               2160000               37865
    ## 10  1629 2006-09-22   Bandidas                      35000000            19282590
    ## # ℹ 145 more rows
    ## # ℹ 1 more variable: USD_Domestic_Gross <dbl>

**Insight:** We can see that there are a total of 155 films that were
exclusively released globally but not domestic which explained why there
is zero domestic gross revenue.

## Unreleased Films

Here we want to drop all of the films that are not release yet. So we
are going to create a new dataframe called `cleaned_df`.

``` r
# Date of Data Collection
scrape_date <- as.Date('2018-5-1', format="%Y-%m-%d")
not_released <- df %>% filter(Release_Date >= scrape_date)
not_released
```

    ## # A tibble: 7 × 6
    ##    Rank Release_Date Movie_Title       USD_Production_Budget USD_Worldwide_Gross
    ##   <dbl> <date>       <chr>                             <dbl>               <dbl>
    ## 1   321 2018-09-03   A Wrinkle in Time             103000000                   0
    ## 2   366 2018-10-08   Amusement Park                100000000                   0
    ## 3  2950 2018-10-08   Meg                            15000000                   0
    ## 4   126 2018-12-18   Aquaman                       160000000                   0
    ## 5    96 2020-12-31   Singularity                   175000000                   0
    ## 6  1119 2020-12-31   Hannibal the Con…              50000000                   0
    ## 7  2517 2020-12-31   Story of Bonnie …              20000000                   0
    ## # ℹ 1 more variable: USD_Domestic_Gross <dbl>

**Insight**: We find that the zero gross revenue data also contains
movies not yet released at the time of data collection like
“Singularity” and “Aquaman”, so our next step is to exclude these rows
from our dataframe.

``` r
cleaned_df <- df[df$Release_Date < scrape_date,]
cleaned_df
```

    ## # A tibble: 5,384 × 6
    ##     Rank Release_Date Movie_Title      USD_Production_Budget USD_Worldwide_Gross
    ##    <dbl> <date>       <chr>                            <dbl>               <dbl>
    ##  1  5293 1915-08-02   The Birth of a …                110000            11000000
    ##  2  5140 1916-05-09   Intolerance                     385907                   0
    ##  3  5230 1916-12-24   20,000 Leagues …                200000             8000000
    ##  4  5299 1920-09-17   Over the Hill t…                100000             3000000
    ##  5  5222 1925-01-01   The Big Parade                  245000            22000000
    ##  6  4250 1925-12-30   Ben-Hur                        3900000             9000000
    ##  7  4630 1927-12-08   Wings                          2000000                   0
    ##  8  5141 1929-01-02   The Broadway Me…                379000             4358000
    ##  9  4240 1930-01-01   Hell's Angels                  4000000                   0
    ## 10  5043 1931-12-31   Mata Hari                       558000              900000
    ## # ℹ 5,374 more rows
    ## # ℹ 1 more variable: USD_Domestic_Gross <dbl>

## Films that Lost Money

What is the percentage of films where the production costs exceeded
gross revenue?

``` r
cleaned_df <- cleaned_df %>% 
  mutate(
    Total_Revenue = USD_Worldwide_Gross + USD_Domestic_Gross
  )

losing_money <- cleaned_df %>% filter(USD_Production_Budget > Total_Revenue)
pct_losing_money <- (nrow(losing_money)/nrow(cleaned_df))*100
cat("The percentage that films were losing money is %", pct_losing_money)
```

    ## The percentage that films were losing money is % 28.10178

# Data Viz: Bubble Charts

``` r
ggplot(cleaned_df, aes(x = USD_Production_Budget, y=Total_Revenue)) + 
  geom_point(color = "blue") +
  labs(title = "Revenue And Expense") +
  xlab("Production Budget in $100 millions") +
  ylab("Revenue in $ billions")
```

![](sns_linear_regression_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
<br />**Insight:** The scatter plot shows that most data points cluster
in the bottom left corner. This suggests that the majority of films earn
low revenues compared to their high production costs, indicating that
most films either face losses or gain minimal profits.

## Plotting Movie Revenue over Time

``` r
ggplot(cleaned_df, aes(x = Release_Date, y=Total_Revenue)) + 
  geom_point(aes(color = Total_Revenue)) +
  labs(title = "Movie Revenue Over Time") +
  xlab("Release Date") +
  ylab("Revenue in $ billions") +
  scale_color_viridis(option = "D")
```

![](sns_linear_regression_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

<br />**Insight**: The graph indicates an upward trend in the highest
gross revenues for films over time. Additionally, it shows an increase
in the number of film productions, as evidenced by the large cluster of
dots in the bottom right corner.

# Converting Years to Decades Trick

Let’s broaden our view by examining the trends in film over the decades.

``` r
cleaned_df <- cleaned_df %>% mutate(Decades = as.integer(format(cleaned_df$Release_Date, "%Y")))
cleaned_df$Decades <- (cleaned_df$Decades %/% 10)*10
```

## Separate the “old” (before 1969) and “New” (1970s onwards) Films

- `old_films` should include all the films before 1969 (up to and
  including 1969)
- `new_films` should include all the films from 1970 onwards

``` r
old_films <- cleaned_df %>% filter(Decades < 1970)
new_films <- cleaned_df %>% filter(Decades >= 1970)
```

### Old films

``` r
summary(old_films)
```

    ##       Rank       Release_Date        Movie_Title        USD_Production_Budget
    ##  Min.   :1253   Min.   :1915-08-02   Length:153         Min.   :  100000     
    ##  1st Qu.:3973   1st Qu.:1946-01-01   Class :character   1st Qu.: 1250000     
    ##  Median :4434   Median :1956-12-23   Mode  :character   Median : 2900000     
    ##  Mean   :4275   Mean   :1954-06-10                      Mean   : 4611298     
    ##  3rd Qu.:4785   3rd Qu.:1964-10-22                      3rd Qu.: 5000000     
    ##  Max.   :5299   Max.   :1969-12-19                      Max.   :42000000     
    ##  USD_Worldwide_Gross USD_Domestic_Gross  Total_Revenue          Decades    
    ##  Min.   :        0   Min.   :        0   Min.   :        0   Min.   :1910  
    ##  1st Qu.:  5273000   1st Qu.:  5000000   1st Qu.: 10000000   1st Qu.:1940  
    ##  Median : 10000000   Median : 10000000   Median : 20000000   Median :1950  
    ##  Mean   : 30419634   Mean   : 22389474   Mean   : 52809108   Mean   :1949  
    ##  3rd Qu.: 33208099   3rd Qu.: 28350000   3rd Qu.: 64489436   3rd Qu.:1960  
    ##  Max.   :390525192   Max.   :198680470   Max.   :589205662   Max.   :1960

### New Films

``` r
summary(new_films)
```

    ##       Rank       Release_Date        Movie_Title        USD_Production_Budget
    ##  Min.   :   1   Min.   :1970-01-01   Length:5231        Min.   :     1100    
    ##  1st Qu.:1314   1st Qu.:2000-07-14   Class :character   1st Qu.:  5000000    
    ##  Median :2629   Median :2006-10-03   Mode  :character   Median : 18000000    
    ##  Mean   :2652   Mean   :2005-02-19                      Mean   : 31811438    
    ##  3rd Qu.:3968   3rd Qu.:2012-01-06                      3rd Qu.: 40000000    
    ##  Max.   :5391   Max.   :2017-12-31                      Max.   :425000000    
    ##  USD_Worldwide_Gross USD_Domestic_Gross  Total_Revenue          Decades    
    ##  Min.   :0.000e+00   Min.   :        0   Min.   :0.000e+00   Min.   :1970  
    ##  1st Qu.:3.832e+06   1st Qu.:  1252581   1st Qu.:6.109e+06   1st Qu.:2000  
    ##  Median :2.854e+07   Median : 17510118   Median :4.707e+07   Median :2000  
    ##  Mean   :9.068e+07   Mean   : 41841922   Mean   :1.325e+08   Mean   :2000  
    ##  3rd Qu.:9.878e+07   3rd Qu.: 53301583   3rd Qu.:1.536e+08   3rd Qu.:2010  
    ##  Max.   :2.784e+09   Max.   :936662225   Max.   :3.544e+09   Max.   :2010

**Insight**: Based on this quick data summary, we learnt that new films
are able to earn drastically more money than the old films, despite the
small different in production cost. The most expensive new film had a
production cost of 425 million and earned a maximum of 3.54 billion,
whereas the most expensive old film was 420 million with a revenue of
only around 589 million, almost seven times less than what new films
make. There are a couple reasons for this: 1. The are more people
enjoying films 2. Higher films ticket price 3. Production company
utilize more advance technology which reduce production cost.

# Regression Plots

**Linear Regression Formula:**
$$ REV \hat ENUE = \theta _0 + \theta _1 BUDGET$$ \## New Films

``` r
ggplot(new_films, aes(x = USD_Production_Budget, y= Total_Revenue)) + 
  geom_point(color = "darksalmon") +
  geom_smooth(method=lm)+ 
  labs(title = "Revenue VS Production Cost (New Films)") +
  xlab("Production Cost in $100 million")+
  ylab("Total Revenue in $billion")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](sns_linear_regression_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
lm_revenue <- lm(data = new_films, formula = Total_Revenue ~ USD_Production_Budget)
summary(lm_revenue)
```

    ## 
    ## Call:
    ## lm(formula = Total_Revenue ~ USD_Production_Budget, data = new_films)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -816126371  -54866328   -7454735   19454750 2019408076 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           -2.589e+06  2.764e+06  -0.936    0.349    
    ## USD_Production_Budget  4.247e+00  5.347e-02  79.434   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 157600000 on 5229 degrees of freedom
    ## Multiple R-squared:  0.5468, Adjusted R-squared:  0.5467 
    ## F-statistic:  6310 on 1 and 5229 DF,  p-value: < 2.2e-16

**Insight:** In summary, our regression model features an intercept of
-2.589e+06 or -2.589 million, a coefficient of 4.247, and an R-squared
value of 0.5468.

It’s important to note that when the film budget is zero, the estimated
revenue is -2.589 million, which is impractical since how can we lost
money without creating a film. One reason for this is that our model is
very simple, we only predefined it as a straight line and find the best
fit for our overall data, thus we need to be cautious on how we
interpret our model.

The coefficient of 4.247 implies that a one-dollar increase in the
budget corresponds to a 4.247 dollar increase in revenue. Lastly, the
R-squared value of approximately 0.5468 suggests that our model can
explain about 54.68% of the variance in revenue data, showcasing a
reasonably accurate prediction considering how simple our model is with
only one independent and dependent variable.

## Old Films

``` r
lm_revenue <- lm(data = old_films, formula = Total_Revenue ~ USD_Production_Budget)
summary(lm_revenue)
```

    ## 
    ## Call:
    ## lm(formula = Total_Revenue ~ USD_Production_Budget, data = old_films)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -87773016 -36739687 -26973382   4189797 538594603 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           3.856e+07  8.734e+06   4.415 1.92e-05 ***
    ## USD_Production_Budget 3.090e+00  1.192e+00   2.593   0.0105 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 83960000 on 151 degrees of freedom
    ## Multiple R-squared:  0.04262,    Adjusted R-squared:  0.03628 
    ## F-statistic: 6.722 on 1 and 151 DF,  p-value: 0.01046

**Insight**: Regarding old movies, our model’s R-squared value is only
0.04262, which is quite low. This suggests that our simple model fails
to predict the relationship between a one-unit increase in production
budget and the change in gross revenue. Therefore, our model is
ineffective for predicting the gross revenue of old films.

``` r
ggplot(old_films, aes(x = USD_Production_Budget, y= Total_Revenue)) + 
  geom_point(color = "darksalmon") +
  geom_smooth(method=lm)+ 
  labs(title = "Revenue VS Production Cost (Old Films)") +
  xlab("Production Cost in $100 million")+
  ylab("Total Revenue in $billion")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](sns_linear_regression_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
<br />**Insight**: Here is a visualization of our model for old films.
There are hardly any dots on our regression line.
