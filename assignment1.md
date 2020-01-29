Statistical assignment 1
================
Francesca Hume 660024948
29/01/2020

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code
    below.

``` r
library("tidyverse")
```

    ## ── Attaching packages ──────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
Data <- read_tsv("/Users/chescamae98/Desktop/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result.

``` r
Data <- Data %>%
  select(pidp, h_memorig, h_eumem, h_sex_dv, h_age_dv)
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
Data<- Data %>%
  filter(h_memorig == 1)
```

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>
.

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.
2)  Recode sex into a character vector with the values “male” or
    “female”.
3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
Data <- Data %>%
  mutate(EU = case_when(
    h_eumem < 1 ~ NA_real_,
    h_eumem == 1 ~ 1,
    h_eumem == 2 ~ 0,
    )) %>%
  mutate(sex = recode(h_sex_dv,
                      `1` = "male",
                      `2` = "female",
                      `0` = NA_character_,
                      ))%>%
  mutate(agegr = case_when(
    between(h_age_dv, 16, 25) ~ "16 to 25",
    between(h_age_dv, 26, 40) ~ "26 to 40",
    between(h_age_dv, 41, 55) ~ "41 to 55",
    between(h_age_dv, 56, 70) ~ "56 to 70",
    h_age_dv > 70 ~ "over 70" ))



table(Data$EU)
```

    ## 
    ##     0     1 
    ##  9338 11118

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$sex)
```

    ## 
    ## female   male 
    ##  12486  10470

``` r
table(Data$agegr)
```

    ## 
    ## 16 to 25 26 to 40 41 to 55 56 to 70  over 70 
    ##     2885     4384     6150     5941     3597

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
Data %>%
  count(EU) %>%
  mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 3 x 3
    ##      EU     n  perc
    ##   <dbl> <int> <dbl>
    ## 1     0  9338  40.7
    ## 2     1 11118  48.4
    ## 3    NA  2501  10.9

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum? Why?

The results suggest that 40.7% of respondents answered that the UK
should leave the EU, whereas 48.4% answered that the UK should remain a
member of the EU. The 2016 referendum saw the majority voting to leave
the EU (52%) rather than remain (48%) which is the opposite of the
result implied by the Understanding society data. Some possible reasons
why this could be include the number of people in the understanding
society dataset that answered I dont know/ NA were more likely to then
vote to leave, also, the understanding society dataset does not include
the whole population of the UK, just a sample of it, and thus may not be
compeltely representative.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
summary(Data$EU)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.0000  0.0000  1.0000  0.5435  1.0000  1.0000    2501

``` r
Data %>%
  filter(!is.na(sex)) %>%
  mutate(remain = ifelse(EU == 1, TRUE,
                                   ifelse(EU == 0, FALSE, NA))) %>%
  group_by( sex, agegr) %>%
  summarise(
    propEU = mean(remain, na.rm = TRUE) * 100)
```

    ## # A tibble: 10 x 3
    ## # Groups:   sex [2]
    ##    sex    agegr    propEU
    ##    <chr>  <chr>     <dbl>
    ##  1 female 16 to 25   72.6
    ##  2 female 26 to 40   64.0
    ##  3 female 41 to 55   56.9
    ##  4 female 56 to 70   50.5
    ##  5 female over 70    44.2
    ##  6 male   16 to 25   66.1
    ##  7 male   26 to 40   58.8
    ##  8 male   41 to 55   51.7
    ##  9 male   56 to 70   47.6
    ## 10 male   over 70    37.6

Write a couple of sentences interpreting your results.

The shows the proportions of age gender in each age group that wanted to
remain in the EU. What can be seen is that in the younger age groups
(i.e. 16 - 25), generally a higher proportion of males and females
wanted to remain (i.e. 66% and 72% respectively in 16-25), than in the
older age groups (i.e. in the over 70s, only (44% of females and 38% of
males wished to remain). In addition, in every age group, a higher
proportion of females said they wished to remain than the proportion of
males.
