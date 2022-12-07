Statistical Modelling
================

 
``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(purrr)
library(modelr)
library(dbplyr)
```

    ## 
    ## 载入程辑包：'dbplyr'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     ident, sql

``` r
library(plyr)
```

    ## ------------------------------------------------------------------------------
    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)
    ## ------------------------------------------------------------------------------
    ## 
    ## 载入程辑包：'plyr'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
library(lubridate)
```

    ## 
    ## 载入程辑包：'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

 
### Data cleaning

``` r
setwd(dir = "./data")
a <- list.files()     
a 
```

    ## [1] "cleaned_NYPD_2016.csv" "cleaned_NYPD_2017.csv" "cleaned_NYPD_2018.csv"
    ## [4] "cleaned_NYPD_2019.csv" "cleaned_NYPD_2020.csv" "cleaned_NYPD_2021.csv"
    ## [7] "cleaned_NYPD_2022.csv"

``` r
dir <- paste("./",a,sep="")                 
n <- length(dir) 
merge_data <- read.csv(file = dir[1],header=T,sep=",")   
merge_data <- cbind(dir[1], merge_data )
merge_data <- rename(merge_data, c("dir[1]"="yyyymm"))
for (i in 2:7){
  new.data <- read.csv(file = dir[i], header=T, sep=",")
  new.data <- cbind(dir[i], new.data)
  new.data <- rename(new.data, c("dir[i]"="yyyymm"))
  merge_data <- rbind(merge_data,new.data)
}
```

### Data cleaning, drop data with unknown

``` r
# filter data with unknown
tidydata1=merge_data%>%
  na.omit()%>%
  filter(!susp_sex == "U")%>%
  filter(!susp_race == "UNKNOWN")%>%
  filter(!susp_age_group == "UNKNOWN")%>%
   filter(!vic_race == "UNKNOWN")%>%
  filter(!vic_age_group == "UNKNOWN")%>%
  separate(date,into=c("mon","day","year"),sep="/")%>%
  mutate(jurisdiction_code=as.numeric(jurisdiction_code))%>%
  mutate(susp_sex=ifelse(susp_sex=="M",1,0),vic_sex=ifelse(vic_sex=="M",1,0),jurisdiction_code=ifelse(jurisdiction_code>=3,3,jurisdiction_code+0))%>%
  mutate(jurisdiction_code=as.factor(jurisdiction_code))%>%
  mutate(covid_state=ifelse(year>=2020,1,0))
# when year >=2020, we assume the world is in a covid state.
```

``` r
tidydata3=tidydata1 %>%
  mutate(
   time= substring(time, 1,2)
  )

tidydata3= tidydata3 %>% 
 mutate(
   time1=substring(time,1,1),
   time2=substring(time,2,2),
   time3=ifelse(time1=="0","",time1),
   time4=paste(time3,time2,sep=""),
   time5=as.numeric(time4)
 ) 
  tidydata3=tidydata3 %>% 
  mutate(time_range=case_when(
      (time5 > 22) | (time5 <= 6) ~ 0,
      (time5>6) & (time5<=14) ~ 1,
      (time5>14) & (time5<=22) ~ 2
    ))
```

This is the beginning of test First of all let’s define the “danger” as
the *total number of crime event* occure in *one day*

# dangers define as the criminal events happen by day

## 1.let’s see does it correlate to the criminal’s gender?

``` r
crimebyday_men=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  filter(susp_sex==1) %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
crimebyday_women=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  filter(susp_sex==0) %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
t.test(crimebyday_men$n_bydate,crimebyday_women$n_bydate)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  crimebyday_men$n_bydate and crimebyday_women$n_bydate
    ## t = 235.61, df = 3517.7, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  174.9619 177.8982
    ## sample estimates:
    ## mean of x mean of y 
    ## 269.11440  92.68438

significant difference

## 2.let’s see does it correlate to the victim’s gender?

``` r
crimebyday_men=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  filter(vic_sex==1) %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
crimebyday_women=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  filter(vic_sex==0) %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
t.test(crimebyday_men$n_bydate,crimebyday_women$n_bydate)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  crimebyday_men$n_bydate and crimebyday_women$n_bydate
    ## t = -133.2, df = 4302.4, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -96.09079 -93.30312
    ## sample estimates:
    ## mean of x mean of y 
    ##  133.5509  228.2479

significant difference female victims are significantly higher than male

## 3.let’s see does it correlate to the criminals’ race?

``` r
crimebyday_balck=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  filter(susp_race=="BLACK") %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
crimebyday_white=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  filter(susp_race=="WHITE") %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
t.test(crimebyday_balck$n_bydate,crimebyday_white$n_bydate)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  crimebyday_balck$n_bydate and crimebyday_white$n_bydate
    ## t = 260.11, df = 3286.2, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  129.0998 131.0609
    ## sample estimates:
    ## mean of x mean of y 
    ## 177.31075  47.23043

significant the number of white race criminals are significantly lower
than black.

## 4.What about other criminals’ race?

``` r
crimebyday_aspa=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  filter(susp_race=="ASIAN / PACIFIC ISLANDER") %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
crimebyday_white=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  filter(susp_race=="WHITE") %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
t.test(crimebyday_aspa$n_bydate,crimebyday_white$n_bydate)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  crimebyday_aspa$n_bydate and crimebyday_white$n_bydate
    ## t = -114.75, df = 4092.9, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -26.22925 -25.34803
    ## sample estimates:
    ## mean of x mean of y 
    ##  21.44178  47.23043

The number of ASIAN / PACIFIC ISLANDER race of criminals are less than
white criminals. …if you need more can be shown about race…

## 5.compare crime events number by covid statu

``` r
crimebyday_covid0=tidydata1 %>% 
  group_by(year,mon,day) %>% 
  dplyr::summarize(
    n_bydate=n(),
    covid_state=covid_state
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon', 'day'. You can override
    ## using the `.groups` argument.

``` r
casenum_byday_0=ifelse(crimebyday_covid0$covid_state==0,crimebyday_covid0$n_bydate,NA) %>% 
  na.omit()
casenum_byday_1=ifelse(crimebyday_covid0$covid_state==0,NA,crimebyday_covid0$n_bydate) %>% 
  na.omit()
t.test(casenum_byday_0,casenum_byday_1)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  casenum_byday_0 and casenum_byday_1
    ## t = 10.966, df = 1883.3, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  16.62263 23.86338
    ## sample estimates:
    ## mean of x mean of y 
    ##  370.0438  349.8008

The diference is significant. Among the overall status of covid we can
see there is a downtrend before covid and after covid.

## 6.compare crime events number by time range

``` r
crimebyday_time0=tidydata3 %>%
  filter(time_range==0) %>% 
  group_by(year,mon,day) %>% 
  dplyr::summarize(
    n_bydate=n()
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
crimebyday_time1=tidydata3 %>%
  filter(time_range==1) %>% 
  group_by(year,mon,day) %>% 
  dplyr::summarize(
    n_bydate=n()
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
crimebyday_time2=tidydata3 %>%
  filter(time_range==2) %>% 
  group_by(year,mon,day) %>% 
  dplyr::summarize(
    n_bydate=n()
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
t.test(crimebyday_time0$n_bydate,crimebyday_time1$n_bydate)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  crimebyday_time0$n_bydate and crimebyday_time1$n_bydate
    ## t = -64.668, df = 4581.6, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -41.21182 -38.78656
    ## sample estimates:
    ## mean of x mean of y 
    ##  82.56511 122.56430

``` r
t.test(crimebyday_time0$n_bydate,crimebyday_time2$n_bydate)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  crimebyday_time0$n_bydate and crimebyday_time2$n_bydate
    ## t = -114.17, df = 4808.1, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -75.37668 -72.83184
    ## sample estimates:
    ## mean of x mean of y 
    ##  82.56511 156.66937

``` r
t.test(crimebyday_time1$n_bydate,crimebyday_time2$n_bydate)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  crimebyday_time1$n_bydate and crimebyday_time2$n_bydate
    ## t = -60.664, df = 4855.3, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -35.20723 -33.00291
    ## sample estimates:
    ## mean of x mean of y 
    ##  122.5643  156.6694

From the test, we can see a significant different number of criminal
events occur at different time range:22-6 6-14 14-22 during a day. And
the relation can be conclude as: 22-6 \< 6-14 \<14-22

# dangers define as the FELONY events happen by day

## about races

``` r
crimebyday_aspa=tidydata1 %>% 
  filter(level=="FELONY") %>%
  group_by(year,mon,day) %>% 
  filter(susp_race=="ASIAN / PACIFIC ISLANDER") %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
crimebyday_white=tidydata1 %>% 
  filter(level=="FELONY") %>%
  group_by(year,mon,day) %>% 
  filter(susp_race=="WHITE") %>% 
  dplyr::summarize(
    n_bydate=n(),
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon'. You can override using the
    ## `.groups` argument.

``` r
t.test(crimebyday_aspa$n_bydate,crimebyday_white$n_bydate)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  crimebyday_aspa$n_bydate and crimebyday_white$n_bydate
    ## t = -54.195, df = 4528.4, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.931695 -4.587345
    ## sample estimates:
    ## mean of x mean of y 
    ##  5.598695 10.358215

races is follow the same as normal event assumption

## about covid state

``` r
compare_date_data3=tidydata3 %>% 
  filter(level=="FELONY") %>% 
  group_by(year,mon,day) %>% 
  dplyr::summarize(
    n_bydate=n(),
    covid_state=covid_state
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon', 'day'. You can override
    ## using the `.groups` argument.

``` r
casenum_byday_0=ifelse(compare_date_data3$covid_state==0,compare_date_data3$n_bydate,NA) %>% 
  na.omit()
casenum_byday_1=ifelse(compare_date_data3$covid_state==0,NA,compare_date_data3$n_bydate) %>% 
  na.omit()
t.test(casenum_byday_0,casenum_byday_1)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  casenum_byday_0 and casenum_byday_1
    ## t = -3.2454, df = 1827.7, p-value = 0.001194
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -3.6015021 -0.8882405
    ## sample estimates:
    ## mean of x mean of y 
    ##  96.51608  98.76096

significant difference we can see after covid, the probability of happen
FELONY is greater than the time before covid.

## about time range

``` r
compare_date_data3=tidydata3 %>% 
  filter(level=="FELONY") %>% 
  group_by(year,mon,day,time_range) %>% 
  dplyr::summarize(
    n_bydate=n(),
    covid_state=covid_state
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year', 'mon', 'day', 'time_range'. You can
    ## override using the `.groups` argument.

``` r
casenum_byday_0=ifelse(compare_date_data3$time_range==0,compare_date_data3$n_bydate,NA) %>% 
  na.omit()
casenum_byday_1=ifelse(compare_date_data3$time_range==1,compare_date_data3$n_bydate,NA) %>% 
  na.omit()
t.test(casenum_byday_0,casenum_byday_1)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  casenum_byday_0 and casenum_byday_1
    ## t = -10.718, df = 4389.7, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.928759 -2.022965
    ## sample estimates:
    ## mean of x mean of y 
    ##  26.82596  29.30183

``` r
casenum_byday_0=ifelse(compare_date_data3$time_range==0,compare_date_data3$n_bydate,NA) %>% 
  na.omit()
casenum_byday_1=ifelse(compare_date_data3$time_range==2,compare_date_data3$n_bydate,NA) %>% 
  na.omit()
t.test(casenum_byday_0,casenum_byday_1)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  casenum_byday_0 and casenum_byday_1
    ## t = -58.182, df = 4799.2, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -14.96447 -13.98888
    ## sample estimates:
    ## mean of x mean of y 
    ##  26.82596  41.30264

``` r
casenum_byday_0=ifelse(compare_date_data3$time_range==1,compare_date_data3$n_bydate,NA) %>% 
  na.omit()
casenum_byday_1=ifelse(compare_date_data3$time_range==2,compare_date_data3$n_bydate,NA) %>% 
  na.omit()
t.test(casenum_byday_0,casenum_byday_1)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  casenum_byday_0 and casenum_byday_1
    ## t = -57.724, df = 4742.7, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -12.40839 -11.59323
    ## sample estimates:
    ## mean of x mean of y 
    ##  29.30183  41.30264

It’s the same as all event circumstances.

## Let’s compare the time range as different seasons and use paired t_test

``` r
tidydata3 = tidydata3 %>% 
  mutate(
    mon=as.numeric(mon)
  )

compare_date_data4=tidydata3 %>% 
  filter(level=="FELONY") %>%
  filter(mon>5 & mon<9) %>% 
  group_by(year) %>% 
  dplyr::summarize(
    n_bydate=n(),
    covid_state=covid_state
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
compare_date_data5=tidydata3 %>% 
  filter(level=="FELONY") %>%
  filter(mon>11 | mon<3) %>% 
  group_by(year) %>% 
  dplyr::summarize(
    n_bydate=n(),
    covid_state=covid_state
  ) %>% 
  unique()
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
t.test(compare_date_data4$n_bydate,compare_date_data5$n_bydate,paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  compare_date_data4$n_bydate and compare_date_data5$n_bydate
    ## t = 2.8493, df = 6, p-value = 0.02921
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##   279.2582 3675.5989
    ## sample estimates:
    ## mean difference 
    ##        1977.429

Here is a significant difference b/w winter and summer.
