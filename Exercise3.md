Exercise 3
================

``` r
library(tidygraph)
```

    ## Warning: package 'tidygraph' was built under R version 4.0.5

    ## 
    ## Attaching package: 'tidygraph'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(scales)
```

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.0.5

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1
    ## v purrr   0.3.4

    ## Warning: package 'tibble' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.5

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x readr::col_factor() masks scales::col_factor()
    ## x purrr::discard()    masks scales::discard()
    ## x dplyr::filter()     masks tidygraph::filter(), stats::filter()
    ## x dplyr::lag()        masks stats::lag()

``` r
library(ggraph)
```

    ## Warning: package 'ggraph' was built under R version 4.0.5

``` r
library(igraph)
```

    ## Warning: package 'igraph' was built under R version 4.0.5

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following object is masked from 'package:tidygraph':
    ## 
    ##     groups

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(arrow)
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
library(dplyr)
```

``` r
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 4.0.5

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
Apps <- read_parquet('/Users/Brock/Downloads/app_data_sample.parquet')
Edges <- read.csv('/Users/Brock/Downloads/edges_sample.csv')
```

## Gender

``` r
exnames <- Apps %>% distinct(examiner_name_first)

exnames
```

    ## # A tibble: 2,595 x 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # ... with 2,585 more rows

``` r
#install_genderdata_package() 
library(gender)
```

    ## Warning: package 'gender' was built under R version 4.0.5

``` r
exnames_gender <- exnames %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
```

``` r
Apps <- Apps %>% 
  left_join(exnames_gender, by = "examiner_name_first")
```

## Race

``` r
library(wru)
```

    ## Warning: package 'wru' was built under R version 4.0.5

``` r
ex_surnames <- Apps %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
```

``` r
ex_race <- predict_race(voter.file = ex_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
ex_race <- ex_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))
```

``` r
Apps <- Apps %>% 
  left_join(ex_race, by = c("examiner_name_last" = "surname"))
```

## Tenure

``` r
library(lubridate) 
```

    ## Warning: package 'lubridate' was built under R version 4.0.5

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration

    ## The following objects are masked from 'package:igraph':
    ## 
    ##     %--%, union

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
ex_dates <- Apps %>% 
  select(examiner_id, filing_date, appl_status_date) 
```

``` r
ex_dates <- ex_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
```

``` r
ex_dates <- ex_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    ) %>% 
  filter(year(latest_date)<2018)
```

``` r
Apps <- Apps %>% 
  left_join(ex_dates, by = "examiner_id")
```

## Workgroup Analysis

``` r
Apps <- Apps %>%
  mutate(wg = (Apps$examiner_art_unit%/%10) * 10)
```

## Work groups 1640 and 1740

``` r
Apps1640 = Apps[Apps$wg==1640,]
Apps1740 = Apps[Apps$wg==1740,]
```

## Summary Statistics of Workgroups

``` r
summary(Apps1640)
```

    ##  application_number  filing_date         examiner_name_last examiner_name_first
    ##  Length:93342       Min.   :2000-01-03   Length:93342       Length:93342       
    ##  Class :character   1st Qu.:2004-02-06   Class :character   Class :character   
    ##  Mode  :character   Median :2008-04-16   Mode  :character   Mode  :character   
    ##                     Mean   :2008-05-14                                         
    ##                     3rd Qu.:2012-06-08                                         
    ##                     Max.   :2017-05-26                                         
    ##                                                                                
    ##  examiner_name_middle  examiner_id    examiner_art_unit  uspc_class       
    ##  Length:93342         Min.   :59211   Min.   :1641      Length:93342      
    ##  Class :character     1st Qu.:67775   1st Qu.:1643      Class :character  
    ##  Mode  :character     Median :76749   Median :1645      Mode  :character  
    ##                       Mean   :80061   Mean   :1645                        
    ##                       3rd Qu.:94046   3rd Qu.:1647                        
    ##                       Max.   :99985   Max.   :1649                        
    ##                       NA's   :811                                         
    ##  uspc_subclass      patent_number      patent_issue_date   
    ##  Length:93342       Length:93342       Min.   :2000-09-12  
    ##  Class :character   Class :character   1st Qu.:2008-05-13  
    ##  Mode  :character   Mode  :character   Median :2011-08-16  
    ##                                        Mean   :2011-03-27  
    ##                                        3rd Qu.:2014-06-03  
    ##                                        Max.   :2017-06-20  
    ##                                        NA's   :53514       
    ##   abandon_date        disposal_type      appl_status_code appl_status_date  
    ##  Min.   :2000-03-07   Length:93342       Min.   : 16.0    Length:93342      
    ##  1st Qu.:2006-11-07   Class :character   1st Qu.:150.0    Class :character  
    ##  Median :2009-09-25   Mode  :character   Median :161.0    Mode  :character  
    ##  Mean   :2009-12-05                      Mean   :150.6                      
    ##  3rd Qu.:2013-02-04                      3rd Qu.:161.0                      
    ##  Max.   :2017-06-05                      Max.   :454.0                      
    ##  NA's   :51937                           NA's   :155                        
    ##        tc          gender          proportion_female    pred.whi     
    ##  Min.   :1600   Length:93342       Min.   :0.000     Min.   :0.0000  
    ##  1st Qu.:1600   Class :character   1st Qu.:0.004     1st Qu.:0.2201  
    ##  Median :1600   Mode  :character   Median :0.748     Median :0.7527  
    ##  Mean   :1600                      Mean   :0.513     Mean   :0.6203  
    ##  3rd Qu.:1600                      3rd Qu.:0.997     3rd Qu.:0.9421  
    ##  Max.   :1600                      Max.   :1.000     Max.   :1.0000  
    ##                                    NA's   :13551                     
    ##     pred.bla          pred.his          pred.asi           pred.oth      
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.000000   Min.   :0.00000  
    ##  1st Qu.:0.00340   1st Qu.:0.01170   1st Qu.:0.005000   1st Qu.:0.01520  
    ##  Median :0.01400   Median :0.02170   Median :0.007875   Median :0.02120  
    ##  Mean   :0.08528   Mean   :0.04648   Mean   :0.222357   Mean   :0.02557  
    ##  3rd Qu.:0.08530   3rd Qu.:0.02650   3rd Qu.:0.079700   3rd Qu.:0.03180  
    ##  Max.   :0.96520   Max.   :0.93240   Max.   :0.981900   Max.   :0.13750  
    ##                                                                          
    ##    max_race_p         race           earliest_date         latest_date        
    ##  Min.   :0.3190   Length:93342       Min.   :2000-01-03   Min.   :2001-09-14  
    ##  1st Qu.:0.7307   Class :character   1st Qu.:2000-01-10   1st Qu.:2017-05-19  
    ##  Median :0.9071   Mode  :character   Median :2000-01-26   Median :2017-05-20  
    ##  Mean   :0.8435                      Mean   :2000-07-26   Mean   :2017-05-06  
    ##  3rd Qu.:0.9565                      3rd Qu.:2000-09-22   3rd Qu.:2017-05-22  
    ##  Max.   :1.0000                      Max.   :2012-04-03   Max.   :2017-05-23  
    ##                                      NA's   :1884         NA's   :1884        
    ##   tenure_days         wg      
    ##  Min.   : 314   Min.   :1640  
    ##  1st Qu.:6074   1st Qu.:1640  
    ##  Median :6315   Median :1640  
    ##  Mean   :6128   Mean   :1640  
    ##  3rd Qu.:6338   3rd Qu.:1640  
    ##  Max.   :6350   Max.   :1640  
    ##  NA's   :1884

``` r
summary(Apps1740) 
```

    ##  application_number  filing_date         examiner_name_last examiner_name_first
    ##  Length:75598       Min.   :2000-01-03   Length:75598       Length:75598       
    ##  Class :character   1st Qu.:2002-11-22   Class :character   Class :character   
    ##  Mode  :character   Median :2009-05-27   Mode  :character   Mode  :character   
    ##                     Mean   :2008-07-11                                         
    ##                     3rd Qu.:2013-05-14                                         
    ##                     Max.   :2017-04-28                                         
    ##                                                                                
    ##  examiner_name_middle  examiner_id    examiner_art_unit  uspc_class       
    ##  Length:75598         Min.   :59356   Min.   :1741      Length:75598      
    ##  Class :character     1st Qu.:65275   1st Qu.:1742      Class :character  
    ##  Mode  :character     Median :75238   Median :1744      Mode  :character  
    ##                       Mean   :77759   Mean   :1744                        
    ##                       3rd Qu.:92507   3rd Qu.:1745                        
    ##                       Max.   :99850   Max.   :1747                        
    ##                       NA's   :519                                         
    ##  uspc_subclass      patent_number      patent_issue_date   
    ##  Length:75598       Length:75598       Min.   :2000-08-08  
    ##  Class :character   Class :character   1st Qu.:2004-09-21  
    ##  Mode  :character   Mode  :character   Median :2011-05-24  
    ##                                        Mean   :2009-10-17  
    ##                                        3rd Qu.:2014-07-29  
    ##                                        Max.   :2017-06-20  
    ##                                        NA's   :33153       
    ##   abandon_date        disposal_type      appl_status_code appl_status_date  
    ##  Min.   :2000-06-26   Length:75598       Min.   : 17.0    Length:75598      
    ##  1st Qu.:2006-03-29   Class :character   1st Qu.:150.0    Class :character  
    ##  Median :2011-08-09   Mode  :character   Median :150.0    Mode  :character  
    ##  Mean   :2010-06-27                      Mean   :148.8                      
    ##  3rd Qu.:2014-02-27                      3rd Qu.:161.0                      
    ##  Max.   :2017-06-04                      Max.   :454.0                      
    ##  NA's   :55531                           NA's   :159                        
    ##        tc          gender          proportion_female    pred.whi     
    ##  Min.   :1700   Length:75598       Min.   :0.000     Min.   :0.0074  
    ##  1st Qu.:1700   Class :character   1st Qu.:0.004     1st Qu.:0.5897  
    ##  Median :1700   Mode  :character   Median :0.005     Median :0.6994  
    ##  Mean   :1700                      Mean   :0.291     Mean   :0.6543  
    ##  3rd Qu.:1700                      3rd Qu.:0.990     3rd Qu.:0.9202  
    ##  Max.   :1700                      Max.   :1.000     Max.   :0.9959  
    ##                                    NA's   :5015                      
    ##     pred.bla          pred.his          pred.asi           pred.oth      
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :0.000000   Min.   :0.00000  
    ##  1st Qu.:0.00440   1st Qu.:0.01870   1st Qu.:0.005399   1st Qu.:0.01630  
    ##  Median :0.03601   Median :0.02460   Median :0.007400   Median :0.02630  
    ##  Mean   :0.10147   Mean   :0.06382   Mean   :0.153350   Mean   :0.02702  
    ##  3rd Qu.:0.16330   3rd Qu.:0.04230   3rd Qu.:0.079700   3rd Qu.:0.03180  
    ##  Max.   :0.92790   Max.   :0.93349   Max.   :0.982300   Max.   :0.14120  
    ##                                                                          
    ##    max_race_p         race           earliest_date         latest_date        
    ##  Min.   :0.3554   Length:75598       Min.   :2000-01-03   Min.   :2001-09-23  
    ##  1st Qu.:0.6665   Class :character   1st Qu.:2000-01-06   1st Qu.:2017-05-17  
    ##  Median :0.8430   Mode  :character   Median :2000-01-19   Median :2017-05-19  
    ##  Mean   :0.7996                      Mean   :2001-11-16   Mean   :2017-05-01  
    ##  3rd Qu.:0.9434                      3rd Qu.:2004-01-09   3rd Qu.:2017-05-22  
    ##  Max.   :0.9959                      Max.   :2015-01-16   Max.   :2017-05-23  
    ##                                      NA's   :1399         NA's   :1399        
    ##   tenure_days         wg      
    ##  Min.   : 464   Min.   :1740  
    ##  1st Qu.:4879   1st Qu.:1740  
    ##  Median :6296   Median :1740  
    ##  Mean   :5645   Mean   :1740  
    ##  3rd Qu.:6342   3rd Qu.:1740  
    ##  Max.   :6350   Max.   :1740  
    ##  NA's   :1399

## Gender Plots

``` r
plot1640gender <- ggplot(Apps1640, aes(gender)) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Frequencies") +
          ylim(0,0.65) +
          ggtitle("Gender distribution for 1640")
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
plot1640gender
```

![](Exercise3_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
plot1740gender <- ggplot(Apps1740, aes(gender)) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Frequencies") +
          ylim(0,0.65) +
          ggtitle("Gender distribution for 1740")
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
plot1740gender
```

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](Exercise3_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

## Race Plots

``` r
plot1640race <- ggplot(Apps1640, aes(race)) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Frequencies") +
          ylim(0,0.6) +
          ggtitle("Race distribution for 1640")
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
plot1640race
```

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](Exercise3_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
plot1740race <- ggplot(Apps1740, aes(race)) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Frequencies") +
          ylim(0,0.6) +
          ggtitle("Race distribution for 1740")
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
plot1740race
```

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](Exercise3_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

## Tenure Plots

``` r
plot1640tenure <- ggplot(Apps1640, aes(round(tenure_days/365,digits=0))) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Frequencies") +
          xlab("Tenure (years)") +
          ylim(0,0.5) +
          ggtitle("Tenure distribution for 1640")
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
plot1640tenure
```

    ## Warning: Removed 1884 rows containing non-finite values (stat_count).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](Exercise3_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
plot1740tenure <- ggplot(Apps1740, aes(round(tenure_days/365,digits=0))) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Frequencies") +
          xlab("Tenure (years)") +
          ylim(0,0.5) +
          ggtitle("Tenure distribution for 1740") 
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
plot1740tenure
```

    ## Warning: Removed 1399 rows containing non-finite values (stat_count).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](Exercise3_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

## Advice Networks & Centrality Scores

``` r
ex_analysis <- Apps %>%
    #filter(period == "t1") %>% 
    arrange(desc(filing_date)) %>%
    filter(wg == 1640| wg == 1740 ) %>%
    select(wg, examiner_art_unit, examiner_id) %>%
    distinct(examiner_id, .keep_all=TRUE) %>% 
    drop_na() 
```

``` r
edges_analysis <- Edges %>%
  filter(ego_examiner_id %in% ex_analysis$examiner_id) %>%
  filter(alter_examiner_id %in% ex_analysis$examiner_id) %>%
  drop_na() 
```

``` r
networkanalysis <- left_join(edges_analysis, ex_analysis, by = c("ego_examiner_id" = "examiner_id"))
colnames(networkanalysis)[5] <- "ego_examiner_wg"
colnames(networkanalysis)[6] <- "ego_examiner_au"
networkanalysis <- left_join(networkanalysis, ex_analysis, by = c("alter_examiner_id" = "examiner_id"))
colnames(networkanalysis)[7] <- "alter_examiner_wg"
colnames(networkanalysis)[8] <- "alter_examiner_au"
```

``` r
edges <- select(networkanalysis, c("ego_examiner_id","alter_examiner_id"))
```

``` r
uspto <- select(networkanalysis, c("ego_examiner_id","ego_examiner_wg","ego_examiner_au")) %>%
    dplyr::rename(id=ego_examiner_id, wg=ego_examiner_wg, au=ego_examiner_au)
alter <- select(networkanalysis, c("alter_examiner_id","alter_examiner_wg","alter_examiner_au")) %>%
  dplyr::rename(id=alter_examiner_id, wg=alter_examiner_wg, au=alter_examiner_au)
nodes <- rbind(uspto, alter) %>%
  distinct() %>%
  drop_na()

nodes
```

    ##        id   wg   au
    ## 1   97910 1640 1641
    ## 2   70204 1640 1642
    ## 3   59338 1640 1648
    ## 4   61757 1640 1642
    ## 5   96963 1640 1644
    ## 6   93839 1640 1644
    ## 7   74224 1640 1647
    ## 8   87897 1640 1643
    ## 9   68665 1640 1642
    ## 10  94046 1640 1644
    ## 11  64659 1640 1648
    ## 12  75380 1640 1648
    ## 13  81117 1640 1644
    ## 14  96143 1640 1643
    ## 15  84896 1640 1642
    ## 16  75730 1640 1646
    ## 17  71119 1740 1746
    ## 18  92784 1640 1646
    ## 19  85865 1640 1646
    ## 20  61276 1640 1641
    ## 21  77971 1640 1641
    ## 22  93421 1640 1642
    ## 23  68922 1640 1646
    ## 24  94658 1740 1745
    ## 25  93869 1640 1645
    ## 26  59693 1640 1646
    ## 27  67901 1740 1741
    ## 28  71259 1640 1646
    ## 29  64064 1640 1641
    ## 30  59771 1740 1742
    ## 31  59211 1640 1644
    ## 32  63511 1640 1644
    ## 33  62397 1640 1641
    ## 34  90331 1640 1641
    ## 35  71414 1640 1648
    ## 36  92572 1640 1644
    ## 37  61558 1640 1645
    ## 38  97884 1740 1745
    ## 39  66336 1640 1645
    ## 40  63394 1640 1644
    ## 41  97818 1740 1745
    ## 42  65654 1640 1641
    ## 43  98563 1640 1649
    ## 44  59497 1640 1645
    ## 45  99465 1740 1742
    ## 46  92143 1740 1744
    ## 47  71107 1740 1744
    ## 48  76021 1740 1744
    ## 49  81877 1740 1742
    ## 50  64403 1740 1743
    ## 51  63842 1740 1745
    ## 52  63577 1740 1743
    ## 53  94956 1740 1745
    ## 54  75238 1740 1743
    ## 55  94161 1740 1744
    ## 56  79495 1640 1643
    ## 57  68788 1740 1742
    ## 58  64169 1640 1643
    ## 59  68463 1740 1741
    ## 60  61126 1740 1747
    ## 61  74342 1740 1743
    ## 62  68619 1740 1746
    ## 63  82111 1740 1745
    ## 64  95604 1640 1641
    ## 65  76959 1740 1741
    ## 66  77369 1740 1742
    ## 67  96027 1640 1643
    ## 68  97657 1740 1744
    ## 69  64445 1640 1647
    ## 70  84867 1740 1742
    ## 71  69378 1740 1745
    ## 72  75933 1740 1745
    ## 73  97772 1640 1644
    ## 74  98114 1740 1741
    ## 75  98826 1740 1745
    ## 76  64992 1740 1744
    ## 77  98852 1740 1742
    ## 78  86683 1740 1746
    ## 79  68445 1640 1642
    ## 80  86115 1640 1648
    ## 81  84289 1740 1745
    ## 82  61534 1740 1746
    ## 83  91352 1740 1743
    ## 84  75367 1740 1744
    ## 85  94211 1740 1746
    ## 86  82047 1640 1641
    ## 87  77749 1740 1741
    ## 88  76154 1740 1745
    ## 89  98014 1740 1747
    ## 90  67050 1740 1743
    ## 91  97543 1640 1646
    ## 92  63470 1640 1649
    ## 93  62506 1740 1744
    ## 94  71760 1740 1744
    ## 95  75409 1740 1742
    ## 96  76583 1740 1744
    ## 97  64506 1640 1648
    ## 98  99106 1740 1743
    ## 99  75954 1740 1743
    ## 100 75406 1740 1746
    ## 101 63384 1740 1745
    ## 102 94251 1740 1741
    ## 103 76749 1640 1646
    ## 104 60651 1740 1741
    ## 105 75243 1640 1642
    ## 106 90863 1640 1644
    ## 107 89192 1740 1745
    ## 108 63176 1740 1743
    ## 109 68970 1740 1742
    ## 110 61517 1740 1744
    ## 111 91159 1740 1744
    ## 112 98470 1640 1647
    ## 113 73788 1640 1648
    ## 114 95634 1640 1641
    ## 115 90219 1640 1644
    ## 116 59738 1640 1641
    ## 117 72882 1640 1643
    ## 118 65024 1640 1645
    ## 119 71946 1640 1649
    ## 120 67657 1640 1645
    ## 121 69583 1640 1648
    ## 122 85987 1640 1641
    ## 123 66266 1640 1647
    ## 124 85381 1640 1646
    ## 125 72903 1640 1643
    ## 126 81987 1640 1647
    ## 127 83398 1740 1746
    ## 128 83254 1640 1647
    ## 129 91016 1640 1646
    ## 130 72820 1640 1649
    ## 131 94285 1640 1648
    ## 132 77184 1740 1745
    ## 133 83794 1640 1648
    ## 134 73689 1740 1747
    ## 135 73074 1740 1747
    ## 136 69917 1640 1641
    ## 137 65446 1640 1643
    ## 138 63963 1640 1647
    ## 139 70858 1640 1641
    ## 140 63226 1640 1642
    ## 141 77915 1740 1745
    ## 142 60958 1640 1645
    ## 143 59475 1740 1742
    ## 144 77298 1740 1744
    ## 145 94084 1640 1645
    ## 146 99217 1740 1747
    ## 147 64074 1740 1744
    ## 148 96556 1740 1742
    ## 149 93909 1740 1745
    ## 150 78952 1640 1647
    ## 151 70610 1740 1746
    ## 152 72771 1740 1743
    ## 153 66582 1740 1746
    ## 154 65919 1740 1747
    ## 155 74052 1740 1745
    ## 156 96865 1640 1643
    ## 157 87125 1740 1745
    ## 158 89403 1740 1743
    ## 159 64548 1740 1741
    ## 160 63585 1740 1741
    ## 161 66442 1740 1743
    ## 162 61283 1740 1743
    ## 163 98489 1740 1745
    ## 164 93553 1740 1744
    ## 165 68227 1740 1744
    ## 166 69193 1740 1742
    ## 167 69209 1740 1746
    ## 168 95160 1740 1743
    ## 169 85761 1640 1645
    ## 170 63065 1740 1747
    ## 171 71557 1740 1742
    ## 172 60077 1740 1742
    ## 173 95660 1740 1742
    ## 174 96568 1740 1742
    ## 175 61105 1640 1646
    ## 176 72355 1640 1649
    ## 177 63326 1740 1741
    ## 178 64915 1740 1747
    ## 179 67259 1740 1743
    ## 180 98700 1640 1648
    ## 181 64507 1640 1644
    ## 182 88189 1740 1744
    ## 183 99340 1640 1646
    ## 184 64331 1640 1641
    ## 185 73274 1740 1747
    ## 186 60879 1740 1746
    ## 187 97741 1740 1741
    ## 188 64004 1640 1644
    ## 189 81831 1740 1741
    ## 190 67620 1640 1642
    ## 191 99892 1640 1643

``` r
library(igraph) 
```

``` r
Net = graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
Net
```

    ## IGRAPH da4e198 DN-- 191 942 -- 
    ## + attr: name (v/c), wg (v/n), au (v/n)
    ## + edges from da4e198 (vertex names):
    ##  [1] 97910->59738 70204->72882 59338->72882 61757->65024 61757->72882
    ##  [6] 96963->72882 97910->59738 93839->71946 74224->65024 96963->67657
    ## [11] 87897->69583 87897->72882 93839->71946 97910->85987 97910->59738
    ## [16] 70204->72882 68665->72882 97910->59738 74224->65024 74224->74224
    ## [21] 94046->72882 94046->59211 93839->66266 64659->72882 64659->59738
    ## [26] 75380->69583 75380->72882 96963->67657 74224->85381 74224->71946
    ## [31] 74224->74224 64659->72882 64659->59738 81117->90863 96143->72903
    ## [36] 96143->72882 70204->72882 96963->66266 84896->72882 75730->81987
    ## + ... omitted several edges

## Centrality Measures

## Degree Centrality

``` r
V(Net)$dc <- degree(Net)
```

## Betweeness

``` r
V(Net)$bc <- betweenness(Net)
```

#Eigenvector

``` r
V(Net)$ec <- evcent(Net)
```

    ## Warning in vattrs[[name]][index] <- value: number of items to replace is not a
    ## multiple of replacement length

``` r
data.frame(round(cbind(V(Net)$dc, V(Net)$bc, unlist(V(Net)$ec[1])),3)) 
```

    ##        X1   X2    X3
    ## 97910 116  0.0 1.000
    ## 70204  32  0.0 0.302
    ## 59338  13  0.0 0.033
    ## 61757  13  3.0 0.071
    ## 96963  11  8.0 0.040
    ## 93839  21  2.0 0.022
    ## 74224  42  0.0 0.011
    ## 87897  53  0.0 0.293
    ## 68665   8  0.0 0.043
    ## 94046  16  0.0 0.017
    ## 64659  20  0.0 0.263
    ## 75380   2  0.0 0.013
    ## 81117   9  0.0 0.004
    ## 96143  11  0.0 0.075
    ## 84896   2  0.0 0.013
    ## 75730   6  0.0 0.003
    ## 71119   7 40.0 0.000
    ## 92784  22  0.0 0.007
    ## 85865  22  0.0 0.007
    ## 61276   9  1.0 0.084
    ## 77971   1  0.0 0.000
    ## 93421  13  0.0 0.082
    ## 68922   4  0.0 0.006
    ## 94658   2  0.0 0.000
    ## 93869   4  0.0 0.023
    ## 59693   8  0.0 0.003
    ## 67901   3  0.0 0.000
    ## 71259  15  0.0 0.018
    ## 64064   2  0.0 0.029
    ## 59771  35 22.0 0.000
    ## 59211  23  3.0 0.020
    ## 63511   9  0.0 0.012
    ## 62397  19  0.0 0.193
    ## 90331   5  0.0 0.053
    ## 71414  10  0.0 0.025
    ## 92572   2  1.0 0.020
    ## 61558   1  0.0 0.011
    ## 97884   1  0.0 0.000
    ## 66336   1  0.0 0.000
    ## 63394   3  0.0 0.001
    ## 97818   5  0.0 0.000
    ## 65654   4  2.0 0.002
    ## 98563  19  0.0 0.016
    ## 59497   5  0.0 0.001
    ## 99465   6  0.0 0.000
    ## 92143  31 30.0 0.000
    ## 71107   5  0.0 0.000
    ## 76021   3  0.0 0.000
    ## 81877  12  0.0 0.000
    ## 64403  22 12.0 0.000
    ## 63842   1  0.0 0.000
    ## 63577   5  0.0 0.000
    ## 94956   8  9.0 0.000
    ## 75238   6  0.0 0.000
    ## 94161  26  6.0 0.000
    ## 79495   5  0.0 0.032
    ## 68788   6  0.0 0.000
    ## 64169   4  0.0 0.045
    ## 68463  11  0.0 0.000
    ## 61126   4  0.0 0.000
    ## 74342  19  0.0 0.000
    ## 68619  42  0.0 0.000
    ## 82111   1  0.0 0.000
    ## 95604   9  1.6 0.041
    ## 76959   2  1.0 0.000
    ## 77369   3  9.0 0.000
    ## 96027  10  0.0 0.057
    ## 97657  13  0.0 0.000
    ## 64445   1  0.0 0.002
    ## 84867   1  0.0 0.000
    ## 69378   4 26.0 0.000
    ## 75933   3  0.0 0.000
    ## 97772   4  0.0 0.026
    ## 98114   3  2.0 0.000
    ## 98826   6 58.0 0.000
    ## 64992   2  0.0 0.000
    ## 98852  17 98.0 0.000
    ## 86683   3  0.0 0.000
    ## 68445   1  0.0 0.011
    ## 86115   4  0.0 0.013
    ## 84289   1  0.0 0.000
    ## 61534   2  0.0 0.000
    ## 91352   1  0.0 0.000
    ## 75367   1  0.0 0.000
    ## 94211   1  0.0 0.000
    ## 82047  10  0.4 0.055
    ## 77749   2  0.0 0.000
    ## 76154   9  0.0 0.000
    ## 98014   2  0.0 0.000
    ## 67050  22  0.0 0.000
    ## 97543   7  0.0 0.021
    ## 63470   1  0.0 0.001
    ## 62506   3  0.0 0.000
    ## 71760   1  0.0 0.000
    ## 75409   2  0.0 0.000
    ## 76583  12  0.0 0.000
    ## 64506   2  0.0 0.013
    ## 99106   1  0.0 0.000
    ## 75954   7  0.0 0.000
    ## 75406  31  0.0 0.000
    ## 63384   2  0.0 0.000
    ## 94251   1  0.0 0.000
    ## 76749  10  0.0 0.000
    ## 60651  22  0.0 0.000
    ## 75243   1  0.0 0.000
    ## 90863  11  1.0 0.023
    ## 89192   1  0.0 0.000
    ## 63176  65 42.0 0.000
    ## 68970   1  0.0 0.000
    ## 61517   7  0.0 0.001
    ## 91159   1  0.0 0.000
    ## 98470   2  0.0 0.013
    ## 73788   2  0.0 0.003
    ## 95634   2  0.0 0.000
    ## 90219   1  0.0 0.011
    ## 59738  69  0.0 0.841
    ## 72882 133  0.0 0.651
    ## 65024  62  0.0 0.036
    ## 71946  27  0.0 0.077
    ## 67657   2  0.0 0.001
    ## 69583  24  0.0 0.073
    ## 85987  16  0.0 0.278
    ## 66266  51  0.0 0.107
    ## 85381  24  0.0 0.003
    ## 72903   3  0.0 0.004
    ## 81987   3  0.0 0.000
    ## 83398   1  0.0 0.000
    ## 83254  25  0.0 0.005
    ## 91016   2  0.0 0.001
    ## 72820   6  0.0 0.002
    ## 94285   1  0.0 0.017
    ## 77184   3  0.0 0.000
    ## 83794  10  0.0 0.024
    ## 73689   3  0.0 0.000
    ## 73074   2  0.0 0.000
    ## 69917  28  0.0 0.209
    ## 65446  24  0.0 0.117
    ## 63963   1  0.0 0.001
    ## 70858   3  0.0 0.000
    ## 63226   1  0.0 0.001
    ## 77915   4  0.0 0.000
    ## 60958   1  0.0 0.000
    ## 59475  12  0.0 0.000
    ## 77298  44  0.0 0.000
    ## 94084   2  0.0 0.000
    ## 99217   2  0.0 0.000
    ## 64074   3  0.0 0.000
    ## 96556  12  0.0 0.000
    ## 93909   5  0.0 0.000
    ## 78952   1  0.0 0.000
    ## 70610   1  0.0 0.000
    ## 72771   1  0.0 0.000
    ## 66582   3  0.0 0.000
    ## 65919   6  0.0 0.000
    ## 74052   1  0.0 0.000
    ## 96865   5  0.0 0.005
    ## 87125   2  0.0 0.000
    ## 89403  14  0.0 0.000
    ## 64548   3  0.0 0.000
    ## 63585   1  0.0 0.000
    ## 66442   2  0.0 0.000
    ## 61283   1  0.0 0.000
    ## 98489   1  0.0 0.000
    ## 93553   3  0.0 0.000
    ## 68227   2  0.0 0.000
    ## 69193   2  0.0 0.000
    ## 69209   3  0.0 0.000
    ## 95160   1  0.0 0.000
    ## 85761   2  0.0 0.000
    ## 63065   4  0.0 0.000
    ## 71557   3  0.0 0.000
    ## 60077   6  0.0 0.000
    ## 95660   1  0.0 0.000
    ## 96568   2  0.0 0.000
    ## 61105   1  0.0 0.017
    ## 72355   1  0.0 0.000
    ## 63326   3  0.0 0.000
    ## 64915   1  0.0 0.000
    ## 67259   3  0.0 0.000
    ## 98700   2  0.0 0.001
    ## 64507   3  0.0 0.035
    ## 88189   1  0.0 0.000
    ## 99340   2  0.0 0.035
    ## 64331   1  0.0 0.000
    ## 73274   1  0.0 0.000
    ## 60879   1  0.0 0.000
    ## 97741   1  0.0 0.000
    ## 64004   1  0.0 0.017
    ## 81831   1  0.0 0.000
    ## 67620   1  0.0 0.005
    ## 99892   1  0.0 0.005
