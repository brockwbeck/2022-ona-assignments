Exercise 2
================

``` r
knitr::opts_chunk$set(echo = TRUE)
```

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

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
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v stringr 1.4.0
    ## v tidyr   1.2.0     v forcats 0.5.1
    ## v readr   2.1.2

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'tibble' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.5

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x tidygraph::filter() masks dplyr::filter(), stats::filter()
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

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
Data <- read.csv('/Users/Brock/Desktop/Exercise1.csv')
```

``` r
FGraph = graph_from_data_frame(d=Data,vertices=NULL ,directed=FALSE)
FGraph
```

    ## IGRAPH 1b817d1 UN-- 10 17 -- 
    ## + attr: name (v/c), edge (e/n)
    ## + edges from 1b817d1 (vertex names):
    ##  [1] 1--2 2--A A--B A--C B--C B--D B--6 B--3 D--6 D--3 D--C D--5 5--6 5--3 C--3
    ## [16] C--4 3--4

``` r
degree(FGraph, v=V(FGraph))
```

    ## 1 2 A B D 5 C 3 6 4 
    ## 1 2 3 5 5 3 5 5 3 2

``` r
betweenness(FGraph)
```

    ##          1          2          A          B          D          5          C 
    ##  0.0000000  8.0000000 14.0000000  9.0333333  3.2666667  0.5333333  8.6000000 
    ##          3          6          4 
    ##  4.6333333  0.9333333  0.0000000

``` r
evcent(FGraph)$vector
```

    ##          1          2          A          B          D          5          C 
    ## 0.03059284 0.12661070 0.49339477 0.97394849 1.00000000 0.62726236 0.94139110 
    ##          3          6          4 
    ## 0.96744261 0.62852844 0.46122992

``` r
ggraph(FGraph) +
  geom_edge_link() +
  geom_node_point()
```

    ## Using `stress` as default layout

![](Exercise2_files/figure-gfm/fGRAPH-1.png)<!-- -->

Using the above graph and the calculated measures, there are various
insights. Seats B, C and D have degrees of 5 in centrality. This would
provide the opportunity to connect with more people sitting nearby. In
betweeness, B scores the highest, with C trailing slightly. D scores
lower in this metric. D scores the highest in the eigenvector metric,
slightly ahead of B and C. Considering the results above, B appears to
be a promising option.
