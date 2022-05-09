Exercise 1
================

## Install Packages

    install.packages("readr")
    install.packages("igraph")
    install.packages('tidygraph')
    install.packages('ggraph')
    library(tidyverse)
    library(igraph)
    library(dplyr)
    library(ggraph)
    library(tidygraph)

    data <- read_csv('/Users/Brock/Downloads/connections.csv')

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (6): First Name, Last Name, Email Address, Company, Position, Connected On
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    data %>% count()

    data %>% count(Company) %>% arrange(-n)

    data = data %>%  unite(name, c("First Name", "Last Name"))

    nodes = data %>% select(c("name", "Company"))
    nodes = nodes %>% rowid_to_column("id")

    nodes 

    edges = data %>% select(c(name, Company)) %>% left_join(nodes %>% select(c(id,name)), by = c("name"="name"))
    edges = edges %>% left_join(edges, by = "Company", keep=FALSE) %>% select(c("id.x", "id.y", "Company")) %>% filter(id.x!=id.y)

    colnames(edges) = c("x", "y", "Company")
    edges 
