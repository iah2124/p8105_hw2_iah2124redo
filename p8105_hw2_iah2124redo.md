p8105_hw2_iah2124redo
================
Iris Hart
2024-10-02

### Problem 0: Loading Packages

``` r
library(tidyverse)
library(readxl)
```

### Problem 1

``` r
trans_ent = 
  read_csv(
    "NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
    col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) |> 
  janitor::clean_names() |> 
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) |> 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

``` r
dim(trans_ent)
## [1] 1868   20
```

This data set contains the variables: line, station name, station
latitude, station longitude, routes, entry, exit only, vending, entrance
type, and ada compliance. Cleaned the data by reading the file and using
janitor to keep the variables needed, and change the names accordingly.
The dimensions of this data set are 1868 x 20. This data is not
considered “tidy”. This is because route number should be a variable, as
should route. That is, to obtain a tidy data set we would need to
convert `route` variables from wide to long format.

How many distinct stations are there?

``` r
trans_ent |> 
  select(station_name, line) |> 
  distinct()
## # A tibble: 465 × 2
##    station_name             line    
##    <chr>                    <chr>   
##  1 25th St                  4 Avenue
##  2 36th St                  4 Avenue
##  3 45th St                  4 Avenue
##  4 53rd St                  4 Avenue
##  5 59th St                  4 Avenue
##  6 77th St                  4 Avenue
##  7 86th St                  4 Avenue
##  8 95th St                  4 Avenue
##  9 9th St                   4 Avenue
## 10 Atlantic Av-Barclays Ctr 4 Avenue
## # ℹ 455 more rows
```

There are 465 distinct stations.

How many stations are ADA compliant?

``` r
trans_ent |> 
  filter(ada == TRUE) |> 
  select(station_name, line) |> 
  distinct()
## # A tibble: 84 × 2
##    station_name                   line           
##    <chr>                          <chr>          
##  1 Atlantic Av-Barclays Ctr       4 Avenue       
##  2 DeKalb Av                      4 Avenue       
##  3 Pacific St                     4 Avenue       
##  4 Grand Central                  42nd St Shuttle
##  5 34th St                        6 Avenue       
##  6 47-50th Sts Rockefeller Center 6 Avenue       
##  7 Church Av                      6 Avenue       
##  8 21st St                        63rd Street    
##  9 Lexington Av                   63rd Street    
## 10 Roosevelt Island               63rd Street    
## # ℹ 74 more rows
```

84 stations are ADA complicit.

What proportion of station entrances / exits without vending allow
entrance?

``` r
trans_ent |> 
  filter(vending == "NO") |> 
  pull(entry) |> 
  mean()
## [1] 0.3770492
```

37.71% of station enterances / exits without vending allow entrance.

``` r
trans_ent |> 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") |> 
  filter(route == "A") |> 
  select(station_name, line) |> 
  distinct()
## # A tibble: 60 × 2
##    station_name                  line           
##    <chr>                         <chr>          
##  1 Times Square                  42nd St Shuttle
##  2 125th St                      8 Avenue       
##  3 145th St                      8 Avenue       
##  4 14th St                       8 Avenue       
##  5 168th St - Washington Heights 8 Avenue       
##  6 175th St                      8 Avenue       
##  7 181st St                      8 Avenue       
##  8 190th St                      8 Avenue       
##  9 34th St                       8 Avenue       
## 10 42nd St                       8 Avenue       
## # ℹ 50 more rows

trans_ent |> 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") |> 
  filter(route == "A", ada == TRUE) |> 
  select(station_name, line) |> 
  distinct()
## # A tibble: 17 × 2
##    station_name                  line            
##    <chr>                         <chr>           
##  1 14th St                       8 Avenue        
##  2 168th St - Washington Heights 8 Avenue        
##  3 175th St                      8 Avenue        
##  4 34th St                       8 Avenue        
##  5 42nd St                       8 Avenue        
##  6 59th St                       8 Avenue        
##  7 Inwood - 207th St             8 Avenue        
##  8 West 4th St                   8 Avenue        
##  9 World Trade Center            8 Avenue        
## 10 Times Square-42nd St          Broadway        
## 11 59th St-Columbus Circle       Broadway-7th Ave
## 12 Times Square                  Broadway-7th Ave
## 13 8th Av                        Canarsie        
## 14 Franklin Av                   Franklin        
## 15 Euclid Av                     Fulton          
## 16 Franklin Av                   Fulton          
## 17 Howard Beach                  Rockaway
```

60 stations service the A train, 17 of them being ADA compliant.

### Problem 2

Reading the Mr. Trash Wheel data from the 2024 Trash Wheel Collection
Data

``` r
mtw = read_excel("202409 Trash Wheel Collection Data.xlsx", sheet = "Mr. Trash Wheel", range = cell_cols("A:M"))
```

Editing variable names to be unique

``` r
names(mtw) <- make.names(names(mtw), unique = TRUE)
```

Removing the last two lines of the sheet, no data contained there

``` r
mtw = mtw[-c(652, 653), ]
```

Viewing the cleaned Mr. Trash Wheel data

``` r
head(mtw)
## # A tibble: 6 × 13
##   Dumpster Month Year  Date                Weight..tons. Volume..cubic.yards.
##      <dbl> <chr> <chr> <dttm>                      <dbl>                <dbl>
## 1        1 May   2014  2014-05-16 00:00:00          4.31                   18
## 2        2 May   2014  2014-05-16 00:00:00          2.74                   13
## 3        3 May   2014  2014-05-16 00:00:00          3.45                   15
## 4        4 May   2014  2014-05-17 00:00:00          3.1                    15
## 5        5 May   2014  2014-05-17 00:00:00          4.06                   18
## 6        6 May   2014  2014-05-20 00:00:00          2.71                   13
## # ℹ 7 more variables: Plastic.Bottles <dbl>, Polystyrene <dbl>,
## #   Cigarette.Butts <dbl>, Glass.Bottles <dbl>, Plastic.Bags <dbl>,
## #   Wrappers <dbl>, Sports.Balls <dbl>
```
