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

Rounding the number of sports balls to the nearest integer

``` r
mtw <- mtw %>%
  mutate(Sports.Balls = as.integer(round(as.numeric(Sports.Balls))))
```

Check for rounded value in sports balls column

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
## #   Wrappers <dbl>, Sports.Balls <int>
```

Reading the Professor Trash Wheel data from the 2024 Trash Wheel
Collection Data

``` r
ptw = read_excel("202409 Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel", range = cell_cols("A:L"))
```

Editing variable names to be unique

``` r
names(ptw) <- make.names(names(ptw), unique = TRUE)
```

Removing the last three lines of the sheet, no data contained there

``` r
ptw = ptw[-c(119, 120, 121), ]
```

Viewing the cleaned Professor Trash Wheel data

``` r
head(ptw)
## # A tibble: 6 × 12
##   Dumpster Month     Year Date                Weight..tons. Volume..cubic.yards.
##      <dbl> <chr>    <dbl> <dttm>                      <dbl>                <dbl>
## 1        1 January   2017 2017-01-02 00:00:00          1.79                   15
## 2        2 January   2017 2017-01-30 00:00:00          1.58                   15
## 3        3 February  2017 2017-02-26 00:00:00          2.32                   18
## 4        4 February  2017 2017-02-26 00:00:00          3.72                   15
## 5        5 February  2017 2017-02-28 00:00:00          1.45                   15
## 6        6 March     2017 2017-03-30 00:00:00          1.71                   15
## # ℹ 6 more variables: Plastic.Bottles <dbl>, Polystyrene <dbl>,
## #   Cigarette.Butts <dbl>, Glass.Bottles <dbl>, Plastic.Bags <dbl>,
## #   Wrappers <dbl>
```

Reading the Gwynnda Trash Wheel data from the 2024 Trash Wheel
Collection Data

``` r
gtw = read_excel("202409 Trash Wheel Collection Data.xlsx", sheet = "Gwynnda Trash Wheel", range = cell_cols("A:K"))
```

Editing variable names to be unique

``` r
names(gtw) <- make.names(names(gtw), unique = TRUE)
```

Removing the last line of the sheet, no data contained there

``` r
gtw = gtw[-264, ]
```

Viewing the cleaned Gwynnda Trash Wheel data

``` r
head(gtw)
## # A tibble: 6 × 11
##   Dumpster Month   Year Date                Weight..tons. Volume..cubic.yards.
##      <dbl> <chr>  <dbl> <dttm>                      <dbl>                <dbl>
## 1        1 July    2021 2021-07-03 00:00:00          0.93                   15
## 2        2 July    2021 2021-07-07 00:00:00          2.26                   15
## 3        3 July    2021 2021-07-07 00:00:00          1.62                   15
## 4        4 July    2021 2021-07-16 00:00:00          1.76                   15
## 5        5 July    2021 2021-07-30 00:00:00          1.53                   15
## 6        6 August  2021 2021-08-11 00:00:00          2.06                   15
## # ℹ 5 more variables: Plastic.Bottles <dbl>, Polystyrene <dbl>,
## #   Cigarette.Butts <dbl>, Plastic.Bags <dbl>, Wrappers <dbl>
```

Combining data sets

``` r
mtw <- mtw %>% mutate(Trash_Wheel = "Mr. Trash Wheel")
ptw <- ptw %>% mutate(Trash_Wheel = "Professor Trash Wheel")
gtw <- gtw %>% mutate(Trash_Wheel = "Gwynnda Trash Wheel")
```

Viewing structure of data set variables

``` r
str(mtw)
## tibble [651 × 14] (S3: tbl_df/tbl/data.frame)
##  $ Dumpster            : num [1:651] 1 2 3 4 5 6 7 8 9 10 ...
##  $ Month               : chr [1:651] "May" "May" "May" "May" ...
##  $ Year                : chr [1:651] "2014" "2014" "2014" "2014" ...
##  $ Date                : POSIXct[1:651], format: "2014-05-16" "2014-05-16" ...
##  $ Weight..tons.       : num [1:651] 4.31 2.74 3.45 3.1 4.06 2.71 1.91 3.7 2.52 3.76 ...
##  $ Volume..cubic.yards.: num [1:651] 18 13 15 15 18 13 8 16 14 18 ...
##  $ Plastic.Bottles     : num [1:651] 1450 1120 2450 2380 980 1430 910 3580 2400 1340 ...
##  $ Polystyrene         : num [1:651] 1820 1030 3100 2730 870 2140 1090 4310 2790 1730 ...
##  $ Cigarette.Butts     : num [1:651] 126000 91000 105000 100000 120000 90000 56000 112000 98000 130000 ...
##  $ Glass.Bottles       : num [1:651] 72 42 50 52 72 46 32 58 49 75 ...
##  $ Plastic.Bags        : num [1:651] 584 496 1080 896 368 ...
##  $ Wrappers            : num [1:651] 1162 874 2032 1971 753 ...
##  $ Sports.Balls        : int [1:651] 7 5 6 6 7 5 3 6 6 7 ...
##  $ Trash_Wheel         : chr [1:651] "Mr. Trash Wheel" "Mr. Trash Wheel" "Mr. Trash Wheel" "Mr. Trash Wheel" ...
str(ptw)
## tibble [118 × 13] (S3: tbl_df/tbl/data.frame)
##  $ Dumpster            : num [1:118] 1 2 3 4 5 6 7 8 9 10 ...
##  $ Month               : chr [1:118] "January" "January" "February" "February" ...
##  $ Year                : num [1:118] 2017 2017 2017 2017 2017 ...
##  $ Date                : POSIXct[1:118], format: "2017-01-02" "2017-01-30" ...
##  $ Weight..tons.       : num [1:118] 1.79 1.58 2.32 3.72 1.45 1.71 1.82 2.37 2.64 2.78 ...
##  $ Volume..cubic.yards.: num [1:118] 15 15 18 15 15 15 15 15 15 15 ...
##  $ Plastic.Bottles     : num [1:118] 1950 9540 8350 8590 7830 8210 9830 9240 9540 8230 ...
##  $ Polystyrene         : num [1:118] 6080 11230 9210 1030 9950 ...
##  $ Cigarette.Butts     : num [1:118] 19700 17600 12000 13000 16000 14000 17000 15000 17000 13000 ...
##  $ Glass.Bottles       : num [1:118] 8 14 19 21 18 23 26 14 28 22 ...
##  $ Plastic.Bags        : num [1:118] 3100 5630 6430 5870 7450 ...
##  $ Wrappers            : num [1:118] 15600 16700 12400 11030 15340 ...
##  $ Trash_Wheel         : chr [1:118] "Professor Trash Wheel" "Professor Trash Wheel" "Professor Trash Wheel" "Professor Trash Wheel" ...
str(gtw)
## tibble [263 × 12] (S3: tbl_df/tbl/data.frame)
##  $ Dumpster            : num [1:263] 1 2 3 4 5 6 7 8 9 10 ...
##  $ Month               : chr [1:263] "July" "July" "July" "July" ...
##  $ Year                : num [1:263] 2021 2021 2021 2021 2021 ...
##  $ Date                : POSIXct[1:263], format: "2021-07-03" "2021-07-07" ...
##  $ Weight..tons.       : num [1:263] 0.93 2.26 1.62 1.76 1.53 2.06 1.9 2.16 2.6 3.21 ...
##  $ Volume..cubic.yards.: num [1:263] 15 15 15 15 15 15 15 15 15 15 ...
##  $ Plastic.Bottles     : num [1:263] 1200 2000 1800 1000 2100 2400 2700 3000 980 240 ...
##  $ Polystyrene         : num [1:263] 360 240 270 180 240 360 320 320 180 42 ...
##  $ Cigarette.Butts     : num [1:263] 3400 3900 2900 2100 4000 3900 4200 4000 1800 400 ...
##  $ Plastic.Bags        : num [1:263] 1800 2200 2400 1800 2700 3000 3200 3600 1000 360 ...
##  $ Wrappers            : num [1:263] NA NA NA NA NA NA NA NA NA NA ...
##  $ Trash_Wheel         : chr [1:263] "Gwynnda Trash Wheel" "Gwynnda Trash Wheel" "Gwynnda Trash Wheel" "Gwynnda Trash Wheel" ...
```

Converting date variable into character

``` r
mtw <- mtw %>% mutate(Year = as.character(Year))
ptw <- ptw %>% mutate(Year = as.character(Year))
gtw <- gtw %>% mutate(Year = as.character(Year))
```

Combining the data sets

``` r
combined_data <- bind_rows(mtw, ptw, gtw)
```

Making sure that trash wheel is the first column in the data set

``` r
combined_data <- combined_data %>%
  select(Trash_Wheel, everything()) %>%  # Ensure Trash_Wheel is the first column
  distinct()
```

Printing the combined data

``` r
print(combined_data)
## # A tibble: 1,032 × 14
##    Trash_Wheel     Dumpster Month Year  Date                Weight..tons.
##    <chr>              <dbl> <chr> <chr> <dttm>                      <dbl>
##  1 Mr. Trash Wheel        1 May   2014  2014-05-16 00:00:00          4.31
##  2 Mr. Trash Wheel        2 May   2014  2014-05-16 00:00:00          2.74
##  3 Mr. Trash Wheel        3 May   2014  2014-05-16 00:00:00          3.45
##  4 Mr. Trash Wheel        4 May   2014  2014-05-17 00:00:00          3.1 
##  5 Mr. Trash Wheel        5 May   2014  2014-05-17 00:00:00          4.06
##  6 Mr. Trash Wheel        6 May   2014  2014-05-20 00:00:00          2.71
##  7 Mr. Trash Wheel        7 May   2014  2014-05-21 00:00:00          1.91
##  8 Mr. Trash Wheel        8 May   2014  2014-05-28 00:00:00          3.7 
##  9 Mr. Trash Wheel        9 June  2014  2014-06-05 00:00:00          2.52
## 10 Mr. Trash Wheel       10 June  2014  2014-06-11 00:00:00          3.76
## # ℹ 1,022 more rows
## # ℹ 8 more variables: Volume..cubic.yards. <dbl>, Plastic.Bottles <dbl>,
## #   Polystyrene <dbl>, Cigarette.Butts <dbl>, Glass.Bottles <dbl>,
## #   Plastic.Bags <dbl>, Wrappers <dbl>, Sports.Balls <int>
```

Identifying how many total rows are in the dataset

``` r
nrow(combined_data)
## [1] 1032
```

Identifying the total weight of trash collected by Professor Trash Wheel

``` r
total_weight_professor_trash_wheel <- sum(ptw %>% pull(Weight..tons.), na.rm = TRUE)
```

Printing the total weight of trash collected by Professor Trash Wheel

``` r
print(total_weight_professor_trash_wheel)
## [1] 246.74
```

Identifying the total number of cigarette butts collected by Gwynnda in
June of 2022

``` r
total_cigarette_butts_gwynnda <- sum(gtw %>% filter(Month == "June", Year == 2022) %>% pull(Cigarette.Butts), na.rm = TRUE)
```

Printing the total number of cigarette butts collected by Gwynnda in
June of 2022

``` r
print(total_cigarette_butts_gwynnda)
## [1] 18120
```

Key variables

``` r
glimpse(combined_data)
## Rows: 1,032
## Columns: 14
## $ Trash_Wheel          <chr> "Mr. Trash Wheel", "Mr. Trash Wheel", "Mr. Trash …
## $ Dumpster             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15…
## $ Month                <chr> "May", "May", "May", "May", "May", "May", "May", …
## $ Year                 <chr> "2014", "2014", "2014", "2014", "2014", "2014", "…
## $ Date                 <dttm> 2014-05-16, 2014-05-16, 2014-05-16, 2014-05-17, …
## $ Weight..tons.        <dbl> 4.31, 2.74, 3.45, 3.10, 4.06, 2.71, 1.91, 3.70, 2…
## $ Volume..cubic.yards. <dbl> 18, 13, 15, 15, 18, 13, 8, 16, 14, 18, 15, 19, 15…
## $ Plastic.Bottles      <dbl> 1450, 1120, 2450, 2380, 980, 1430, 910, 3580, 240…
## $ Polystyrene          <dbl> 1820, 1030, 3100, 2730, 870, 2140, 1090, 4310, 27…
## $ Cigarette.Butts      <dbl> 126000, 91000, 105000, 100000, 120000, 90000, 560…
## $ Glass.Bottles        <dbl> 72, 42, 50, 52, 72, 46, 32, 58, 49, 75, 38, 45, 5…
## $ Plastic.Bags         <dbl> 584, 496, 1080, 896, 368, 672, 416, 1552, 984, 44…
## $ Wrappers             <dbl> 1162, 874, 2032, 1971, 753, 1144, 692, 3015, 1988…
## $ Sports.Balls         <int> 7, 5, 6, 6, 7, 5, 3, 6, 6, 7, 6, 8, 6, 6, 6, 6, 5…
```

In this combined data set containing data from Mr. Trash Wheel,
Professor Trash Wheel, and Gwynnda Trash Wheel sets, there are 1032
total observations. Key variables in this data set are which trash wheel
the data comes from, month, year, the weight (in tons), date of trash
collection, and cigarette butts collected. The total weight of trash
collected by Professor Trash Wheel is 246.72tons. The total number of
cigarette butts collected by Gwynnda in June of 2022 was 18120. This
combined data set shows key information on how much trash is collected
by each wheel, type of trash, and when it was collected.

### Problem 3

Importing and cleaning the data

``` r
bakers_df = read_csv("gbb_datasets/bakers.csv") |>
  janitor::clean_names() 

bakes_df = read_csv("gbb_datasets/bakes.csv",
                    na = c("NA", "", ".", "N/A", "UNKNOWN", "Unknown")) |>
  janitor::clean_names() 

results = read_csv("gbb_datasets/results.csv", skip = 2) |>
   janitor::clean_names() 
```

``` r
colnames(results) <- c("series", "Episode", "baker", "Technical", "Result")
```

Removing the first two rows in the results file

``` r
results = results[-c(1, 2), ]
```

Change variable to be in unison, baker for the baker name across all
datasets

``` r
bakers_df = bakers_df |>
  separate(baker_name, into = c("baker", "baker_last_name"), sep = " ")
```

Check for completeness

``` r
na_bakers_df = bakers_df |>
  anti_join(bakes_df, by = c("baker", "series"))
```

``` r
print(na_bakers_df)
## # A tibble: 26 × 6
##    baker  baker_last_name series baker_age baker_occupation             hometown
##    <chr>  <chr>            <dbl>     <dbl> <chr>                        <chr>   
##  1 Alice  Fevronia            10        28 Geography teacher            Essex   
##  2 Amelia LeBruin             10        24 Fashion designer             Halifax 
##  3 Antony Amourdoux            9        30 Banker                       London  
##  4 Briony Williams             9        33 Full-time parent             Bristol 
##  5 Dan    Beasley-Harling      9        36 Full-time parent             London  
##  6 Dan    Chambers            10        32 Support worker               Rotherh…
##  7 David  Atherton            10        36 International health adviser Whitby  
##  8 Helena Garcia              10        40 Online project manager       Leeds   
##  9 Henry  Bird                10        20 Student                      Durham  
## 10 Imelda McCarron             9        33 Countryside recreation offi… County …
## # ℹ 16 more rows
```

25 reported missing bakers who do not have the information across
datasets

Checking for those who do not have the same information recorded in
bakers and bakes data sets

``` r
anti_join(bakers_df, bakes_df)
## # A tibble: 26 × 6
##    baker  baker_last_name series baker_age baker_occupation             hometown
##    <chr>  <chr>            <dbl>     <dbl> <chr>                        <chr>   
##  1 Alice  Fevronia            10        28 Geography teacher            Essex   
##  2 Amelia LeBruin             10        24 Fashion designer             Halifax 
##  3 Antony Amourdoux            9        30 Banker                       London  
##  4 Briony Williams             9        33 Full-time parent             Bristol 
##  5 Dan    Beasley-Harling      9        36 Full-time parent             London  
##  6 Dan    Chambers            10        32 Support worker               Rotherh…
##  7 David  Atherton            10        36 International health adviser Whitby  
##  8 Helena Garcia              10        40 Online project manager       Leeds   
##  9 Henry  Bird                10        20 Student                      Durham  
## 10 Imelda McCarron             9        33 Countryside recreation offi… County …
## # ℹ 16 more rows
```

Checking for those who do not have the same information in bakers and
results data sets

``` r
anti_join(bakers_df, results)
## # A tibble: 1 × 6
##   baker baker_last_name series baker_age baker_occupation hometown    
##   <chr> <chr>            <dbl>     <dbl> <chr>            <chr>       
## 1 Jo    Wheatley             2        41 Housewife        Ongar, Essex
```

Identified Jo Wheatley

Change Jo’s name to be consistent across data sets

``` r
results = results |>
  mutate(baker = case_match(baker, "Joanne" ~ "Jo"))
```

Merge bakers and bakes data sets

``` r
bakers_and_bakes = 
  left_join(bakers_df, bakes_df, by = c("series", "baker"))
```

Check to see if merge worked

``` r
print(bakers_and_bakes)
## # A tibble: 566 × 9
##    baker baker_last_name series baker_age baker_occupation  hometown     episode
##    <chr> <chr>            <dbl>     <dbl> <chr>             <chr>          <dbl>
##  1 Ali   Imdad                4        25 Charity worker    Saltley, Bi…       1
##  2 Ali   Imdad                4        25 Charity worker    Saltley, Bi…       2
##  3 Ali   Imdad                4        25 Charity worker    Saltley, Bi…       3
##  4 Ali   Imdad                4        25 Charity worker    Saltley, Bi…       4
##  5 Alice Fevronia            10        28 Geography teacher Essex             NA
##  6 Alvin Magallanes           6        37 Nurse             Bracknell, …       1
##  7 Alvin Magallanes           6        37 Nurse             Bracknell, …       2
##  8 Alvin Magallanes           6        37 Nurse             Bracknell, …       3
##  9 Alvin Magallanes           6        37 Nurse             Bracknell, …       4
## 10 Alvin Magallanes           6        37 Nurse             Bracknell, …       5
## # ℹ 556 more rows
## # ℹ 2 more variables: signature_bake <chr>, show_stopper <chr>
```

Merge bakers and bakes data to the results data

``` r
gbb = 
  left_join(bakers_and_bakes, results, by = c("series", "baker"))
```

Check to see if merge worked

``` r
print(gbb)
## # A tibble: 573 × 12
##    baker baker_last_name series baker_age baker_occupation  hometown     episode
##    <chr> <chr>            <dbl>     <dbl> <chr>             <chr>          <dbl>
##  1 Ali   Imdad                4        25 Charity worker    Saltley, Bi…       1
##  2 Ali   Imdad                4        25 Charity worker    Saltley, Bi…       2
##  3 Ali   Imdad                4        25 Charity worker    Saltley, Bi…       3
##  4 Ali   Imdad                4        25 Charity worker    Saltley, Bi…       4
##  5 Alice Fevronia            10        28 Geography teacher Essex             NA
##  6 Alvin Magallanes           6        37 Nurse             Bracknell, …       1
##  7 Alvin Magallanes           6        37 Nurse             Bracknell, …       2
##  8 Alvin Magallanes           6        37 Nurse             Bracknell, …       3
##  9 Alvin Magallanes           6        37 Nurse             Bracknell, …       4
## 10 Alvin Magallanes           6        37 Nurse             Bracknell, …       5
## # ℹ 563 more rows
## # ℹ 5 more variables: signature_bake <chr>, show_stopper <chr>, Episode <dbl>,
## #   Technical <dbl>, Result <chr>
```

Export to a CSV file in folder

``` r
write_csv(gbb, "gbb_datasets/gbb.csv")
```

Importing and cleaning viewers data

``` r
viewers_df <- read_csv("gbb_datasets/viewers.csv", show_col_types = FALSE) %>%
  janitor::clean_names()
```

Showing the first 10 line

``` r
head(viewers_df, 10)
## # A tibble: 10 × 11
##    episode series_1 series_2 series_3 series_4 series_5 series_6 series_7
##      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1       1     2.24     3.1      3.85     6.6      8.51     11.6     13.6
##  2       2     3        3.53     4.6      6.65     8.79     11.6     13.4
##  3       3     3        3.82     4.53     7.17     9.28     12.0     13.0
##  4       4     2.6      3.6      4.71     6.82    10.2      12.4     13.3
##  5       5     3.03     3.83     4.61     6.95     9.95     12.4     13.1
##  6       6     2.75     4.25     4.82     7.32    10.1      12       13.1
##  7       7    NA        4.42     5.1      7.76    10.3      12.4     13.4
##  8       8    NA        5.06     5.35     7.41     9.02     11.1     13.3
##  9       9    NA       NA        5.7      7.41    10.7      12.6     13.4
## 10      10    NA       NA        6.74     9.45    13.5      15.0     15.9
## # ℹ 3 more variables: series_8 <dbl>, series_9 <dbl>, series_10 <dbl>
```

Finding the avergae viewing in season 1

``` r
average_season_1 <- viewers_df %>%
  summarize(average_viewership = mean(series_1, na.rm = TRUE))
```

Print the average

``` r
print(average_season_1)
## # A tibble: 1 × 1
##   average_viewership
##                <dbl>
## 1               2.77
```

The average viewership in season 1 is 2.77.

Finding the avergae viewing in season 5

``` r
average_season_5 <- viewers_df %>%
  summarize(average_viewership = mean(series_5, na.rm = TRUE))
```

Print the average

``` r
print(average_season_5)
## # A tibble: 1 × 1
##   average_viewership
##                <dbl>
## 1               10.0
```

The average viewership in season 5 is 10.04.
