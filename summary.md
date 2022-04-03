MAC study
================

## Datasets

-   File name: `MAC Endpoint data with baseline values 102121.sav`
-   Data are from a 2 x 2 crossover design (AB/BA) with control/mac
    treatments
-   Includes *n* = 70 observations from 35 subjects
-   There are 113 variables including:
    -   `ID`
    -   Study design variables:
        -   Sequence: `Group`
        -   `Phase`  
        -   `Visit`
        -   `Treatment`
    -   Demographics & body measurements
        -   `Age`
        -   `Height`
        -   `Weight`
        -   `BMI`
        -   Waist circumference: `WC`
    -   Blood lipids
    -   Insulin resistance
    -   Inflammatory/oxidative markers

## Descriptive analysis on inflammatory/oxidative markers

-   Variables included:
    -   `CRPmgdL`
    -   `ESelectinngdL`
    -   `IL6pgmL`
    -   `TNFapgmL`
    -   `sICAM1pgmL`
    -   `sVCAm1pgmL`
    -   `isopgf2pgmL`
    -   `MDAnmolmL`

### Mean/SD by treatment

    ## `summarise()` has grouped output by 'var'. You can override using the `.groups`
    ## argument.

| var           | Treatment |   mean |     sd |
|:--------------|:----------|-------:|-------:|
| CRPmgdL       | Baseline  |   5.68 |   5.10 |
| CRPmgdL       | Control   |   6.84 |   6.86 |
| CRPmgdL       | Mac       |   6.49 |   6.97 |
| ESelectinngdL | Baseline  |  38.49 |  17.35 |
| ESelectinngdL | Control   |  39.91 |  21.50 |
| ESelectinngdL | Mac       |  38.67 |  17.26 |
| IL6pgmL       | Baseline  |   0.56 |   0.33 |
| IL6pgmL       | Control   |   0.57 |   0.33 |
| IL6pgmL       | Mac       |   0.54 |   0.24 |
| TNFapgmL      | Baseline  |   1.09 |   0.39 |
| TNFapgmL      | Control   |   1.04 |   0.33 |
| TNFapgmL      | Mac       |   1.03 |   0.36 |
| sICAM1pgmL    | Baseline  | 415.37 | 119.66 |
| sICAM1pgmL    | Control   | 430.07 | 115.48 |
| sICAM1pgmL    | Mac       | 426.82 | 110.24 |
| sVCAm1pgmL    | Baseline  | 467.83 | 162.66 |
| sVCAm1pgmL    | Control   | 480.02 | 147.22 |
| sVCAm1pgmL    | Mac       | 476.86 | 147.55 |
| isopgf2pgmL   | Baseline  | 629.86 | 154.47 |
| isopgf2pgmL   | Control   | 642.39 | 139.09 |
| isopgf2pgmL   | Mac       | 671.19 | 163.09 |
| MDAnmolmL     | Baseline  |   2.44 |   1.05 |
| MDAnmolmL     | Control   |   2.49 |   1.05 |
| MDAnmolmL     | Mac       |   2.36 |   1.21 |

### Mean/SD by sequence group and treatment

    ##                                                            
    ##                     Mac-Control         Control-Mac        
    ##                     Mac         Control Mac         Control
    ##  CRPmgdL       Mean   4.77        6.43    8.31        7.26 
    ##                SD     4.22        7.29    8.81        6.58 
    ##  ESelectinngdL Mean  40.06       43.04   37.21       36.58 
    ##                SD    16.59       25.12   18.35       17.01 
    ##  IL6pgmL       Mean   0.53        0.62    0.56        0.52 
    ##                SD     0.26        0.36    0.23        0.30 
    ##  TNFapgmL      Mean   0.96        0.98    1.11        1.11 
    ##                SD     0.19        0.24    0.47        0.40 
    ##  sICAM1pgmL    Mean 426.24      443.93  427.44      415.39 
    ##                SD    95.95      119.30  126.65      113.02 
    ##  sVCAm1pgmL    Mean 484.21      495.64  469.08      463.49 
    ##                SD   106.68      130.19  184.52      165.78 
    ##  isopgf2pgmL   Mean 656.72      642.26  686.51      642.53 
    ##                SD   163.13      123.18  166.62      158.07 
    ##  MDAnmolmL     Mean   2.36        2.34    2.36        2.66 
    ##                SD     1.31        1.03    1.13        1.07
