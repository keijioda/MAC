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

## Mixed model analysis on on inflammatory/oxidative markers

-   Each of inflammatory/oxidative variables was log-transformed and
    used as a dependent variable in the following mixed model.
-   The mixed model included:
    -   treatment(mac/control), sequence groups (AB/BA), and phase as
        fixed-effects
    -   subjects as random-effects
-   Marginal means were estimated for both treatments (with 95% CI)
    adjusted for sequence and phase, and then back-transformed into the
    original scale.
-   The last row of `Mac - Control` actually refers to the **ratio** of
    treatment means (mac/control) after the back-transformation.
    -   None of these variables showed a significant difference between
        the two treatments.

<!-- -->

    ## $`log(CRPmgdL)`
    ##      Treatment emmean lower.CL upper.CL   pval
    ##            Mac   4.26     3.18     5.71       
    ##        Control   4.84     3.62     6.49       
    ##  Mac - Control   0.88     0.70     1.11 0.2632
    ## 
    ## $`log(ESelectinngdL)`
    ##      Treatment emmean lower.CL upper.CL   pval
    ##            Mac  35.27    30.26    41.11       
    ##        Control  35.74    30.67    41.66       
    ##  Mac - Control   0.99     0.93     1.04 0.6215
    ## 
    ## $`log(IL6pgmL)`
    ##      Treatment emmean lower.CL upper.CL   pval
    ##            Mac   0.48     0.40     0.58       
    ##        Control   0.49     0.40     0.59       
    ##  Mac - Control   1.00     0.84     1.19 0.9614
    ## 
    ## $`log(TNFapgmL)`
    ##      Treatment emmean lower.CL upper.CL   pval
    ##            Mac   0.98     0.89     1.09       
    ##        Control   1.00     0.91     1.11       
    ##  Mac - Control   0.98     0.92     1.05 0.5777
    ## 
    ## $`log(sICAM1pgmL)`
    ##      Treatment emmean lower.CL upper.CL   pval
    ##            Mac 413.63   378.94   451.50       
    ##        Control 415.91   381.03   453.99       
    ##  Mac - Control   0.99     0.95     1.04 0.8189
    ## 
    ## $`log(sVCAm1pgmL)`
    ##      Treatment emmean lower.CL upper.CL   pval
    ##            Mac 456.00   411.69   505.07       
    ##        Control 459.30   414.67   508.73       
    ##  Mac - Control   0.99     0.95     1.04 0.7387
    ## 
    ## $`log(isopgf2pgmL)`
    ##      Treatment emmean lower.CL upper.CL   pval
    ##            Mac 649.78   595.56   708.94       
    ##        Control 626.20   573.95   683.21       
    ##  Mac - Control   1.04     0.93     1.16 0.4896
    ## 
    ## $`log(MDAnmolmL)`
    ##      Treatment emmean lower.CL upper.CL   pval
    ##            Mac   2.09     1.78     2.46       
    ##        Control   2.28     1.94     2.68       
    ##  Mac - Control   0.92     0.82     1.03 0.1311
