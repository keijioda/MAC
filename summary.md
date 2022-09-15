MAC study
================

## Datasets

-   File name: `MAC Endpoint data with baseline values 102121.sav`
-   Data are from a 2 x 2 crossover design (AB/BA) with control/mac
    treatments
-   Includes *n* = 70 observations from 35 subjects
-   There are 114 variables including:
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

## Descriptive analysis on insulin resistance

-   Variables included:
    -   `GlucosemgdL`
    -   `InsulinuIUml`
    -   `HOMA2_IR`

### Mean/SD by treatment

    ##                                           
    ##                    Baseline Control Mac   
    ##  GlucosemgdL  Mean 103.83   102.43  102.71
    ##               SD     7.33     7.66    8.81
    ##  InsulinuIUml Mean  11.65    12.66   13.55
    ##               SD     5.79     6.81    6.27
    ##  HOMA2_IR     Mean   1.35     1.46    1.56
    ##               SD     0.67     0.78    0.72

### Mean/SD by sequence group and treatment

    ##                                                           
    ##                    Mac-Control         Control-Mac        
    ##                    Mac         Control Mac         Control
    ##  GlucosemgdL  Mean 102.17      103.89  103.29      100.88 
    ##               SD     7.67        7.44   10.09        7.82 
    ##  InsulinuIUml Mean  14.13       13.68   12.93       11.58 
    ##               SD     6.59        6.96    6.05        6.68 
    ##  HOMA2_IR     Mean   1.62        1.58    1.49        1.33 
    ##               SD     0.74        0.80    0.71        0.77

## Mixed model analysis on on insulin resistance

-   For insulin and HOMA-IR, a log-transformation was applied. No
    transformation was used on glucose.
-   The mixed model included:
    -   treatment(mac/control), sequence groups (AB/BA), and phase as
        fixed-effects
    -   subjects as random-effects
-   Marginal means were estimated for both treatments (with 95% CI)
    adjusted for sequence and phase. For log-transformed outcomes,
    estimated marginal means (and 95% CIs) were back-transformed into
    the original scale, while the standard errors are on the log scale.
-   For insulin and HOMA-IR, the last row of `Mac - Control` actually
    refers to the **ratio** of treatment means (mac/control) after the
    back-transformation.
    -   None of these variables showed a significant difference between
        the two treatments.

<!-- -->

    ## $GlucosemgdL
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac 102.73 1.40    99.89   105.57       
    ##        Control 102.39 1.40    99.54   105.23       
    ##  Mac - Control   0.34 0.74    -1.16     1.85 0.6443
    ## 
    ## $`log(InsulinuIUml)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac  12.13 0.09    10.17    14.46       
    ##        Control  11.07 0.09     9.28    13.20       
    ##  Mac - Control   1.10 0.05     0.98     1.22 0.0997
    ## 
    ## $`log(HOMA2_IR)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac   1.40 0.09     1.17     1.66       
    ##        Control   1.27 0.09     1.07     1.52       
    ##  Mac - Control   1.10 0.05     0.98     1.22 0.0953

### Examining the interaction with baseline BMI

-   To examine if the effect of mac treatment may be different depending
    on baseline BMI, a dichotomous BMI variable (\<30 and \>=30) was
    added into the mixed model, along with its interaction with
    treatment.
-   Marginal means were estimated for each combination of treatment and
    baseline BMI adjusted for sequence and phase. For log-transformed
    outcomes, estimated marginal means were back-transformed into the
    original scale.
-   For insulin and HOMA-IR, the last row of `Mac - Control` actually
    refers to the **ratio** of treatment means (mac/control) after the
    back-transformation.
    -   None of these variables showed a significant interaction with
        baseline BMI, indicating that the effect of mac treatment is not
        significantly different between BMI \<30 and \>=30.

<!-- -->

    ## $GlucosemgdL
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30 103.74    99.72   107.76              
    ##     Control     <30 102.89    98.88   106.91              
    ##  Mac - Ctrl     <30   0.85    -1.28     2.97 0.4235       
    ##         Mac    >=30 101.67    97.54   105.79              
    ##     Control    >=30 101.85    97.73   105.98              
    ##  Mac - Ctrl    >=30  -0.18    -2.36     2.00 0.8654 0.4970
    ## 
    ## $`log(InsulinuIUml)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30  10.87     8.53    13.87              
    ##     Control     <30   9.75     7.65    12.43              
    ##  Mac - Ctrl     <30   1.12     0.95     1.30 0.1623       
    ##         Mac    >=30  13.60    10.60    17.45              
    ##     Control    >=30  12.65     9.86    16.23              
    ##  Mac - Ctrl    >=30   1.08     0.92     1.26 0.3631 0.7385
    ## 
    ## $`log(HOMA2_IR)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30   1.26     0.99     1.60              
    ##     Control     <30   1.13     0.88     1.44              
    ##  Mac - Ctrl     <30   1.12     0.96     1.30 0.1520       
    ##         Mac    >=30   1.56     1.21     2.00              
    ##     Control    >=30   1.45     1.13     1.86              
    ##  Mac - Ctrl    >=30   1.07     0.92     1.26 0.3655 0.7169

### Examining the interaction with baseline WC

-   To examine if the effect of mac treatment may be different depending
    on baseline waist circumference (WC), a dichotomous WC variable
    (\<108 and \>=108) was added into the mixed model, along with its
    interaction with treatment.
-   Marginal means were estimated for each combination of treatment and
    baseline BMI adjusted for sequence and phase. For log-transformed
    outcomes, estimated marginal means were back-transformed into the
    original scale.
-   For insulin and HOMA-IR, the last row of `Mac - Control` actually
    refers to the **ratio** of treatment means (mac/control) after the
    back-transformation.
    -   For insulin and HOMA-IR, the treatment x baseline WC interaction
        was significant. In both outcomes, the mean after mac treatment
        was significantly higher than the control among those with
        baseline WC \< 108 cm. However, no significant treatment
        difference was found among those with baseline WC \>= 108 cm.

<!-- -->

    ## $GlucosemgdL
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108 102.34    98.42   106.26              
    ##     Control   <108 102.55    98.64   106.47              
    ##  Mac - Ctrl   <108  -0.22    -2.27     1.84 0.8312       
    ##         Mac  >=108 103.20    98.92   107.48              
    ##     Control  >=108 102.19    97.90   106.47              
    ##  Mac - Ctrl  >=108   1.02    -1.23     3.26 0.3643 0.4168
    ## 
    ## $`log(InsulinuIUml)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108  10.90     8.69    13.67              
    ##     Control   <108   8.97     7.15    11.25              
    ##  Mac - Ctrl   <108   1.22     1.06     1.40 0.0083       
    ##         Mac  >=108  13.78    10.75    17.65              
    ##     Control  >=108  14.23    11.10    18.23              
    ##  Mac - Ctrl  >=108   0.97     0.83     1.13 0.6738 0.0345
    ## 
    ## $`log(HOMA2_IR)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108   1.26     1.00     1.58              
    ##     Control   <108   1.04     0.83     1.30              
    ##  Mac - Ctrl   <108   1.21     1.05     1.39 0.0085       
    ##         Mac  >=108   1.58     1.23     2.03              
    ##     Control  >=108   1.63     1.27     2.09              
    ##  Mac - Ctrl  >=108   0.97     0.83     1.13 0.7029 0.0372

### Examining the interaction with baseline % body fat

-   To examine if the effect of mac treatment may be different depending
    on baseline % body fat (BF), a dichotomous BF variable (\<43 and
    \>=43) was added into the mixed model, along with its interaction
    with treatment.
-   Marginal means were estimated for each combination of treatment and
    baseline BMI adjusted for sequence and phase. For log-transformed
    outcomes, estimated marginal means were back-transformed into the
    original scale.
-   For insulin and HOMA-IR, the last row of `Mac - Control` actually
    refers to the **ratio** of treatment means (mac/control) after the
    back-transformation.
    -   None of these variables showed a significant interaction with
        baseline BF, indicating that the effect of mac treatment is not
        significantly different between % body fat \<43 and \>=43.

<!-- -->

    ## $GlucosemgdL
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43 104.42   100.49   108.36              
    ##     Control    <43 103.19    99.37   107.00              
    ##  Mac - Ctrl    <43   1.24    -1.00     3.48 0.2689       
    ##         Mac   >=43 101.31    97.68   104.95              
    ##     Control   >=43 101.63    97.91   105.35              
    ##  Mac - Ctrl   >=43  -0.32    -2.43     1.79 0.7594 0.3127
    ## 
    ## $`log(InsulinuIUml)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43  12.44     9.65    16.03              
    ##     Control    <43  11.35     8.87    14.51              
    ##  Mac - Ctrl    <43   1.10     0.93     1.29 0.2639       
    ##         Mac   >=43  11.87     9.39    15.00              
    ##     Control   >=43  10.81     8.51    13.74              
    ##  Mac - Ctrl   >=43   1.10     0.94     1.28 0.2272 0.9874
    ## 
    ## $`log(HOMA2_IR)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43   1.44     1.12     1.85              
    ##     Control    <43   1.31     1.03     1.67              
    ##  Mac - Ctrl    <43   1.10     0.93     1.29 0.2499       
    ##         Mac   >=43   1.36     1.08     1.72              
    ##     Control   >=43   1.24     0.98     1.57              
    ##  Mac - Ctrl   >=43   1.10     0.94     1.28 0.2251 0.9959

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

    ##                                            
    ##                     Baseline Control Mac   
    ##  CRPmgdL       Mean   5.68     6.84    6.49
    ##                SD     5.10     6.86    6.97
    ##  ESelectinngdL Mean  38.49    39.91   38.67
    ##                SD    17.35    21.50   17.26
    ##  IL6pgmL       Mean   0.56     0.57    0.54
    ##                SD     0.33     0.33    0.24
    ##  TNFapgmL      Mean   1.09     1.04    1.03
    ##                SD     0.39     0.33    0.36
    ##  sICAM1pgmL    Mean 415.37   430.07  426.82
    ##                SD   119.66   115.48  110.24
    ##  sVCAm1pgmL    Mean 467.83   480.02  476.86
    ##                SD   162.66   147.22  147.55
    ##  isopgf2pgmL   Mean 629.86   642.39  671.19
    ##                SD   154.47   139.09  163.09
    ##  MDAnmolmL     Mean   2.44     2.49    2.36
    ##                SD     1.05     1.05    1.21

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

## Mixed model analysis on inflammatory/oxidative markers

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
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac   4.26 0.14     3.18     5.71       
    ##        Control   4.84 0.14     3.62     6.49       
    ##  Mac - Control   0.88 0.11     0.70     1.11 0.2632
    ## 
    ## $`log(ESelectinngdL)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac  35.27 0.08    30.26    41.11       
    ##        Control  35.74 0.08    30.67    41.66       
    ##  Mac - Control   0.99 0.03     0.93     1.04 0.6215
    ## 
    ## $`log(IL6pgmL)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac   0.48 0.09     0.40     0.58       
    ##        Control   0.49 0.09     0.40     0.59       
    ##  Mac - Control   1.00 0.09     0.84     1.19 0.9614
    ## 
    ## $`log(TNFapgmL)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac   0.98 0.05     0.89     1.09       
    ##        Control   1.00 0.05     0.91     1.11       
    ##  Mac - Control   0.98 0.03     0.92     1.05 0.5777
    ## 
    ## $`log(sICAM1pgmL)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac 413.63 0.04   378.94   451.50       
    ##        Control 415.91 0.04   381.03   453.99       
    ##  Mac - Control   0.99 0.02     0.95     1.04 0.8189
    ## 
    ## $`log(sVCAm1pgmL)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac 456.00 0.05   411.69   505.07       
    ##        Control 459.30 0.05   414.67   508.73       
    ##  Mac - Control   0.99 0.02     0.95     1.04 0.7387
    ## 
    ## $`log(isopgf2pgmL)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac 649.78 0.04   595.56   708.94       
    ##        Control 626.20 0.04   573.95   683.21       
    ##  Mac - Control   1.04 0.05     0.93     1.16 0.4896
    ## 
    ## $`log(MDAnmolmL)`
    ##      Treatment emmean   SE lower.CL upper.CL   pval
    ##            Mac   2.09 0.08     1.78     2.46       
    ##        Control   2.28 0.08     1.94     2.68       
    ##  Mac - Control   0.92 0.06     0.82     1.03 0.1311

### Examining the interaction with baseline BMI

-   To examine if the effect of mac treatment may be different depending
    on baseline BMI, a dichotomous BMI variable (\<30 and \>=30) was
    added into the mixed model, along with its interaction with
    treatment.
-   Marginal means were estimated for each combination of treatment and
    baseline BMI adjusted for sequence and phase, and then
    back-transformed into the original scale.
-   The last row of `Mac - Control` actually refers to the **ratio** of
    treatment means (mac/control) after the back-transformation.
    -   None of these variables showed a significant interaction with
        baseline BMI, indicating that the effect of mac treatment is not
        significantly different between BMI \<30 and \>=30.

<!-- -->

    ## $`log(CRPmgdL)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30   2.82     1.98     4.02              
    ##     Control     <30   3.14     2.20     4.47              
    ##  Mac - Ctrl     <30   0.90     0.65     1.24 0.5125       
    ##         Mac    >=30   6.58     4.58     9.46              
    ##     Control    >=30   7.66     5.33    11.01              
    ##  Mac - Ctrl    >=30   0.86     0.62     1.20 0.3621 0.8429
    ## 
    ## $`log(ESelectinngdL)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30  30.70    24.98    37.72              
    ##     Control     <30  31.23    25.41    38.37              
    ##  Mac - Ctrl     <30   0.98     0.91     1.06 0.6559       
    ##         Mac    >=30  40.82    33.04    50.44              
    ##     Control    >=30  41.20    33.35    50.91              
    ##  Mac - Ctrl    >=30   0.99     0.92     1.07 0.8114 0.8885
    ## 
    ## $`log(IL6pgmL)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30   0.43     0.33     0.56              
    ##     Control     <30   0.45     0.34     0.58              
    ##  Mac - Ctrl     <30   0.97     0.75     1.24 0.7949       
    ##         Mac    >=30   0.55     0.42     0.71              
    ##     Control    >=30   0.53     0.41     0.70              
    ##  Mac - Ctrl    >=30   1.03     0.79     1.32 0.8426 0.7467
    ## 
    ## $`log(TNFapgmL)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30   0.93     0.81     1.07              
    ##     Control     <30   0.94     0.82     1.08              
    ##  Mac - Ctrl     <30   0.99     0.90     1.08 0.8038       
    ##         Mac    >=30   1.05     0.91     1.21              
    ##     Control    >=30   1.07     0.93     1.24              
    ##  Mac - Ctrl    >=30   0.98     0.89     1.07 0.5959 0.8361
    ## 
    ## $`log(sICAM1pgmL)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30 382.74   339.74   431.18              
    ##     Control     <30 391.01   347.08   440.50              
    ##  Mac - Ctrl     <30   0.98     0.91     1.05 0.5298       
    ##         Mac    >=30 448.88   397.19   507.30              
    ##     Control    >=30 443.87   392.76   501.64              
    ##  Mac - Ctrl    >=30   1.01     0.94     1.09 0.7476 0.5048
    ## 
    ## $`log(sVCAm1pgmL)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30 413.06   359.54   474.54              
    ##     Control     <30 430.08   374.36   494.09              
    ##  Mac - Ctrl     <30   0.96     0.90     1.02 0.1780       
    ##         Mac    >=30 506.08   438.89   583.55              
    ##     Control    >=30 492.25   426.90   567.61              
    ##  Mac - Ctrl    >=30   1.03     0.97     1.09 0.3642 0.1156
    ## 
    ## $`log(isopgf2pgmL)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30 625.43   553.15   707.16              
    ##     Control     <30 613.28   542.40   693.42              
    ##  Mac - Ctrl     <30   1.02     0.88     1.19 0.7952       
    ##         Mac    >=30 676.46   596.33   767.36              
    ##     Control    >=30 640.11   564.29   726.12              
    ##  Mac - Ctrl    >=30   1.06     0.90     1.24 0.4783 0.7429
    ## 
    ## $`log(MDAnmolmL)`
    ##   Treatment BaseBMI emmean lower.CL upper.CL   pval intx.P
    ##         Mac     <30   1.90     1.51     2.37              
    ##     Control     <30   2.10     1.67     2.62              
    ##  Mac - Ctrl     <30   0.90     0.77     1.07 0.2204       
    ##         Mac    >=30   2.32     1.84     2.92              
    ##     Control    >=30   2.50     1.98     3.14              
    ##  Mac - Ctrl    >=30   0.93     0.78     1.10 0.3749 0.8218

### Examining the interaction with baseline WC

-   To examine if the effect of mac treatment may be different depending
    on baseline waist circumference (WC), a dichotomous WC variable
    (\<108 and \>=108) was added into the mixed model, along with its
    interaction with treatment.
-   Marginal means were estimated for each combination of treatment and
    baseline WC adjusted for sequence and phase, and then
    back-transformed into the original scale.
-   The last row of `Mac - Control` actually refers to the **ratio** of
    treatment means (mac/control) after the back-transformation.
    -   None of these variables showed a significant interaction with
        baseline WC, indicating that the effect of mac treatment is not
        significantly different between WC \<108 and \>=108.

<!-- -->

    ## $`log(CRPmgdL)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108   3.92     2.64     5.83              
    ##     Control   <108   4.10     2.75     6.09              
    ##  Mac - Ctrl   <108   0.96     0.70     1.31 0.7766       
    ##         Mac  >=108   4.71     3.05     7.27              
    ##     Control  >=108   5.92     3.84     9.13              
    ##  Mac - Ctrl  >=108   0.80     0.57     1.12 0.1829 0.4240
    ## 
    ## $`log(ESelectinngdL)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108  32.05    26.17    39.26              
    ##     Control   <108  31.44    25.67    38.51              
    ##  Mac - Ctrl   <108   1.02     0.95     1.10 0.5948       
    ##         Mac  >=108  39.53    31.67    49.33              
    ##     Control  >=108  41.65    33.37    51.98              
    ##  Mac - Ctrl  >=108   0.95     0.88     1.03 0.1921 0.1886
    ## 
    ## $`log(IL6pgmL)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108   0.47     0.36     0.60              
    ##     Control   <108   0.50     0.39     0.65              
    ##  Mac - Ctrl   <108   0.93     0.73     1.18 0.5545       
    ##         Mac  >=108   0.51     0.38     0.67              
    ##     Control  >=108   0.47     0.35     0.62              
    ##  Mac - Ctrl  >=108   1.08     0.83     1.40 0.5655 0.4125
    ## 
    ## $`log(TNFapgmL)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108   0.92     0.80     1.06              
    ##     Control   <108   0.95     0.83     1.09              
    ##  Mac - Ctrl   <108   0.97     0.89     1.06 0.5146       
    ##         Mac  >=108   1.06     0.92     1.24              
    ##     Control  >=108   1.07     0.92     1.24              
    ##  Mac - Ctrl  >=108   1.00     0.90     1.10 0.9204 0.7139
    ## 
    ## $`log(sICAM1pgmL)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108 397.98   353.81   447.67              
    ##     Control   <108 390.32   347.00   439.05              
    ##  Mac - Ctrl   <108   1.02     0.95     1.09 0.5511       
    ##         Mac  >=108 433.11   380.86   492.53              
    ##     Control  >=108 448.66   394.53   510.22              
    ##  Mac - Ctrl  >=108   0.97     0.90     1.04 0.3245 0.2615
    ## 
    ## $`log(sVCAm1pgmL)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108 432.71   376.57   497.22              
    ##     Control   <108 442.14   384.77   508.06              
    ##  Mac - Ctrl   <108   0.98     0.92     1.04 0.4683       
    ##         Mac  >=108 485.44   417.04   565.05              
    ##     Control  >=108 480.66   412.94   559.50              
    ##  Mac - Ctrl  >=108   1.01     0.95     1.08 0.7602 0.4759
    ## 
    ## $`log(isopgf2pgmL)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108 585.01   522.40   655.11              
    ##     Control   <108 607.52   542.51   680.31              
    ##  Mac - Ctrl   <108   0.96     0.83     1.11 0.5948       
    ##         Mac  >=108 736.55   650.86   833.52              
    ##     Control  >=108 649.26   573.72   734.74              
    ##  Mac - Ctrl  >=108   1.13     0.97     1.33 0.1102 0.1258
    ## 
    ## $`log(MDAnmolmL)`
    ##   Treatment BaseWC emmean lower.CL upper.CL   pval intx.P
    ##         Mac   <108   1.80     1.45     2.22              
    ##     Control   <108   2.04     1.65     2.52              
    ##  Mac - Ctrl   <108   0.88     0.75     1.03 0.1146       
    ##         Mac  >=108   2.50     1.98     3.15              
    ##     Control  >=108   2.61     2.07     3.29              
    ##  Mac - Ctrl  >=108   0.96     0.81     1.14 0.6200 0.4740

### Examining the interaction with baseline % body fat

-   To examine if the effect of mac treatment may be different depending
    on baseline % body fat (BF), a dichotomous BF variable (\<43 and
    \>=43) was added into the mixed model, along with its interaction
    with treatment.
-   Marginal means were estimated for each combination of treatment and
    baseline BF adjusted for sequence and phase, and then
    back-transformed into the original scale.
-   The last row of `Mac - Control` actually refers to the **ratio** of
    treatment means (mac/control) after the back-transformation.
    -   None of these variables showed a significant interaction with
        baseline BF, indicating that the effect of mac treatment is not
        significantly different between % body fat \<43 and \>=43.

<!-- -->

    ## $`log(CRPmgdL)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43   2.85     1.95     4.18              
    ##     Control    <43   3.47     2.40     5.01              
    ##  Mac - Ctrl    <43   0.82     0.57     1.18 0.2778       
    ##         Mac   >=43   5.97     4.21     8.46              
    ##     Control   >=43   6.64     4.64     9.50              
    ##  Mac - Ctrl   >=43   0.90     0.64     1.26 0.5265 0.7191
    ## 
    ## $`log(ESelectinngdL)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43  33.69    27.60    41.12              
    ##     Control    <43  34.40    28.32    41.78              
    ##  Mac - Ctrl    <43   0.98     0.90     1.06 0.6100       
    ##         Mac   >=43  36.65    30.41    44.17              
    ##     Control   >=43  37.06    30.64    44.82              
    ##  Mac - Ctrl   >=43   0.99     0.92     1.07 0.7760 0.8599
    ## 
    ## $`log(IL6pgmL)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43   0.45     0.34     0.59              
    ##     Control    <43   0.42     0.33     0.55              
    ##  Mac - Ctrl    <43   1.06     0.82     1.38 0.6485       
    ##         Mac   >=43   0.52     0.40     0.66              
    ##     Control   >=43   0.55     0.43     0.72              
    ##  Mac - Ctrl   >=43   0.93     0.73     1.19 0.5575 0.4658
    ## 
    ## $`log(TNFapgmL)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43   0.94     0.81     1.09              
    ##     Control    <43   0.95     0.83     1.10              
    ##  Mac - Ctrl    <43   0.99     0.90     1.08 0.7572       
    ##         Mac   >=43   1.03     0.90     1.17              
    ##     Control   >=43   1.05     0.92     1.21              
    ##  Mac - Ctrl   >=43   0.97     0.89     1.07 0.5644 0.8651
    ## 
    ## $`log(sICAM1pgmL)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43 403.56   357.15   456.00              
    ##     Control    <43 401.29   356.42   451.80              
    ##  Mac - Ctrl    <43   1.01     0.93     1.08 0.8773       
    ##         Mac   >=43 422.26   377.22   472.67              
    ##     Control   >=43 430.19   383.28   482.84              
    ##  Mac - Ctrl   >=43   0.98     0.92     1.05 0.5895 0.6315
    ## 
    ## $`log(sVCAm1pgmL)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43 425.76   372.55   486.57              
    ##     Control    <43 430.61   378.19   490.30              
    ##  Mac - Ctrl    <43   0.99     0.92     1.06 0.7346       
    ##         Mac   >=43 482.98   426.69   546.69              
    ##     Control   >=43 488.10   430.04   554.01              
    ##  Mac - Ctrl   >=43   0.99     0.93     1.05 0.7371 0.9866
    ## 
    ## $`log(isopgf2pgmL)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43 650.84   571.02   741.82              
    ##     Control    <43 651.16   573.74   739.04              
    ##  Mac - Ctrl    <43   1.00     0.85     1.17 0.9949       
    ##         Mac   >=43 648.89   575.67   731.43              
    ##     Control   >=43 603.54   533.73   682.48              
    ##  Mac - Ctrl   >=43   1.08     0.93     1.25 0.3291 0.4998
    ## 
    ## $`log(MDAnmolmL)`
    ##   Treatment BaseBF emmean lower.CL upper.CL   pval intx.P
    ##         Mac    <43   2.08     1.65     2.64              
    ##     Control    <43   2.44     1.94     3.07              
    ##  Mac - Ctrl    <43   0.85     0.72     1.01 0.0615       
    ##         Mac   >=43   2.10     1.69     2.60              
    ##     Control   >=43   2.14     1.71     2.67              
    ##  Mac - Ctrl   >=43   0.98     0.84     1.15 0.7920 0.2306
