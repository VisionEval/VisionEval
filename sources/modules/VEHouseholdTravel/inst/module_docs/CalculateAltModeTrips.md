
# CalculateAltModeTrips Module
### November 23, 2018

This module calculates household transit trips, walk trips, and bike trips. The models are sensitive to household DVMT so they are run after all household DVMT adjustments (e.g. to account for cost on household DVMT) are made.

## Model Parameter Estimation

Hurdle models are estimated for calculating the numbers of household transit, walk, and bike trips using the [pscl](https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf) package. Separate models are calculated for metropolitan and non-metropolitan households to account for the additional variables available in metropolitan areas.

Following are the estimation statistics for the metropolitan and nonmetropolitan **walk** trip models.

**Metropolitan Walk Trip Model**
```

Call:
hurdle(formula = ModelFormula, data = Data_df, dist = "poisson", zero.dist = "binomial", 
    link = "logit")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-4.7404 -1.3364 -0.5991  0.5860 32.2119 

Count model coefficients (truncated poisson with log link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)   4.586e+00  6.697e-03  684.79   <2e-16 ***
HhSize        3.172e-01  8.370e-04  378.98   <2e-16 ***
LogIncome     1.419e-01  6.561e-04  216.19   <2e-16 ***
LogDensity   -3.934e-03  3.344e-04  -11.76   <2e-16 ***
BusEqRevMiPC  1.630e-03  1.302e-05  125.19   <2e-16 ***
Urban         4.585e-02  6.120e-04   74.92   <2e-16 ***
LogDvmt      -2.189e-01  7.180e-04 -304.82   <2e-16 ***
Age0to14     -3.257e-01  9.079e-04 -358.75   <2e-16 ***
Age15to19    -8.750e-02  1.099e-03  -79.64   <2e-16 ***
Age20to29     4.693e-02  9.317e-04   50.37   <2e-16 ***
Age30to54     2.095e-02  7.154e-04   29.28   <2e-16 ***
Age65Plus    -3.509e-02  8.485e-04  -41.35   <2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -2.2804393  0.2614324  -8.723  < 2e-16 ***
HhSize        0.4598264  0.0385440  11.930  < 2e-16 ***
LogIncome     0.2799329  0.0260141  10.761  < 2e-16 ***
LogDensity    0.0232094  0.0137806   1.684 0.092142 .  
BusEqRevMiPC -0.0038171  0.0005494  -6.948 3.71e-12 ***
Urban         0.0643700  0.0260836   2.468 0.013593 *  
LogDvmt      -0.2562672  0.0313806  -8.166 3.18e-16 ***
Age0to14     -0.3719179  0.0422678  -8.799  < 2e-16 ***
Age15to19    -0.1964975  0.0555859  -3.535 0.000408 ***
Age20to29     0.0930435  0.0428988   2.169 0.030090 *  
Age30to54     0.0649368  0.0309904   2.095 0.036137 *  
Age65Plus    -0.0374239  0.0344160  -1.087 0.276861    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Number of iterations in BFGS optimization: 18 
Log-likelihood: -2.475e+06 on 24 Df
```

**Nonmetropolitan Walk Trip Model**
```

Call:
hurdle(formula = ModelFormula, data = Data_df, dist = "poisson", zero.dist = "binomial", 
    link = "logit")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-2.9713 -1.2632 -0.5841  0.5359 34.5831 

Count model coefficients (truncated poisson with log link):
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  6.1532453  0.0044802 1373.45   <2e-16 ***
HhSize       0.3303483  0.0006748  489.54   <2e-16 ***
LogIncome   -0.0237607  0.0005589  -42.51   <2e-16 ***
LogDensity  -0.0377237  0.0001842 -204.76   <2e-16 ***
LogDvmt     -0.0414981  0.0010329  -40.17   <2e-16 ***
Age0to14    -0.3605797  0.0007033 -512.69   <2e-16 ***
Age15to19   -0.1465391  0.0008401 -174.43   <2e-16 ***
Age20to29    0.0241638  0.0006777   35.66   <2e-16 ***
Age30to54   -0.0190164  0.0005360  -35.48   <2e-16 ***
Age65Plus   -0.0301695  0.0006138  -49.15   <2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.498259   0.159756  -3.119  0.00182 ** 
HhSize       0.144868   0.029151   4.969 6.71e-07 ***
LogIncome    0.057786   0.019820   2.916  0.00355 ** 
LogDensity  -0.034457   0.006878  -5.010 5.45e-07 ***
LogDvmt      0.118384   0.036099   3.279  0.00104 ** 
Age0to14    -0.190407   0.030042  -6.338 2.33e-10 ***
Age15to19    0.021314   0.038479   0.554  0.57965    
Age20to29    0.092761   0.028744   3.227  0.00125 ** 
Age30to54    0.064027   0.021311   3.004  0.00266 ** 
Age65Plus   -0.049842   0.023212  -2.147  0.03177 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Number of iterations in BFGS optimization: 16 
Log-likelihood: -4.195e+06 on 20 Df
```

Following are the estimation statistics for the metropolitan and nonmetropolitan **bike** trip models.

**Metropolitan Bike Trip Model**
```

Call:
hurdle(formula = ModelFormula, data = Data_df, dist = "poisson", zero.dist = "binomial", 
    link = "logit")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-1.2356 -0.3524 -0.2790 -0.2282 34.1390 

Count model coefficients (truncated poisson with log link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)   6.3291295  0.0247412  255.81   <2e-16 ***
HhSize        0.1106682  0.0042717   25.91   <2e-16 ***
LogIncome    -0.0673258  0.0029047  -23.18   <2e-16 ***
BusEqRevMiPC -0.0022411  0.0000602  -37.23   <2e-16 ***
LogDvmt      -0.1701043  0.0033316  -51.06   <2e-16 ***
Age0to14     -0.1938577  0.0044805  -43.27   <2e-16 ***
Age15to19    -0.1357341  0.0052914  -25.65   <2e-16 ***
Age20to29     0.0788564  0.0044252   17.82   <2e-16 ***
Age30to54     0.0788955  0.0036176   21.81   <2e-16 ***
Age65Plus     0.0490975  0.0043174   11.37   <2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -4.772470   0.380270 -12.550  < 2e-16 ***
HhSize        0.166048   0.058051   2.860 0.004232 ** 
LogIncome     0.200809   0.043180   4.650 3.31e-06 ***
BusEqRevMiPC -0.006162   0.000865  -7.123 1.05e-12 ***
LogDvmt      -0.040378   0.049599  -0.814 0.415595    
Age0to14      0.045109   0.059956   0.752 0.451827    
Age15to19     0.207085   0.071723   2.887 0.003886 ** 
Age20to29     0.230143   0.061426   3.747 0.000179 ***
Age30to54     0.171313   0.048320   3.545 0.000392 ***
Age65Plus    -0.075679   0.061373  -1.233 0.217540    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Number of iterations in BFGS optimization: 39 
Log-likelihood: -1.193e+05 on 20 Df
```

**Nonmetropolitan Bike Trip Model**
```

Call:
hurdle(formula = ModelFormula, data = Data_df, dist = "poisson", zero.dist = "binomial", 
    link = "logit")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-2.4409 -0.3567 -0.2792 -0.2273 57.2806 

Count model coefficients (truncated poisson with log link):
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  5.890211   0.017451  337.54   <2e-16 ***
HhSize       0.242486   0.002576   94.15   <2e-16 ***
LogIncome    0.033460   0.002126   15.74   <2e-16 ***
LogDvmt     -0.386822   0.003250 -119.02   <2e-16 ***
Age0to14    -0.289379   0.002742 -105.54   <2e-16 ***
Age15to19   -0.092259   0.003160  -29.20   <2e-16 ***
Age20to29    0.095861   0.002578   37.18   <2e-16 ***
Age30to54    0.024639   0.002280   10.81   <2e-16 ***
Age65Plus   -0.033766   0.002863  -11.79   <2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -4.62329    0.27759 -16.655  < 2e-16 ***
HhSize       0.21577    0.04552   4.740 2.14e-06 ***
LogIncome    0.19358    0.03288   5.888 3.90e-09 ***
LogDvmt     -0.12137    0.05761  -2.107 0.035130 *  
Age0to14    -0.05064    0.04528  -1.118 0.263437    
Age15to19    0.18243    0.05279   3.456 0.000548 ***
Age20to29    0.25783    0.04318   5.971 2.36e-09 ***
Age30to54    0.17484    0.03476   5.031 4.89e-07 ***
Age65Plus   -0.18267    0.04441  -4.113 3.90e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Number of iterations in BFGS optimization: 18 
Log-likelihood: -2.874e+05 on 18 Df
```

Following are the estimation statistics for the metropolitan and nonmetropolitan **transit** trip models.

**Metropolitan Transit Trip Model**
```

Call:
hurdle(formula = ModelFormula, data = Data_df, dist = "poisson", zero.dist = "binomial", 
    link = "logit")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-3.9020 -0.3420 -0.2262 -0.1478 34.7487 

Count model coefficients (truncated poisson with log link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)   6.028e+00  1.043e-02 577.719  < 2e-16 ***
HhSize        1.347e-02  6.685e-04  20.153  < 2e-16 ***
LogIncome     5.802e-02  1.001e-03  57.970  < 2e-16 ***
LogDensity    4.494e-02  5.880e-04  76.432  < 2e-16 ***
BusEqRevMiPC  1.886e-03  2.542e-05  74.193  < 2e-16 ***
LogDvmt      -9.595e-02  1.006e-03 -95.365  < 2e-16 ***
Urban         3.455e-02  1.076e-03  32.118  < 2e-16 ***
Age15to19    -1.227e-03  1.207e-03  -1.017    0.309    
Age20to29     6.579e-02  1.333e-03  49.369  < 2e-16 ***
Age30to54     4.877e-02  1.262e-03  38.658  < 2e-16 ***
Age65Plus     8.669e-03  1.662e-03   5.214 1.84e-07 ***
Zero hurdle model coefficients (binomial with logit link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -4.1806957  0.4118196 -10.152  < 2e-16 ***
HhSize        0.5182683  0.0245849  21.081  < 2e-16 ***
LogIncome     0.3450214  0.0403773   8.545  < 2e-16 ***
LogDensity   -0.0396004  0.0214168  -1.849 0.064452 .  
BusEqRevMiPC  0.0097507  0.0008816  11.060  < 2e-16 ***
LogDvmt      -1.0665989  0.0416937 -25.582  < 2e-16 ***
Urban         0.0715848  0.0400024   1.790 0.073532 .  
Age15to19     0.3070926  0.0470241   6.531 6.55e-11 ***
Age20to29     0.1944264  0.0521244   3.730 0.000191 ***
Age30to54     0.3877417  0.0466772   8.307  < 2e-16 ***
Age65Plus    -0.5823975  0.0644612  -9.035  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Number of iterations in BFGS optimization: 21 
Log-likelihood: -3.382e+05 on 22 Df
```

**Nonmetropolitan Transit Trip Model**
```

Call:
hurdle(formula = ModelFormula, data = Data_df, dist = "poisson", zero.dist = "binomial", 
    link = "logit")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-6.3336 -0.2401 -0.1567 -0.1049 45.2277 

Count model coefficients (truncated poisson with log link):
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  6.7691955  0.0105618 640.913  < 2e-16 ***
HhSize       0.0532313  0.0018395  28.937  < 2e-16 ***
LogIncome    0.0588446  0.0012726  46.240  < 2e-16 ***
LogDensity  -0.0124246  0.0004656 -26.683  < 2e-16 ***
LogDvmt     -0.1352185  0.0019875 -68.034  < 2e-16 ***
Age0to14    -0.0083880  0.0018218  -4.604 4.14e-06 ***
Age15to19    0.0236042  0.0020681  11.413  < 2e-16 ***
Age20to29   -0.0322947  0.0021757 -14.843  < 2e-16 ***
Age30to54   -0.0274391  0.0017202 -15.951  < 2e-16 ***
Age65Plus   -0.0709628  0.0027659 -25.656  < 2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.44590    0.35061  -4.124 3.72e-05 ***
HhSize       0.49018    0.06451   7.598 3.00e-14 ***
LogIncome    0.23243    0.04346   5.349 8.86e-08 ***
LogDensity  -0.17545    0.01442 -12.167  < 2e-16 ***
LogDvmt     -1.27415    0.06932 -18.380  < 2e-16 ***
Age0to14     0.25511    0.06337   4.026 5.68e-05 ***
Age15to19    0.38536    0.07126   5.407 6.39e-08 ***
Age20to29    0.07015    0.07214   0.972    0.331    
Age30to54    0.53278    0.05692   9.360  < 2e-16 ***
Age65Plus   -0.60592    0.08592  -7.052 1.77e-12 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Number of iterations in BFGS optimization: 21 
Log-likelihood: -2.818e+05 on 20 Df
```

## How the Module Works

This module is run after all household DVMT adjustments are made due to cost, travel demand management, and light-weight vehicle (e.g. bike, scooter) diversion, so that alternative mode travel reflects the result of those influences. The alternative mode trip models are run and the results are saved.


## User Inputs
This module has no user input requirements.

## Datasets Used by the Module
The following table documents each dataset that is retrieved from the datastore and used by the module. Each row in the table describes a dataset. All the datasets must be present in the datastore. One or more of these datasets may be entered into the datastore from the user input files. The table names and their meanings are as follows:

NAME - The dataset name.

TABLE - The table in the datastore that the data is retrieved from.

GROUP - The group in the datastore where the table is located. Note that the datastore has a group named 'Global' and groups for every model run year. For example, if the model run years are 2010 and 2050, then the datastore will have a group named '2010' and a group named '2050'. If the value for 'GROUP' is 'Year', then the dataset will exist in each model run year group. If the value for 'GROUP' is 'BaseYear' then the dataset will only exist in the base year group (e.g. '2010'). If the value for 'GROUP' is 'Global' then the dataset will only exist in the 'Global' group.

TYPE - The data type. The framework uses the type to check units and inputs. Refer to the model system design and users guide for information on allowed types.

UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.

PROHIBIT - Values that are prohibited. Values in the datastore do not meet any of the listed conditions.

ISELEMENTOF - Categorical values that are permitted. Values in the datastore are one or more of the listed values.

|NAME            |TABLE     |GROUP |TYPE      |UNITS      |PROHIBIT |ISELEMENTOF        |
|:---------------|:---------|:-----|:---------|:----------|:--------|:------------------|
|Marea           |Marea     |Year  |character |ID         |         |                   |
|TranRevMiPC     |Marea     |Year  |compound  |MI/PRSN/YR |NA, < 0  |                   |
|Marea           |Bzone     |Year  |character |ID         |         |                   |
|Bzone           |Bzone     |Year  |character |ID         |         |                   |
|D1B             |Bzone     |Year  |compound  |PRSN/SQMI  |NA, < 0  |                   |
|Marea           |Household |Year  |character |ID         |         |                   |
|Bzone           |Household |Year  |character |ID         |         |                   |
|Age0to14        |Household |Year  |people    |PRSN       |NA, < 0  |                   |
|Age15to19       |Household |Year  |people    |PRSN       |NA, < 0  |                   |
|Age20to29       |Household |Year  |people    |PRSN       |NA, < 0  |                   |
|Age30to54       |Household |Year  |people    |PRSN       |NA, < 0  |                   |
|Age55to64       |Household |Year  |people    |PRSN       |NA, < 0  |                   |
|Age65Plus       |Household |Year  |people    |PRSN       |NA, < 0  |                   |
|LocType         |Household |Year  |character |category   |NA       |Urban, Town, Rural |
|HhSize          |Household |Year  |people    |PRSN       |NA, <= 0 |                   |
|Income          |Household |Year  |currency  |USD.2001   |NA, < 0  |                   |
|Vehicles        |Household |Year  |vehicles  |VEH        |NA, < 0  |                   |
|IsUrbanMixNbrhd |Household |Year  |integer   |binary     |NA       |0, 1               |
|Dvmt            |Household |Year  |compound  |MI/DAY     |NA, < 0  |                   |

## Datasets Produced by the Module
The following table documents each dataset that is retrieved from the datastore and used by the module. Each row in the table describes a dataset. All the datasets must be present in the datastore. One or more of these datasets may be entered into the datastore from the user input files. The table names and their meanings are as follows:

NAME - The dataset name.

TABLE - The table in the datastore that the data is retrieved from.

GROUP - The group in the datastore where the table is located. Note that the datastore has a group named 'Global' and groups for every model run year. For example, if the model run years are 2010 and 2050, then the datastore will have a group named '2010' and a group named '2050'. If the value for 'GROUP' is 'Year', then the dataset will exist in each model run year. If the value for 'GROUP' is 'BaseYear' then the dataset will only exist in the base year group (e.g. '2010'). If the value for 'GROUP' is 'Global' then the dataset will only exist in the 'Global' group.

TYPE - The data type. The framework uses the type to check units and inputs. Refer to the model system design and users guide for information on allowed types.

UNITS - The units that input values need to represent. Some data types have defined units that are represented as abbreviations or combinations of abbreviations. For example 'MI/HR' means miles per hour. Many of these abbreviations are self evident, but the VisionEval model system design and users guide should be consulted.

PROHIBIT - Values that are prohibited. Values in the datastore do not meet any of the listed conditions.

ISELEMENTOF - Categorical values that are permitted. Values in the datastore are one or more of the listed values.

DESCRIPTION - A description of the data.

|NAME         |TABLE     |GROUP |TYPE     |UNITS   |PROHIBIT |ISELEMENTOF |DESCRIPTION                                                          |
|:------------|:---------|:-----|:--------|:-------|:--------|:-----------|:--------------------------------------------------------------------|
|WalkTrips    |Household |Year  |compound |TRIP/YR |NA, < 0  |            |Average number of walk trips per year by household members           |
|BikeTrips    |Household |Year  |compound |TRIP/YR |NA, < 0  |            |Average number of bicycle trips per year by household members        |
|TransitTrips |Household |Year  |compound |TRIP/YR |NA, < 0  |            |Average number of public transit trips per year by household members |
