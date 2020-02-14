
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
-4.7373 -1.3362 -0.5994  0.5859 32.2096 

Count model coefficients (truncated poisson with log link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)   4.588e+00  6.696e-03  685.10   <2e-16 ***
HhSize        3.171e-01  8.368e-04  378.95   <2e-16 ***
LogIncome     1.417e-01  6.557e-04  216.19   <2e-16 ***
LogDensity   -4.032e-03  3.344e-04  -12.06   <2e-16 ***
BusEqRevMiPC  1.632e-03  1.302e-05  125.36   <2e-16 ***
Urban         4.580e-02  6.120e-04   74.84   <2e-16 ***
LogDvmt      -2.187e-01  7.165e-04 -305.19   <2e-16 ***
Age0to14     -3.256e-01  9.078e-04 -358.64   <2e-16 ***
Age15to19    -8.747e-02  1.099e-03  -79.61   <2e-16 ***
Age20to29     4.688e-02  9.317e-04   50.31   <2e-16 ***
Age30to54     2.091e-02  7.154e-04   29.23   <2e-16 ***
Age65Plus    -3.504e-02  8.485e-04  -41.30   <2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -2.2779405  0.2614106  -8.714  < 2e-16 ***
HhSize        0.4593582  0.0385316  11.922  < 2e-16 ***
LogIncome     0.2794224  0.0259941  10.749  < 2e-16 ***
LogDensity    0.0232475  0.0137835   1.687 0.091678 .  
BusEqRevMiPC -0.0038107  0.0005493  -6.938 3.98e-12 ***
Urban         0.0643767  0.0260840   2.468 0.013585 *  
LogDvmt      -0.2552992  0.0313200  -8.151 3.60e-16 ***
Age0to14     -0.3715079  0.0422595  -8.791  < 2e-16 ***
Age15to19    -0.1962355  0.0555825  -3.531 0.000415 ***
Age20to29     0.0929654  0.0428981   2.167 0.030226 *  
Age30to54     0.0648896  0.0309901   2.094 0.036271 *  
Age65Plus    -0.0372897  0.0344148  -1.084 0.278571    
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
-2.9708 -1.2632 -0.5842  0.5359 34.5821 

Count model coefficients (truncated poisson with log link):
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  6.1534005  0.0044802 1373.47   <2e-16 ***
HhSize       0.3302854  0.0006750  489.34   <2e-16 ***
LogIncome   -0.0238485  0.0005589  -42.67   <2e-16 ***
LogDensity  -0.0377050  0.0001842 -204.67   <2e-16 ***
LogDvmt     -0.0412971  0.0010340  -39.94   <2e-16 ***
Age0to14    -0.3605450  0.0007034 -512.59   <2e-16 ***
Age15to19   -0.1465219  0.0008402 -174.40   <2e-16 ***
Age20to29    0.0241464  0.0006777   35.63   <2e-16 ***
Age30to54   -0.0190250  0.0005360  -35.49   <2e-16 ***
Age65Plus   -0.0301608  0.0006138  -49.14   <2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.498728   0.159757  -3.122  0.00180 ** 
HhSize       0.144693   0.029160   4.962 6.98e-07 ***
LogIncome    0.057696   0.019819   2.911  0.00360 ** 
LogDensity  -0.034436   0.006878  -5.007 5.53e-07 ***
LogDvmt      0.118819   0.036144   3.287  0.00101 ** 
Age0to14    -0.190284   0.030047  -6.333 2.40e-10 ***
Age15to19    0.021423   0.038482   0.557  0.57774    
Age20to29    0.092751   0.028744   3.227  0.00125 ** 
Age30to54    0.064009   0.021311   3.004  0.00267 ** 
Age65Plus   -0.049782   0.023213  -2.145  0.03199 *  
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
-1.2354 -0.3524 -0.2790 -0.2282 34.1229 

Count model coefficients (truncated poisson with log link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)   6.330e+00  2.473e-02  255.94   <2e-16 ***
HhSize        1.105e-01  4.271e-03   25.88   <2e-16 ***
LogIncome    -6.754e-02  2.903e-03  -23.27   <2e-16 ***
BusEqRevMiPC -2.240e-03  6.019e-05  -37.22   <2e-16 ***
LogDvmt      -1.697e-01  3.325e-03  -51.03   <2e-16 ***
Age0to14     -1.936e-01  4.480e-03  -43.23   <2e-16 ***
Age15to19    -1.357e-01  5.291e-03  -25.64   <2e-16 ***
Age20to29     7.879e-02  4.425e-03   17.80   <2e-16 ***
Age30to54     7.896e-02  3.618e-03   21.83   <2e-16 ***
Age65Plus     4.916e-02  4.317e-03   11.38   <2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -4.7713578  0.3801813 -12.550  < 2e-16 ***
HhSize        0.1658710  0.0580388   2.858 0.004264 ** 
LogIncome     0.2005757  0.0431518   4.648 3.35e-06 ***
BusEqRevMiPC -0.0061592  0.0008648  -7.122 1.06e-12 ***
LogDvmt      -0.0399244  0.0495123  -0.806 0.420040    
Age0to14      0.0452386  0.0599474   0.755 0.450466    
Age15to19     0.2071779  0.0717209   2.889 0.003869 ** 
Age20to29     0.2301316  0.0614262   3.746 0.000179 ***
Age30to54     0.1712935  0.0483191   3.545 0.000393 ***
Age65Plus    -0.0756286  0.0613717  -1.232 0.217836    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Number of iterations in BFGS optimization: 36 
Log-likelihood: -1.193e+05 on 20 Df
```

**Nonmetropolitan Bike Trip Model**
```

Call:
hurdle(formula = ModelFormula, data = Data_df, dist = "poisson", zero.dist = "binomial", 
    link = "logit")

Pearson residuals:
    Min      1Q  Median      3Q     Max 
-2.4409 -0.3567 -0.2792 -0.2273 57.2919 

Count model coefficients (truncated poisson with log link):
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  5.892104   0.017447  337.71   <2e-16 ***
HhSize       0.242550   0.002576   94.17   <2e-16 ***
LogIncome    0.033409   0.002126   15.71   <2e-16 ***
LogDvmt     -0.387245   0.003255 -118.98   <2e-16 ***
Age0to14    -0.289406   0.002742 -105.55   <2e-16 ***
Age15to19   -0.092299   0.003159  -29.21   <2e-16 ***
Age20to29    0.095888   0.002578   37.19   <2e-16 ***
Age30to54    0.024688   0.002280   10.83   <2e-16 ***
Age65Plus   -0.033849   0.002863  -11.82   <2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -4.62279    0.27757 -16.654  < 2e-16 ***
HhSize       0.21575    0.04553   4.738 2.15e-06 ***
LogIncome    0.19353    0.03287   5.887 3.93e-09 ***
LogDvmt     -0.12136    0.05767  -2.104 0.035354 *  
Age0to14    -0.05063    0.04528  -1.118 0.263517    
Age15to19    0.18242    0.05279   3.456 0.000549 ***
Age20to29    0.25782    0.04318   5.971 2.36e-09 ***
Age30to54    0.17485    0.03476   5.031 4.89e-07 ***
Age65Plus   -0.18268    0.04441  -4.114 3.90e-05 ***
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
-3.8998 -0.3420 -0.2262 -0.1478 34.7586 

Count model coefficients (truncated poisson with log link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)   6.029e+00  1.043e-02 577.932   <2e-16 ***
HhSize        1.347e-02  6.685e-04  20.148   <2e-16 ***
LogIncome     5.790e-02  1.000e-03  57.896   <2e-16 ***
LogDensity    4.491e-02  5.881e-04  76.364   <2e-16 ***
BusEqRevMiPC  1.888e-03  2.542e-05  74.259   <2e-16 ***
LogDvmt      -9.576e-02  1.004e-03 -95.366   <2e-16 ***
Urban         3.454e-02  1.076e-03  32.110   <2e-16 ***
Age15to19    -1.256e-03  1.207e-03  -1.041    0.298    
Age20to29     6.571e-02  1.333e-03  49.312   <2e-16 ***
Age30to54     4.872e-02  1.262e-03  38.620   <2e-16 ***
Age65Plus     8.644e-03  1.662e-03   5.200    2e-07 ***
Zero hurdle model coefficients (binomial with logit link):
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -4.1704957  0.4117472 -10.129  < 2e-16 ***
HhSize        0.5182818  0.0245849  21.081  < 2e-16 ***
LogIncome     0.3439376  0.0403431   8.525  < 2e-16 ***
LogDensity   -0.0399763  0.0214212  -1.866 0.062014 .  
BusEqRevMiPC  0.0097638  0.0008815  11.077  < 2e-16 ***
LogDvmt      -1.0649137  0.0416130 -25.591  < 2e-16 ***
Urban         0.0713825  0.0400027   1.784 0.074352 .  
Age15to19     0.3066829  0.0470226   6.522 6.94e-11 ***
Age20to29     0.1935694  0.0521167   3.714 0.000204 ***
Age30to54     0.3872192  0.0466700   8.297  < 2e-16 ***
Age65Plus    -0.5825307  0.0644610  -9.037  < 2e-16 ***
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
-6.3324 -0.2401 -0.1568 -0.1048 45.1873 

Count model coefficients (truncated poisson with log link):
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  6.7699170  0.0105608  641.04  < 2e-16 ***
HhSize       0.0532249  0.0018397   28.93  < 2e-16 ***
LogIncome    0.0588111  0.0012727   46.21  < 2e-16 ***
LogDensity  -0.0124144  0.0004657  -26.66  < 2e-16 ***
LogDvmt     -0.1353373  0.0019911  -67.97  < 2e-16 ***
Age0to14    -0.0083800  0.0018218   -4.60 4.23e-06 ***
Age15to19    0.0236052  0.0020681   11.41  < 2e-16 ***
Age20to29   -0.0322833  0.0021758  -14.84  < 2e-16 ***
Age30to54   -0.0274216  0.0017203  -15.94  < 2e-16 ***
Age65Plus   -0.0709431  0.0027659  -25.65  < 2e-16 ***
Zero hurdle model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.44021    0.35057  -4.108 3.99e-05 ***
HhSize       0.49047    0.06451   7.603 2.90e-14 ***
LogIncome    0.23233    0.04346   5.346 8.99e-08 ***
LogDensity  -0.17540    0.01442 -12.164  < 2e-16 ***
LogDvmt     -1.27574    0.06945 -18.370  < 2e-16 ***
Age0to14     0.25493    0.06336   4.023 5.74e-05 ***
Age15to19    0.38514    0.07126   5.405 6.49e-08 ***
Age20to29    0.07020    0.07214   0.973    0.331    
Age30to54    0.53298    0.05692   9.363  < 2e-16 ***
Age65Plus   -0.60607    0.08592  -7.054 1.74e-12 ***
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
