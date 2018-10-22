# IntensityDiff
An R package to perform the intensity difference statistical calculation.

![Diagnostic screenshot](https://raw.githubusercontent.com/s-andrews/intensitydiff/master/man/diagnostic_plots.png)

The intensity difference test is a 'magnitude of effect' test operated over a large set of unreplicated paired data values.  The constraint for the test is that the average difference between the two sets of values should be zero, and that the noise in the difference should be related to the magnitude of the mean of the two values.  This constraint applies to many count based datasets but can potentially apply in a large number of datatypes.

## Installation
```
install.packages("devtools")
library(devtools)
install_github("s-andrews/intensitydiff")
```

## Usage
```
library(intensitydiff)

intensity.difference(vector.1,vector.2) -> results

intensity.diff.plot(results)
```

## Calculation
The process for the calculation is as follows.

For each point in the dataset a statistic is calculated by:

1. Selecting a local subset of the data.  By default 1% of the data is modelled for each test (this can be modified with the window.proportion argument.  The points are selected on the basis of having the most similar mean values to the point being tested.

2. From the subset of points the difference values between the two datasets are extracted.

3. The standard deviation for these points is calculated, assuming a forced mean difference of zero.

4. The z-score for the difference for the point being tested is calculated.

5. The probability of selecting a point with a difference greater than the observed difference given calculated mean and standard deviation is calculated.
   
6. Once the statistics have been calculated for each point in the set then a corrcted FDR value for each point is also calculated.


