
**dailymet** Package News
===========================

# New in version 0.1.6

## Bug fixes

- The arguments `newdata` and `prob` of the `quantMax` method for the 
  class `"pgpTList"` were not used.

# New in version 0.1.5

## Enhancements
- In both in `rqTList` and `pgpTList` it is now possible to use quite
  arbitrary "design" functions to create variables entering in the
  formula(s) defining the model objects. The `designVars` function has
  been added to simplify the managment of the variables in fitting,
  prediction and simulation steps.
  
- A new generic `quantMax` and the corresponding method for the
  `"pgpTList"` class have been added. This method should be used in
  place of `quantile` to avoid possible confusions between the
  quantile of the maximum and the marginal quantiles.


# New in version 0.1.4

## Enhancements

- The `quantile` method for the class `"pgpTList"` now computes the
  confidence intevals by the "delta method". The quantiles and
  confidence intervals are computed for a quite large number of
  probabilities in order to build nice-looking plots.

- For the objects with class `"quantile.pgpTList"` as created when
  applying the `quantile` method, there now exist an `autoplot` method
  and a `format` method. The first builds a ggplot, the second makes a
  table with suitably rounded quantiles and selecting round
  probabilities as is often needed in reports.

- The `Intro` vignette has been enhanced to illustrate the use of the
  new methods. The secion *Investigating the distribution of the
  maximum* should be instructive for the users.

# News in version 0.1.3

## Bug fixes

- In the ` predict.pgpTList` there was an error in the determination
  of the maximum of the thresholds. This could have an impact on the
  result of the `quantile` method although it seems very small in
  practice.

## Enhancements

- The `simulate.pgpTList` can now be used to compute the quantiles of 
  the maximum `M` over the prediction/simulation period.


# News in version 0.1.2

## Enhancements

- The `predict` method for the class `"PgpTList"` provides new
  variables on output.
  
- The new method `quantile` for the classes `"PgpTList"` and
  `"predict.PgpTList"` allows to investigate the tail distribution of
  the maximum on a "new" period of interest.

# News in version 0.1.1

## Enhancements

- The `predict` method for the class `"PgpTList"` now accepts a
  `newdata` argument which can be simply a vector with class `"Date"`.

- New method `simulate` for the class `"PgpTList"`. It has a `newdata`
  argument similar to that of the `predict` method.

- The `predict` and the `simulate` methods return objects having the
  S3 classes `"predict.pgpTList"` and `"simulate.pgpTList"` for which
  some methds are available, such as `autoplot`.

- The package no longer depends on **NSGEV** and **potomax** from
  which it imported the GEV and the GPD2 distributions
  respectively. These distributions are now imported from the
  **nieve** package.
