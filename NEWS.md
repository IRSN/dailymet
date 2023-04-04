
**dailymet** Package News
===========================

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
