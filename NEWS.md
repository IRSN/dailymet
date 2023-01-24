---
   title: "**dailymet** Package News"
---

# News in version 0.1.1

## Enhancements

- The `predict` method for the class `"PgpTList"` now accepts a
  `newdata` argument which can be simply a vector with class `"Date"`.

- New method `simulate` for the class `"PgpTList"`. It has a `newdata`
  argument similar to that of the `predict` method.

- The `predict` and the `simulate` methods return objects having the
  S3 classes `"predict.pgpTList"` and `"simulate.pgpTList"` for which
  some methds are available, such as `autoplot`.
