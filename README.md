dailymet README
================
Yves Deville <deville.yves@alpestat.com>

# Goals and scope

**dailymet** is a (maybe temporary) R package providing tools than can
help when studying the extremes of daily meteorological timeseries,
mainly series of daily maximum temperature `TX`. It puts some emphasis
on Peaks Over Threshold (POT) models, and more specifically on
time-varying POT models in which basis functions are used to describe
the yearly seasonality and the trend. The package scope is as follows.

- Read timeseries from `.csv` files, produce summaries and ggplots.

- Provide supplementary S3 methods for the objects `rq` from the
  **quantreg** package and `fevd` from the **extRemes** package.

- Provide bases of functions that can be used to describe the trend and
  the seasonality. Among these, a basis of sine wave functions with
  prescribed phases is useful to get a precise description of the yearly
  seasonality with a reduced number of parameters hence with a easier
  estimation.

- Provide the new S3 classes `"rqTList"` and `"fevdTList"` representing
  lists of `rq` or `fevd`objects “by threshold” i.e., differing only by
  their threshold. These classes are helpful to assess the sensitivity
  of the results to the threshold choice, which generally remains highly
  subective.

For now, the package focuses on univariate approaches in which the
analysis is for only *one* meteorological timeseries, often assumed to
be the `TX`.

# News

See the
[NEWS.md](https://github.com/yvesdeville/dailymet/blob/main/NEWS.md)
file

# Install release version from GitHub

## Using the *remotes* package

Provided that the **remotes** package is installed, the installation of
**dailymet** from github can be done by using

``` r
remotes::install_github("yvesdeville/dailymet", dependencies = TRUE, auth_token = myToken)
```

where `myToken` stands for *your* token. This should install the package
and make it ready to use.

You can also select a specific branch or a specific commit by using the
suitable syntax for `install_github`, see the **remotes** package
documentation.

## Clone, build and install

### Cloning the repository

If you do not have yet a local `dailymet` repository, use `git clone` to
clone the `dailymet` repository

``` bash
git clone https://github.com/yvesdeville/dailymet
```

This will create a `dailymet` sub-directory of the current directory,
i.e. the directory from which the git command was issued. Of course this
can work only if you have the authorisation to clone.
