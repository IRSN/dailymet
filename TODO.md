TODO list for the **dailymet** package
================
Yves Deville <deville.yves@alpestat.com>

## Package scope and management

\[x\] Replace the dependency on the **NSGEV** and the **potomax** by a
dependency on the publicly released package **nieve**.

## Documentation

\[o\] Add a `.bib` file with the citations for the packages and cite the
packages in the vignette.

## R objects and functions, short-term

\[ \] Add a `fevdTList` creator allowing the definition from the
formulas (`threshold`, `location`, `scale` and `shape`) and a `dailyMet`
object.

\[ \] Add methods to coerce a `dailyMet` into suitable **zoo**
object(s).

\[o\] Add methods for the `pgpTList` class.

\[x\] Change the `simulate` method for the class `"pgpTList"` so that it
simulates the marks that are are exceeds a *high threshold* chosen as
the threshold `v` now given by the `predict` method which computes the
corresponding rate `lambdav`as well. By doing so the maximum of the
simulated marks will also be the simulate maximum for all the marks on
the period of interest.

## Interface

\[ \] Allow some renaming of the coefficients both for the GP part and
for the time Poisson part. For the GP part, the coefficients could be
renamed by using prefixes such as `scale_` and `shape_` followed by the
name of the variables as in `lm`. For instance we would get names like
`"scale_Cst"` and `"scale_sinPhi1"`for the models used in the examples
of the package.

## Long-term

\[x\] Allow the ML estimation, prediction, … for a full Poisson-GP
model, with a formula for the rate. This eventually should the
dependency on **extRemes** and **NHPoisson**.

## Consistency wih **NSGEV**

\[ \] The `quantMax` method of the class `"rqTList"` uses the argument
`newdata` where the same method for the class `"TVGEV"` of **NSGEV**
uses date. This should be made consistent.
