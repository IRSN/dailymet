TODO list for the **dailymet** package
================
Yves Deville <deville.yves@alpestat.com>

## Package scope and management

\[ \] Replace the dependency on the **NSGEV** and the **potomax** by a
dependency on the publicly released package **nieve**.

## Documentation

\[ \] Add a `.bib` file with the citations for the packages and cite the
packages in the vignette.

## R objects and functions, short-term

\[ \] Add a `fevdTList` creator allowing the definition from the
formulas (`threshold`, `location`, `scale` and `shape`) and a `dailyMet`
object.

\[ \] Add methods to coerce a `dailyMet` into suitable **zoo**
object(s).

\[ \] Add methods for the `pgpTList` class.

## Interface

\[ \] Allow some renaming of the coefficients both for the GP part and
for the time Poisson part. For the GP part, the coefficients could be
renamed by using prefixes such as `scale_` and `shape_` followed by the
name of the variables as in `lm`. For instance we would get names like
`"scale_Cst"` and `"scale_sinPhi1"`for the models used in the examples
of the package.

## Long-term

\[ \] Allow the ML estimation, prediction, … for a full Poisson-GP
model, with a formula for the rate. This eventually should the
dependency on **extRemes** and **NHPoisson**.
