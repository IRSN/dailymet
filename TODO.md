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

## Long-term

\[ \] Allow the ML estimation, prediction, … for a full Poisson-GP
model, with a formula for the rate. This eventually should the
dependency on **extRemes** and **NHPoisson**.
