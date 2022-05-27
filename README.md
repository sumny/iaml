# iaml

Reproduce results from YAHPO Gym iaml.

## Installation

You can install the released version from GitHub using:

``` r
remotes::install_github("sumny/iaml")
```

Note that for *exact* reproduction, you need the R packages with the exact version as specified in the DESCRIPTION
(i.e., == instead of >=).
Also note that some measures (ram and time related ones) can never be exactly reproduced because they depend on system
hardware and system load.

By default, in all `eval_*` functions, the original fixed seed is used and therefore, runs are fully deterministic,
(except for the ram and time measures mentioned above).

To change this, simply provide a custom seed.

Note that data is obtained via `mlr3oml` and is cached via `qs`.

## Example

```r
library(iaml)
scenario = "iaml_glmnet"
configuration = list(alpha = 0.1, s = 0.1, trainsize = 0.05, task_id = "40981")
eval_yahpo(scenario, configuration)
```
