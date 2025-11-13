# Describe the contents of an environment

Describe the contents of an environment

## Usage

``` r
# S3 method for class 'environment'
btw_this(x, ..., items = NULL)
```

## Arguments

- x:

  An environment.

- ...:

  Additional arguments are silently ignored.

- items:

  Optional. A character vector of objects in the environment to
  describe.

## Value

A string describing the environment contents with `#>` prefixing each
object's printed representation.

## See also

[`btw_tool_env_describe_environment()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_environment.md)

Other btw formatting methods:
[`btw_this()`](https://posit-dev.github.io/btw/dev/reference/btw_this.md),
[`btw_this.character()`](https://posit-dev.github.io/btw/dev/reference/btw_this.character.md),
[`btw_this.data.frame()`](https://posit-dev.github.io/btw/dev/reference/btw_this.data.frame.md)

## Examples

``` r
env <- new.env()
env$cyl_6 <- mtcars[mtcars$cyl == 6, ]
env$gear_5 <- mtcars[mtcars$gear == 5, ]
btw_this(env)
#> [1] "## Context"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
#> [2] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> [3] "cyl_6\n```json\n{\"n_cols\":11,\"n_rows\":7,\"groups\":[],\"class\":\"data.frame\",\"columns\":{\"mpg\":{\"variable\":\"mpg\",\"type\":\"numeric\",\"mean\":19.7429,\"sd\":1.4536,\"p0\":17.8,\"p25\":18.65,\"p50\":19.7,\"p75\":21,\"p100\":21.4},\"cyl\":{\"variable\":\"cyl\",\"type\":\"numeric\",\"mean\":6,\"sd\":0,\"p0\":6,\"p25\":6,\"p50\":6,\"p75\":6,\"p100\":6},\"disp\":{\"variable\":\"disp\",\"type\":\"numeric\",\"mean\":183.3143,\"sd\":41.5625,\"p0\":145,\"p25\":160,\"p50\":167.6,\"p75\":196.3,\"p100\":258},\"hp\":{\"variable\":\"hp\",\"type\":\"numeric\",\"mean\":122.2857,\"sd\":24.2605,\"p0\":105,\"p25\":110,\"p50\":110,\"p75\":123,\"p100\":175},\"drat\":{\"variable\":\"drat\",\"type\":\"numeric\",\"mean\":3.5857,\"sd\":0.4761,\"p0\":2.76,\"p25\":3.35,\"p50\":3.9,\"p75\":3.91,\"p100\":3.92},\"wt\":{\"variable\":\"wt\",\"type\":\"numeric\",\"mean\":3.1171,\"sd\":0.3563,\"p0\":2.62,\"p25\":2.8225,\"p50\":3.215,\"p75\":3.44,\"p100\":3.46},\"qsec\":{\"variable\":\"qsec\",\"type\":\"numeric\",\"mean\":17.9771,\"sd\":1.7069,\"p0\":15.5,\"p25\":16.74,\"p50\":18.3,\"p75\":19.17,\"p100\":20.22},\"vs\":{\"variable\":\"vs\",\"type\":\"numeric\",\"mean\":0.5714,\"sd\":0.5345,\"p0\":0,\"p25\":0,\"p50\":1,\"p75\":1,\"p100\":1},\"am\":{\"variable\":\"am\",\"type\":\"numeric\",\"mean\":0.4286,\"sd\":0.5345,\"p0\":0,\"p25\":0,\"p50\":0,\"p75\":1,\"p100\":1},\"gear\":{\"variable\":\"gear\",\"type\":\"numeric\",\"mean\":3.8571,\"sd\":0.6901,\"p0\":3,\"p25\":3.5,\"p50\":4,\"p75\":4,\"p100\":5},\"carb\":{\"variable\":\"carb\",\"type\":\"numeric\",\"mean\":3.4286,\"sd\":1.8127,\"p0\":1,\"p25\":2.5,\"p50\":4,\"p75\":4,\"p100\":6}}}\n```"
#> [4] ""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> [5] "gear_5\n```json\n{\"n_cols\":11,\"n_rows\":5,\"groups\":[],\"class\":\"data.frame\",\"columns\":{\"mpg\":{\"variable\":\"mpg\",\"type\":\"numeric\",\"mean\":21.38,\"sd\":6.659,\"p0\":15,\"p25\":15.8,\"p50\":19.7,\"p75\":26,\"p100\":30.4},\"cyl\":{\"variable\":\"cyl\",\"type\":\"numeric\",\"mean\":6,\"sd\":2,\"p0\":4,\"p25\":4,\"p50\":6,\"p75\":8,\"p100\":8},\"disp\":{\"variable\":\"disp\",\"type\":\"numeric\",\"mean\":202.48,\"sd\":115.4906,\"p0\":95.1,\"p25\":120.3,\"p50\":145,\"p75\":301,\"p100\":351},\"hp\":{\"variable\":\"hp\",\"type\":\"numeric\",\"mean\":195.6,\"sd\":102.8338,\"p0\":91,\"p25\":113,\"p50\":175,\"p75\":264,\"p100\":335},\"drat\":{\"variable\":\"drat\",\"type\":\"numeric\",\"mean\":3.916,\"sd\":0.3895,\"p0\":3.54,\"p25\":3.62,\"p50\":3.77,\"p75\":4.22,\"p100\":4.43},\"wt\":{\"variable\":\"wt\",\"type\":\"numeric\",\"mean\":2.6326,\"sd\":0.8189,\"p0\":1.513,\"p25\":2.14,\"p50\":2.77,\"p75\":3.17,\"p100\":3.57},\"qsec\":{\"variable\":\"qsec\",\"type\":\"numeric\",\"mean\":15.64,\"sd\":1.1305,\"p0\":14.5,\"p25\":14.6,\"p50\":15.5,\"p75\":16.7,\"p100\":16.9},\"vs\":{\"variable\":\"vs\",\"type\":\"numeric\",\"mean\":0.2,\"sd\":0.4472,\"p0\":0,\"p25\":0,\"p50\":0,\"p75\":0,\"p100\":1},\"am\":{\"variable\":\"am\",\"type\":\"numeric\",\"mean\":1,\"sd\":0,\"p0\":1,\"p25\":1,\"p50\":1,\"p75\":1,\"p100\":1},\"gear\":{\"variable\":\"gear\",\"type\":\"numeric\",\"mean\":5,\"sd\":0,\"p0\":5,\"p25\":5,\"p50\":5,\"p75\":5,\"p100\":5},\"carb\":{\"variable\":\"carb\",\"type\":\"numeric\",\"mean\":4.4,\"sd\":2.6077,\"p0\":2,\"p25\":2,\"p50\":4,\"p75\":6,\"p100\":8}}}\n```"                                               
```
