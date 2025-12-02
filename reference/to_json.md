# Convert filter parameters to JSON

Converts filter parameters to JSON, first unboxing the parameters which
need to be unboxed. The list of parameters needing to be unbox is stored
in the internal data `queries` which is created by
"./data-raw/data_creation.R".

## Usage

``` r
to_json(f)
```

## Arguments

- f:

  List. Filter parameters

## Value

A JSON object
