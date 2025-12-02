# Fetch authorization token

For a given username, check to see if we already have a token in the
storage environment `srv_auth_env`, otherwise prompt safely for password
and fetch a token from the NatureCounts server.

## Usage

``` r
srv_auth(username)
```

## Arguments

- username:

  Character vector. Username for <http://naturecounts.ca>. If provided,
  the user will be prompted for a password. If left NULL, only public
  collections will be returned.

## Value

Token character string
