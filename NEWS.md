# ICEHmeasures 2.0.0

- Updated the `equiplot()` syntax for wide-format datasets (`wide = TRUE`). The `strat_var` argument is now required and should contain the variables representing each dot in the desired order. The function remains unchanged when `wide = FALSE`.
- Fixed the handling of the `cluster` argument in `cixr()`.
- Added additional input validations to `cixr()`, `siilogit()`, and `mad()`.

---

# ICEHmeasures 1.1.0

- Fixed cluster and weight behavior in `siilogit()` function
- Added several input validations and more informative warnings when arguments were misspecified

---

# ICEHmeasures 1.0.1

- First public release