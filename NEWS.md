# restoptr 1.0.4

- Remove dependency to rgdal (see https://r-spatial.org/r/2022/04/12/evolution.html)
- Increase time limit in tests, as sometimes CRAN test servers cannot complete some tests due to insufficient time.

# restoptr 1.0.3

- Fix unexpected behaviour of `add_available_areas_constraint` when data is vector data.
- Add `touches` option in `add_available_areas_constraint` and `add_locked_out_constraint`, useful when data is vector data.
- Fix Warning due to `terra` update when using `levels`.
- Add `solution_name_prefix` setting, that allows defining a custom prefix for solutions names.
- Fix the `restoptr.Rmd` vignette, there was a bug due to the new `terra` version, not allowing to subset layers having not unique name. This was fixed using the newly introduced `solution_name_prefix` setting.
- Allow numeric values for `min_restore` and `max_restore`, in `add_restorable_constraint()` when unit is not `cells`, as the conversion to integer is done during the conversion to unitless values.
- Relies on `restopt-2.0.1`, which brings improved performances for the `compactness` constraint.

# restoptr 1.0.2

- Fix a few bugs.
- Add a vignette to illustrate the submitted article case study.

# restoptr 1.0.1

- Update doc to fix a minor issue with r-oldrel-windows-ix86+x86_64
- Remove time limit in some tests to pass CRAN tests

# restoptr 1.0.0

- First stable release of the package.

# restoptr 0.0.0.99999

- Initial work on developing package.
