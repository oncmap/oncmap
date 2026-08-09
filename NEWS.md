# oncmap 0.1.8

## New input formats

- Added `clevercap` input format.
- Added `patchcap` input format.
- Fixed string-based format filters and `patchcap` value matching.

## Bug fixes

- `report_adherence()` -- `adhweek` now covers 7 calendar days. The rolling
  window was a fixed 7 rows of `all_periods`, which only equals 7 days when
  `periods_per_day` is 1; at 2 periods per day it spanned 3.5 days.
- `adherence_preprocess()` -- actuations falling between midnight and
  `day_start_time` on the first monitored day are now excluded, rather than
  retained but counted into no period, which silently dropped them from the
  adherence denominator.
- `read_input()` -- no longer errors on single-column input files.
- `read_input()` -- reports a clear error when `include_formats`,
  `exclude_formats` or `formats_def` leave no formats to match against.
- `read_input()` -- a format whose `filter` cannot be applied to the file is
  logged and skipped, instead of aborting format detection entirely.
- `read_input()` -- a missing input file raises an error instead of returning
  an empty result indistinguishable from "no format matched".

## Documentation and packaging

- `read_input()` and `process_eamd()` examples now use the installed sample
  data via `system.file()` instead of an unresolvable relative path.
- Added `inst/extdata/sample-data-ecap1.csv` as example data.
- Corrected the declared R dependency (`3.60` was a typo for `3.6.0`) and
  declared the `tools` and `utils` dependencies.

# oncmap 0.1.7

- Updated `read_input` checking for headers -- ignore empty ones in the format.

# oncmap 0.1.6

-   Added a `NEWS.md` file to track changes to the package.
-   Added additional variables in the output of `process_eamd()`
    -   `patient_id` -- containing the inferred patient id
    -   `input_data` -- details of the input file processing
