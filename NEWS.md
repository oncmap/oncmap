# oncmap (development version)

## Dose grouping

- Regimens gain `actuations_per_dose` (default 1) and `dose_group_window`
  (seconds, `NA` disables). Counted actuations chain into one dose while each
  is at most `dose_group_window` seconds after the previous one -- `60` joins
  FindAir puffs in consecutive minutes. A dose counts in the period of its
  first actuation.
- With grouping, `opens` counts doses, a period is correct when it has
  `doses_per_period` doses each of at least `actuations_per_dose` actuations,
  and `perdoses` counts actuations capped per dose at `actuations_per_dose`
  against the prescribed actuations. `min_wait` applies between doses rather
  than actuations, and burst capping caps at the prescribed actuations.
- `all_periods` gains `fulldoses`, `actuations` and `actuationsrx`;
  `adherence_preprocess()` timestamps and `input_report` gain the dose each
  counted actuation belongs to (`dose`, `oncmap_dose`).
- The defaults leave results unchanged: every actuation is a dose of one.

## Burst removal (behaviour change)

- Every actuation in a burst is now removed from processing (`excluded`
  code 5, "part of an actuation burst" in `input_report`). 0.1.9 instead
  capped a period containing a burst at the prescribed doses, keeping its
  earliest burst actuations. Bursts are removed before dose grouping.

# oncmap 0.1.9

## New input formats

- Added `patchcap` input format.
- Added `findair` input format. `datetime_header` may now name several
  columns (comma separated), which are joined with a space before parsing.
- Fixed string-based format filters and `patchcap` value matching.

## Medication filter

- Each input format now declares a `medication_header` column (`NA` when the
  device does not record the medication): `ecap3` (`Medications`),
  `adheretech`/`adheretechxls`/`clevercap` (`Medication`) and `findair`
  (`drug`).
- `read_input()` and `process_eamd()` gain a `medication` argument to keep
  only the actuations of one or more medications. `NULL` (the default)
  processes all of them. A medication not in the file is an error listing
  those that are; for a format with no medication column the filter is
  ignored with a warning.
- `read_input()` returns `medications`, the medications present in the file,
  and a per-actuation `medication` column in `data`.
- `process_eamd()` labels the report with `medication` when `med` is not set.

## Burst capping

- Regimens gain `burst_actuations` and `burst_window` (seconds), comma
  separated pairs of rules. An actuation is part of a burst when any rolling
  window of `burst_window` seconds holds at least `burst_actuations`
  actuations. In a dosing period containing a burst, burst actuations beyond
  `doses_per_period` are excluded with the new `excluded` code 5, capping the
  period at the prescribed dose. `NA` (the default, and all built-in regimens)
  disables it. E.g. `"8,3"` / `"120,60"`: 8 or more within 2 minutes, or 3 or
  more within 1 minute.

## Input report

- `process_eamd()` returns `input_report`: the input file's rows, each labeled
  with its parsed timestamp, whether it counted towards adherence
  (`oncmap_used`) and, if not, why (`oncmap_reason`) -- removed by the format
  filter, medication not selected, missing or unparseable timestamp, within
  `min_wait`, outside the monitored interval, non-monitored, or burst capped.
  Also available as `report_input()`.
- `read_input()` returns `dropped` (rows not read and why) and an `input_row`
  column in `data`. Rows whose timestamp cannot be parsed are now dropped
  rather than returned as `NA`, which made `adherence_preprocess()` fail.

## Manual entries

- Input formats declare `manual_header` and `manual_values`, the column and
  values marking a manually entered actuation (`findair`: `manual` = `1`).
  `read_input()` normalises it to a logical `manual` column in `data`.
- `process_eamd(include_manual = FALSE)` excludes manual entries (new
  `excluded` code 6, "manual entry excluded" in `input_report`). They are set
  aside before the other rules, so they neither trigger `min_wait` nor take
  part in burst detection. The default `TRUE` counts them, as before.
  `adherence_preprocess()` takes the flag as `manual` / `include_manual`.
- New `input_choices()` lists the medications and manual-entry flags present
  in a `read_input()` result, so a UI can default to the single value when
  there is only one. Format selection happens in `read_input()`; reading with
  `medication` narrows the manual flags to that medication.

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

# oncmap 0.1.8

- Added `clevercap` input format.

# oncmap 0.1.7

- Updated `read_input` checking for headers -- ignore empty ones in the format.

# oncmap 0.1.6

-   Added a `NEWS.md` file to track changes to the package.
-   Added additional variables in the output of `process_eamd()`
    -   `patient_id` -- containing the inferred patient id
    -   `input_data` -- details of the input file processing
