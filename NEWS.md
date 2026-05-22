# celsiuslids 1.1.1

* Added citation metadata (`CITATION.cff`, `.zenodo.json`) so the software can be archived and cited via a DOI. No functional changes.

# celsiuslids 1.1.0

* **Multi-entry tables.** Tables that can legitimately hold more than one record
  per LS member now generate a random number of rows per `CORENO` (1–5), with
  every `CORENO` appearing at least once and a per-table ceiling of 600,000
  rows. All other tables are unchanged (one row per `CORENO`). Affected tables:
  `NM71`, `NM81`, `NM91`, `NM01`, `NM11`, `NM21`, `EMBR`, `CANC`, `LBSM`, `SBSM`,
  `IDMI`, `WDOW`, `ENLS`, `REEN`, `LBSF`, `SBSF`.
* **Lower-case output names.** Variable names in downloaded files are now written
  in lower case while keeping the upper-case `_LIDS` suffix (e.g. `VAR1` →
  `var1_LIDS`). The in-app preview, selection menus, and table/file names are
  unchanged.
* **New warnings.** On *Generate*, the app now warns when a selected table can
  hold multiple records per `CORENO`, and when the per-table 600,000-row ceiling
  can be reached at the chosen sample size.

# celsiuslids 1.0.0

* Initial release.
