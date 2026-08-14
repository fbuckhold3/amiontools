# amiontools

Shared R package for pulling resident schedule data from Amion,
classifying assignments into top-level rotation categories, and
crosswalking Amion identities against RDM resident records — used across
the IMSLU / GME Tools ecosystem (`amion-va-report`, rotation analysis,
attendance reconciliation, and duty-hour tooling).

## What's here

- **`R/fetch.R`** — raw Amion CSV export fetch (`fetch_amion_data()`,
  `fetch_amion_month()`, URL building). Amion's export is public and
  unauthenticated, keyed by a program token (`Lo=`), not a secret.
- **`R/classify.R`** — rolls up Amion's raw `Grouping` field into a smaller
  set of top-level rotation categories (`ROTATION_CATEGORY_MAP`,
  `classify_rotation()`), plus day-value helpers for half/full-day blocks.
- **`R/crosswalk.R`** — `get_amion_crosswalk()`, reads the
  `record_id <-> amion_staff_id` link stored on RDM's `resident_data` form.
- **`R/rotation_summary.R`** — `build_rotation_summary()`, the end-to-end
  fetch → crosswalk → classify → aggregate pipeline producing per-resident
  rotation-day counts by category.

## Why `Assignment Type == "r"` matters

Every Amion row also carries an `Assignment Type` of `r` (actual rotation
assignment), `c` (clinic/didactic half-day block), or `o` (role/roster
position — overlapping, not yet fully understood). Filtering to `r` only
gives one clean macro-rotation per resident-date with no overlap —
confirmed empirically against a full academic year of data (2026-08-14,
zero resident-dates had more than one `r` row). `c`/`o` rows are excluded
from rotation-day counting for that reason, though they'll matter for
future duty-hour work.

## Status

Early — built 2026-08-14 to formalize a hand-validated rotation-count
script. Not yet wired into any deployed app.
