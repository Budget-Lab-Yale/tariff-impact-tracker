# Fact-Check Resolution Notes

**Date:** 2026-04-01
**Based on:** `fact_check_claims_checklist.docx`

---

## Items Fixed (Code + Text Changes)

### IN-5 — Revenue date mismatch: **FIXED**
- **Issue:** "tariffs so far have raised $49.7 billion of revenue through January 2026" — but $49.7B = $214.7B − $165B, and $214.7B is through *February*, not January. Revenue data extends one month beyond PCE data.
- **Root cause:** Sentence used `data_month` (PCE date = January 2026) instead of `latest_rev_date` (revenue date = February 2026).
- **Fix:** Changed variable reference from `data_month` to `latest_rev_date` in both `tariff_impacts_report.Rmd` (line 49) and `tariff_impacts_drupal.Rmd` (line 120). Now correctly says "through February 2026."

### PG-3 — Comparison period prices (0.5% / 0.3%): **FIXED**
- **Issue:** "versus 0.5% / 0.3% over the same 13-month period starting in 2023" could not be reproduced.
- **Root cause:** Code in `report_setup.R` (lines 670–686) computed a 1-month change (Dec 2022 → Jan 2023) instead of the correct 13-month analog (Dec 2022 → Jan 2024). The comparison window length changed every time the data updated because it filtered `year==2023, month==comparison_month` rather than computing the proper endpoint.
- **Fix:** Changed the comparison endpoint filter from `year(date) == 2023, month(date) == comparison_month` to `year(date) == 2024, month(date) == comparison_month`. This produces the correct 13-month analog window (Dec 2022 → Jan 2024). Values will now be dynamically correct regardless of which month the data extends to.

### KT-2 — "repealing" IEEPA tariffs: **FIXED**
- **Issue:** The Court did not "repeal" tariffs; it held that IEEPA did not authorize them, and the tariffs were vacated.
- **Fix:** Changed "the SCOTUS decision repealing IEEPA Tariffs" to "the Supreme Court decision vacating IEEPA tariffs" in both Rmd files.

### TR-1 — "statutory" vs "effective" tariff rate: **FIXED**
- **Issue:** Text says "statutory tariff rate" but the underlying data (CSV label "Weighted ETR") is an effective tariff rate.
- **Fix:** Changed "the import-weighted average statutory tariff rate" to "the import-weighted average effective tariff rate" in both Rmd files.

### LM-4 — Ambiguous comparison window: **FIXED (wording)**
- **Issue:** "0.5% less than the growth rate in 2024 over the same period" was ambiguous — the fact-checker interpreted "same period" as a 14-month shifted window.
- **Investigation:** The number is correct. The code compares the same calendar window (Dec→Feb) across years: Dec 2024→Feb 2026 = −0.554%, Dec 2023→Feb 2024 = −0.079%, difference = −0.476% ≈ −0.5%.
- **Fix:** Changed wording to "0.5% below the December-to-February change observed in 2024" in both Rmd files.

### LM-5 — Trend methodology disconnect: **FIXED (wording)**
- **Issue:** "versus the trend's −0.7% predicted change" — the fact-checker read −1.1% from the LP trend in the CSV, but the text uses a simple linear trend (−0.678% ≈ −0.7%).
- **Investigation:** The code (`calc_trend()`) fits a simple linear regression, not the LP trend shown in the figure. The number −0.7% is correct for the simple linear trend.
- **Fix:** Changed "versus the trend's X% predicted change" to "versus X% predicted by a simple linear trend" in both Rmd files, to distinguish from the LP trend shown in the figure.

### TD-1 — "grew 17.8% over trend" ambiguity: **FIXED (wording)**
- **Issue:** "Real imports grew 17.8% over the pre-2025 trend between December 2024 and March 2025" reads like a growth rate, but it's actually a change in the deviation from trend (4.2% → 22.0%, a 17.8pp increase in the gap).
- **Investigation:** Code confirms this is `mar_2025$imports_vs_trend - dec_2024_trade$imports_vs_trend`. The $50.4B is the dollar value of the same gap change.
- **Fix:** Rewrote to: "between December 2024 and March 2025, real imports moved from X% to Y% above the pre-2025 trend — a Z percentage point increase in the gap, equal to $N billion in real 2025 dollars."

---

## Items Requiring Re-Knit Only (No Code Changes Needed)

### FX-1 through FX-4 — Exchange rate figures stale from data revision
- **Issue:** USD −7.4%, CNY +0.1%, CAD +0.9%, MXN +14.0% matched the March 20 vintage exactly, but BIS NEER data was revised between March 20 and March 31.
- **Current values (Feb 2026 monthly avg):** USD −7.6%, CNY −0.5% (direction flipped to weaker), CAD +0.7%, MXN +13.4%.
- **Resolution:** The code is correct — it dynamically computes monthly averages from BIS NEER data. Simply re-knitting the report with the March 31 data pull will produce the updated figures. No code change needed.

---

## Items Confirmed Correct (No Changes Needed)

### TR-7 — Revenue projection $327B
- **Issue:** Fact-checker found $334B on the public Budget Lab page, not $327B.
- **Resolution:** The middle number is `annualized_rev`, dynamically computed as `avg_monthly_2mo_rev * 12` in `report_setup.R`. It changes with each data vintage. The $246B and $319B are hardcoded and stable. The $327B vs $334B difference reflects different data vintages, not an error. Re-knitting updates it automatically.

### FX-5 — "20.3% stronger than long-run average"
- **Issue:** Fact-checker couldn't independently verify.
- **Resolution:** Dynamically computed in `report_setup.R` from Haver series `FXTWBDI@USECON` (Fed's broad real trade-weighted dollar index). Calculation: `(Dec 2024 value / mean of Jan 2000 – Dec 2024) − 1 = 20.3%`. Methodology is sound. Not the BIS RBUSBIS series the fact-checker looked for.

### LM-4 — "0.5% less than 2024" (number)
- The number is correct (see wording fix above). Dec→Feb change in 2024 was −0.079% vs −0.554% in 2025, difference = −0.476% ≈ −0.5%.

### LM-5 — Manufacturing trend "−0.7%" (number)
- The number is correct for the simple linear trend (−0.678%). See wording fix above.

### All other confirmed items (KT-1, KT-3–7, IN-1–4, IN-6–7, TR-2–6, PG-1/2/4, IP-1–13, IM-1, LM-1–3, IO-1, TD-2–5)
- No changes needed.

---

## Files Modified

| File | Changes |
|------|---------|
| `R/report_setup.R` | Fixed PG-3 comparison window (lines 670–686) |
| `R/tariff_impacts_report.Rmd` | KT-2 wording, TR-1 wording, IN-5 variable, LM-4 wording, LM-5 wording, TD-1 wording |
| `R/tariff_impacts_drupal.Rmd` | Same text changes as report.Rmd |

## Next Steps

- Re-knit both report and Drupal documents with current (March 31) data to update FX figures and annualized revenue
- Verify PG-3 comparison values look reasonable after the fix
