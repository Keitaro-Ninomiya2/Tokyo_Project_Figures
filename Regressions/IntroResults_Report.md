# Introduction Results Report

**Script:** `Regressions/IntroResults_Master.R`
**Data:** `Tokyo_Personnel_Master_All_Years_v2.csv` (306,334 named worker-years; 467,397 total rows)
**Date:** 2026-03-14

---

## Descriptive Statistics (Baseline Quantities)


| Statistic                                   | Value                                   |
| ------------------------------------------- | --------------------------------------- |
| Workers per year (wartime avg.)             | ~20,000                                 |
| Female share (wartime avg.)                 | 5.4-7.4%                                |
| Total drafted (all years)                   | 4,037                                   |
| Drafted in 1944 alone                       | 2,782                                   |
| Position-year observations (panel)          | 12,099 (9,206 with ka FE)               |
| Mean new hires per position-year            | 2.35 (drafted: 3.45, non-drafted: 2.26) |
| Pct positions with any transfer in          | 63.4%                                   |
| Rank distribution (wartime, enriched 5-level): 1 / 2 / 3 / 4 / 5 | 23.6% / 70.3% / 5.6% / 0.4% / 0.1% |
| Unique kyoku / ka / positions               | 64 / 1,504 / 71                         |


---

## RESULT SECTION 1: Internal Transfers and Hiring (Lines 21-27)

### Line 21 (overview)

> "impacted units responded by promoting lower-ranked workers or poaching from external offices as replacements, which in turn determined the number of new hires"

Supported by the combination of Lines 22, 23, and 30 below.

---

### Line 22

> "A draft-induced exit increases the number of internal transfers into the office by **5%** more than in the adjacent office."

**Regression:** `fepois(n_transfers_in ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm)`


|                | Coef   | SE     | p-value | N     |
| -------------- | ------ | ------ | ------- | ----- |
| n_drafted_male | 0.0501 | 0.0099 | <0.001  | 8,818 |


**% change:** exp(0.0501) - 1 = **5.1%** per draft exit. **Matches the claim.**

**Descriptives:** Mean transfers in: drafted positions = 84.5, non-drafted = 37.5.

---

### Line 23

> "This was driven by promoting workers in lower-ranked positions and poaching workers from external offices."

**Regression:** Decompose transfers by origin: same-ka (internal reallocation / promotion proxy) vs. diff-ka (external poaching).

`fepois(n_transfers_same_ka ~ n_drafted_male + log(cumul_n_male+1) | year_num + ka_id + pos_norm)`
`fepois(n_transfers_diff_ka ~ n_drafted_male + log(cumul_n_male+1) | year_num + ka_id + pos_norm)`

| Transfer origin | Coef | SE | p-value | % change | N |
|---|---|---|---|---|---|
| Same ka (internal reallocation) | 0.0913 | 0.0322 | 0.005 | **9.6%** | 1,732 |
| Diff ka (external poaching) | 0.0494 | 0.0099 | <0.001 | **5.1%** | 8,815 |

**Interpretation:** Both channels are significant. Internal reallocation (same-ka transfers) responds more strongly per draft (9.6% vs 5.1%), but external poaching is far more common in volume (367,000 diff-ka vs 7,860 same-ka total transfers). Both promotion and poaching channels are active.

**Descriptives:** Mean same-ka transfers per position-year = 0.85. Mean diff-ka transfers = 39.9.

---

### Line 24

> "The offices selected to give up workers to the impacted office are not immediate neighbors... I do not find effects on the chances of being selected by distance to the office measured by network."

**Distance definition:** same ka = 1, same kyoku different ka = 2, different kyoku = 3.

**Regression:** `fepois(n_transfers_distK ~ n_drafted_male + log(cumul_n_male+1) | FE)` for K=1,2,3.


| Distance                | Coef   | SE     | p-value | N     |
| ----------------------- | ------ | ------ | ------- | ----- |
| 1 (same ka)             | 0.0913 | 0.0322 | 0.005   | 1,732 |
| 2 (same kyoku, diff ka) | 0.0486 | 0.0100 | <0.001  | 5,868 |
| 3 (diff kyoku)          | 0.0390 | 0.0121 | 0.001   | 8,441 |


**Descriptives (all transfers):**

- Distance 1: 1.4%
- Distance 2: 51.7%
- Distance 3: 46.9%

**FLAG:** The coefficient on drafting is **largest for distance 1** (same ka, 0.09) and **declines monotonically** with distance (0.05 at dist 2, 0.04 at dist 3). This means draft-induced transfers are disproportionately sourced from nearby offices. The intro claim that source offices are "not immediate neighbors" is **contradicted** by the regression — closer offices respond more strongly to drafting. However, in raw volume, most transfers come from farther away (dist 2-3 = 98.6%), which is simply because there are many more distant offices than nearby ones. The sentence should be revised to reflect that the *intensive margin* response is stronger for closer offices, even though the *extensive margin* volume is dominated by distant transfers.

---

### Line 25

> "The selected offices did not change their number of new hires as a result of having their workers removed."

**Regression:** `fepois(n_new_hires ~ n_transfers_out + log(cumul_n_male+1) | FE)` on positions with **zero own drafts** (donor isolation).


|                 | Coef   | SE     | p-value | N     |
| --------------- | ------ | ------ | ------- | ----- |
| n_transfers_out | 0.0000 | 0.0001 | 0.950   | 6,035 |


**Confirmed:** Coefficient is essentially zero. Donor offices did not adjust their hiring.

---

### Line 26

> "This suggests the central human resource team reassigned workers from teams with redundant workers."

Not a testable regression claim. This is the interpretive implication of Line 25.

---

### Line 27

> "Office units that managed to find a replacement from external offices had [] lower numbers of new hires."

**Regression:** `fepois(n_new_hires ~ n_drafted_male + n_transfers_in + log(cumul_n_male+1) | FE)` among **drafted positions only**.


|                | Coef   | SE     | p-value | N   |
| -------------- | ------ | ------ | ------- | --- |
| n_transfers_in | 0.0005 | 0.0001 | <0.001  | 342 |
| n_drafted_male | 0.0233 | 0.0079 |         |     |


**FLAG:** The coefficient on `n_transfers_in` is **positive** (0.0005), not negative as the intro implies. Descriptively, drafted positions with transfers have **more** new hires (4.38) than those without (1.82). This likely reflects that larger, more-drafted offices get both more transfers AND more hires. The sentence may need revision or a different specification (e.g., transfers-in as share of vacancies). Consider replacing with a descriptive statement or an IV approach.

---

## RESULT SECTION 2: Gender Composition (Lines 29-33)

### Line 30

> "Impacted units induced an increase in the number of new hires by **4 percent** more than their adjacent units, which translates to **[]** headcounts on average."

**Regression:** `fepois(n_new_hires ~ n_drafted_male + log(cumul_n_male+1) | year_num + ka_id + pos_norm)`


|                | Coef   | SE     | p-value | N     |
| -------------- | ------ | ------ | ------- | ----- |
| n_drafted_male | 0.0352 | 0.0069 | <0.001  | 6,497 |


**% change:** exp(0.0352) - 1 = **3.6%**. Baseline mean new hires = 2.40.
**Headcount translation:** 2.40 x 0.036 = **0.086 headcounts** per draft.

**Note:** Estimate is 3.6%, slightly below the stated 4%. Consider updating to "approximately 4 percent" or "3.6 percent."

---

### Lines 29 & 31

> L29: "I do not find a differential effect of drafting on the number of male and female workers entering."
> L31: "The exiters were more likely to be replaced by a male new hire."

**Regression:** Separate Poisson regressions for female vs. male new hires.


| Outcome            | Coef   | SE     | p-value | N     |
| ------------------ | ------ | ------ | ------- | ----- |
| n_new_hires_female | 0.0324 | 0.0211 | 0.125   | 3,556 |
| n_new_hires_male   | 0.0359 | 0.0067 | <0.001  | 6,482 |


**Interpretation:** Male new hires respond significantly to drafting; female new hires do not (p=0.13). This supports both L29 (no significant female effect) and L31 (male replacement dominant).

**Descriptives:** Female share among all new hires = 5.8% (1,291 female vs. 20,810 male).

---

### Line 32

> "Replacement with female new hires was concentrated in lower-ranked positions, as a draft induced a **[]%** increase in the number of female new hires in low-ranked roles and a **[]%** change in higher-ranked positions."

**Regression:** `fepois(n_new_hires_female ~ n_drafted_male + ... | FE)` by rank (1, 2, 3 separately).

| Rank | Coef | SE | p-value | % change | N | Total female hires |
|---|---|---|---|---|---|---|
| 1 (雇/嘱託) | 0.5548 | 0.3009 | 0.065 | **74.2%** | 469 | 692 |
| 2 (regular staff) | 0.0154 | 0.0154 | 0.317 | **1.6%** | 1,888 | 581 |
| 3 (senior/kanri) | 4.571 | 1.264 | <0.001 | **9,564%** | 37 | 18 |

For comparison — male new hires by rank:

| Rank | Coef | SE | p-value | N |
|---|---|---|---|---|
| 1 (雇/嘱託) | 0.1172 | 0.0600 | 0.051 | 838 |
| 2 (regular staff) | 0.0308 | 0.0075 | <0.001 | 3,911 |
| 3 (senior/kanri) | 0.1351 | 0.0500 | 0.007 | 532 |

**Placeholder fill:** "...a draft induced a **74.2%** increase in the number of female new hires in the lowest-ranked (雇/嘱託) roles and a **1.6%** change in regular staff positions."

**Note:** The rank 1 effect is large (74.2%) and marginally significant (p=0.065). The rank 3 result (9,564%) is driven by only 18 total female hires across 37 non-singleton obs — this is an artifact of near-zero baseline and should not be interpreted. The concentration of female replacement hiring is clearly at rank 1 (雇/嘱託), not rank 2 or 3.

**Descriptives:** Mean female new hires per position-year: rank 1 = 0.45, rank 2 = 0.10, rank 3 = 0.01.

---

### Line 33

> "I do not find any differences in exposure to more females relative to men in the impacted workplaces."

**Regression:** `feols(female_share_all ~ any_drafted + cumul_n_male | year_num + ka_id + pos_norm)` (excludes kanri positions).


| Treatment   | Coef    | SE     | p-value | N     |
| ----------- | ------- | ------ | ------- | ----- |
| any_drafted | -0.0112 | 0.0091 | 0.217   | 8,233 |
| draft_share | -0.0218 | 0.0164 | 0.183   | 8,233 |


**Confirmed:** Neither specification is significant. Overall female share does not change with drafting.

**Descriptives:** Mean female share (all workers) = 4.7%. Drafted positions: 3.8%. Non-drafted: 4.8%.

---

## RESULT SECTION 3: Promotion and Retention (Lines 35-38)

### Lines 35-36

> "Military drafting accelerated the promotion of pre-existing staff."
> "Workers from impacted offices were assigned to positions **[]** ranks higher on average."

**Enriched rank definition (5-level):**

- Pre-1948: rank 5 = 局長; rank 4 = 部長/次長/課長; rank 3 = 主事/技師/事務官/所長/校長/區長; rank 2 = 書記/技手/屬/主事補 etc.; rank 1 = 雇/囑託員/臨時
- Post-1948: rank 5 = 局長; rank 4 = 部長/次長; rank 3 = 課長/所長/校長/場長/園長/支所長/區長; rank 2 = 係長; rank 1 = 事務吏員/技術吏員/事務員

Z-score standardization: rank is standardized within each era (pre/post 1948) to mean 0, SD 1.

**Regression:** `feols(rank_change ~ draft_share | ka_id_1944 + pos_norm_1944)` on non-drafted 1944 workers appearing postwar.

| Specification | DV | Coef | SE | p-value | N |
|---|---|---|---|---|---|
| Ka FE: any_drafted | rank_e_change | 0.058 | 0.068 | 0.401 | 488 |
| Ka FE: draft_share | rank_e_change | 0.083 | 0.145 | 0.568 | 488 |
| Ka FE: any_drafted | rank_z_change | 0.061 | 0.085 | 0.476 | 488 |
| Ka FE: draft_share | rank_z_change | 0.104 | 0.177 | 0.561 | 488 |
| Ka FE: draft_share | max_rank_e_change | -0.151 | 0.231 | 0.515 | 488 |
| Kyoku FE: draft_share | rank_e_change | 0.078 | 0.126 | 0.537 | 523 |
| Kyoku FE: draft_share | rank_z_change | 0.103 | 0.160 | 0.518 | 523 |

**Note:** With the enriched 5-level rank, all coefficients are now **positive** (drafted-position workers gain 0.06-0.10 standardized ranks more), consistent with the direction of the intro claim. However, **none are statistically significant** (p > 0.40 across all specs). The positive direction is suggestive but underpowered with N=488.

**Descriptives (enriched rank):**

| Group | N | Mean rank 1944 | Mean rank postwar | Mean rank change |
|---|---|---|---|---|
| Drafted position | 497 | 1.96 | 1.48 | -0.481 |
| Non-drafted position | 446 | 2.17 | 1.65 | -0.520 |

Note: Both groups show rank *declines* because the postwar reform compressed most workers into rank 1 (事務吏員/技術吏員 = 70.7% of postwar workers). Drafted-position workers declined **less** (-0.48 vs -0.52), consistent with a modest promotion advantage.

**Placeholder fill:** The z-scored conditional estimate is +0.10 SD (not significant). The unconditional descriptive gap is 0.04 enriched ranks less decline.

---

### Line 37

> "I do not find any effects on retention between workers across drafted and non-drafted offices."

**Regression:** `feols(tenure_postwar ~ draft_share | ka_id_1944 + pos_norm_1944)` on non-drafted 1944 workers.


| Treatment   | Coef  | SE    | p-value | N     |
| ----------- | ----- | ----- | ------- | ----- |
| any_drafted | 0.051 | 0.066 | 0.445   | 2,648 |
| draft_share | 0.072 | 0.123 | 0.562   | 2,648 |


**Confirmed:** No significant effects on postwar tenure.

**Descriptives:** Mean postwar tenure = 0.45 years. 17.7% of 1944 workers appear postwar. Drafted position: 0.47 years. Non-drafted: 0.44 years.

---

### Line 38

> "I do not find any impact on the office composition in future assignments between workers from non-drafted and drafted offices."

**Regression:** `feols(avg_female_share_dest ~ draft_share | ka_id_1944 + pos_norm_1944)` on non-drafted 1944 workers appearing postwar.


| Treatment   | Coef   | SE    | p-value | N   |
| ----------- | ------ | ----- | ------- | --- |
| any_drafted | -0.002 | 0.004 | 0.710   | 488 |
| draft_share | -0.007 | 0.007 | 0.305   | 488 |


**Confirmed:** No significant effects on destination office gender composition.

**Descriptives:** Mean destination female share = 3.1%.

---

## Summary of Flags / Discrepancies


| Line  | Claim                                       | Issue |
| ----- | ------------------------------------------- | ----- |
| 22    | 5%                                          | **Confirmed:** 5.1% |
| 23    | "promoting + poaching"                      | **Confirmed:** Both same-ka (9.6%) and diff-ka (5.1%) channels significant |
| 24    | "not immediate neighbors" / "no distance effects" | **Contradicted:** Coefficient is LARGEST for same ka (0.09 > 0.05 > 0.04). Closer offices respond more. Revise sentence. |
| 27    | "[] lower numbers of new hires"             | Coefficient is **positive**, contradicting "lower." Spec may need revision. |
| 30    | 4%                                          | Estimate is **3.6%**, slightly below stated 4%. |
| 32    | []% increase (low rank)                     | **74.2%** at rank 1 (雇/嘱託), p=0.065. **1.6%** at rank 2 (regular), p=0.32. Concentration is at lowest rank. |
| 35-36 | "accelerated promotion" / "[] ranks higher" | Direction is **positive** with enriched 5-level rank (+0.06-0.10 SD) but **not significant** (p>0.40). Underpowered (N=488). |


---

## Script Reference

All regressions are in: `Regressions/IntroResults_Master.R`

Original table scripts for full specifications:

- `Regressions/Table1_DraftHiring.R` (Lines 21-27, 30)
- `Regressions/Table2_GenderComposition.R` (Lines 29, 31-33)
- `Regressions/Table3_Retention.R` (Lines 37-38)

New code written for this report:

- **Line 23 (Transfer decomposition):** same-ka vs diff-ka transfer Poisson — `IntroResults_Revisions.R`
- **Line 24 (Network distance):** transfer distance decomposition + `fepois(n_transfers_distK ~ n_drafted_male | FE)` — `IntroResults_Master.R` + `IntroResults_Revisions.R`
- **Line 32 (Rank-separated female hires):** rank 1/2/3 subsamples — `IntroResults_Revisions.R`
- **Lines 35-36 (Promotion):** enriched 5-level rank + z-score standardization + `feols(rank_change ~ draft_share | FE)` — `IntroResults_Revisions.R`

