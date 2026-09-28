# Method note: PreDraftWorkerPools1944, v7

Generated from: `Regressions/PreDraftWorkerPools_v7.R` (latest); see sections below for v3–v6.  
Last run: 2026-09-28

---

## V5 changes

Keeps v4 sample, pool variables, and main OLS spec unchanged. Adds four robustness tables.

### V5 specification additions

| Table | Item | Detail |
|---|---|---|
| 1 | Confounds in S | Panel A: v4 baseline. Panel B: adds D×rank_2, D×rank_3 (rank-1 base), D×log(n_incumb42+1), log(n_incumb42+1). Rank main effects absorbed by pos_norm FE; only interactions included. n_incumb42 = linked men in same 1944 cell (proxy for own-cell 1942 size). |
| 2 | FE comparison | Panel A: kyoku×ka FE + pos_norm FE (v4 default). Panel B: kyoku FE + pos_norm FE. Both use v5 confound controls. |
| 3 | New hires by rank | Separate regressions for rank-1 / rank-2 / rank-3 cell subsamples; outcome = n_new_hires. Full sample included as reference column. Uses v5 confound controls. |
| 4 | Functional form | Panel A: D×1[S=0] + D×log(S+1)×1[S>0] with main effects (piecewise linear in S). Panel B: S terciles, D×1[T2] + D×1[T3] with tercile main effects, T1 base. |

### Inference

Every b2 estimate reports two p-values:
- **[Webb p, kyoku]**: kyoku-clustered boottest (G=6, Webb weights, B=9,999)
- **{Webb p, ka}**: ka-clustered boottest (G = n_distinct(ka_id))

Ka-cluster boottest may return NA where ka is also an FE (wrapped in tryCatch).

### Consistency check

For every spec in Tables 1–2: b2(same_ka) + b2(outside_own_ka) − b2(total_internal) reported and should be ≈ 0.

### V5 output files

| File | Content |
|---|---|
| `tables/regression_v5_table{1-4}_raw.csv` | Long-format raw results (one row per outcome×term×panel) |
| `tables/table_v5_{1-4}.csv` | Formatted wide tables |
| `tables/table_v5_{1-4}.tex` | LaTeX tables |

### b2 across four specifications — `Regressions/PreDraftWorkerPools_b2_4specs.R`

Last run: 2026-09-28. N=576, G=6 (kyoku) for specs 1–3; G=97 (ka) for spec 4 unless noted.  
Spec 1: OLS, kyoku-clustered (v4 main). Spec 2: PPML (fepois), kyoku-clustered.  
Spec 3: OLS, occupation-level pool (S = same pos_norm × ka_group linked men).  
Spec 4: OLS, ka-clustered (same regressors as spec 1).  
Stars: specs 1/3/4 from Webb boottest; spec 2 from cluster-robust Wald (fwildclusterboot 0.14.3 does not support non-linear models).

| Outcome | (1) b2 | SE | p | (2) b2 | SE | p | (3) b2 | SE | p | (4) b2 | SE | p |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Same ka | 0.060 | 0.037 | 0.155 | 0.006 | 0.007 | 0.362 | 0.100 | 0.054 | 0.138 | 0.060 | 0.036 | **0.090** |
| Other ka same kyoku | −0.051 | 0.032 | 0.173 | −0.001 | 0.010 | 0.932 | −0.061 | 0.049 | **0.033** | −0.051 | 0.042 | 0.291 |
| Other kyoku | −0.006 | 0.018 | 0.867 | −0.012 | 0.006 | **0.055** | 0.000 | 0.005 | 0.943 | −0.006 | 0.015 | 0.757 |
| Outside own ka | −0.056 | 0.017 | 0.112 | −0.006 | 0.008 | 0.500 | −0.060 | 0.048 | **0.009** | −0.056 | 0.042 | 0.251 |
| Total internal | 0.003 | 0.025 | 0.896 | −0.003 | 0.005 | 0.544 | 0.040 | 0.041 | 0.414 | 0.003 | 0.060 | 0.963 |
| N | 576 | | | 558† | | | 576 | | | 576 | | |
| Clusters | 6 | | | 6 | | | 6 | | | 97 | | |

†PPML N for n_total_internal; other outcomes drop more cells (n_same_section: N=240, n_same_department: N=461, n_different_department: N=432, n_outside_own_ka: N=558).  
Spec 3 pool: mean S_pos=2.39 vs mean S_i=24.2 in spec 1; 427/576 cells (74%) have S_pos=0.  
Output files: `tables/b2_4specs_raw.csv`, `tables/b2_4specs.csv`, `tables/b2_4specs.tex`.

---

## V7 changes — `Regressions/PreDraftWorkerPools_v7.R`

Last run: 2026-09-28.  
Baseline pool switched to **1942 ka assignment**. All five outcomes re-run across five columns.

### Pool definition (v7)

S_i = linked men (1942 ∩ 1944), placed by their **1942** ka_group, at rank = cell_rank or cell_rank−1.  
Cell exclusion: men whose 1942 ka_group = cell_ka_group AND who land in cell i (office × kakari × pos_norm) in 1944.  
Spec-3 pos_norm exclusion: additionally requires the man's 1942 pos_norm = cell's 1944 pos_norm (pool defined by ka × pos_norm pair).

| Item | Value |
|---|---:|
| Linked men in 1942 (defined rank) | 3,131 |
| Mean S_i | 21.61 |
| % S_i = 0 | 57.8% |
| Total cell exclusions removed | 2,203 |
| Mean exclusion per affected cell | 2.1 |
| Mean S_pos (spec 3) | 1.54 |
| % S_pos = 0 (spec 3) | 90.3% |

### Columns

| Col | Model | Cluster | Note |
|---|---|---|---|
| (1) | OLS | ka (G=97) | b2 = D×log(S+1) |
| (2) | PPML | ka (G=22–92) | OLS-scale APE derivative; native coef in v7_ppml_diag.csv |
| (3) | OLS pos_norm pool | ka (G=97) | S = same pos_norm × 1942 ka |
| (4) | OLS spline | ka (G=97) | Panel A = slope; Panel B = level |
| (5) | OLS | kyoku (G=6) | b2 = D×log(S+1) |

### PPML diagnostics

All converged. Same separation pattern as v6 (n_same_section N=240, others 432–558).

| Outcome | N_ppml | N_dropped | G_ka | b2_native | SE | p_wald | μ̄ | b2_ols_scale |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Same ka | 240 | 336 | 22 | −0.013 | 0.016 | 0.429 | 2.20 | −0.028 |
| Other ka same kyoku | 461 | 115 | 73 | −0.024 | 0.011 | **0.039** | 1.31 | −0.031 |
| Other kyoku | 432 | 144 | 65 | −0.009 | 0.006 | 0.136 | 0.73 | −0.007 |
| Outside own ka | 558 | 18 | 92 | −0.013 | 0.009 | 0.142 | 1.65 | −0.022 |
| Total internal | 558 | 18 | 92 | −0.014 | 0.008 | **0.065** | 2.60 | −0.037 |

### V7 main table

**Panel A** (D×log(S+1) for cols 1–3, 5; D×log(S+1)×1[S>0] for col 4):

| Outcome | (1) OLS ka | SE | p | (2) PPML ka | SE | p | (3) pos_norm ka | SE | p | (4) spline ka | SE | p | (5) OLS kyoku | SE | p |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Same ka | **0.107** | 0.020 | **0.000** | −0.028 | 0.035 | 0.429 | 0.054 | 0.015 | 0.137 | **0.108** | 0.015 | **0.003** | **0.107** | 0.017 | **0.042** |
| Other ka same kyoku | **−0.067** | 0.028 | **0.033** | **−0.031** | 0.015 | **0.039** | −0.028 | 0.019 | 0.121 | **0.020** | 0.009 | **0.019** | **−0.067** | 0.024 | **0.090** |
| Other kyoku | −0.016 | 0.013 | 0.444 | −0.007 | 0.005 | 0.136 | 0.022 | 0.010 | 0.256 | **0.023** | 0.005 | **0.013** | −0.016 | 0.016 | 0.738 |
| Outside own ka | **−0.083** | 0.024 | **0.004** | −0.022 | 0.015 | 0.142 | −0.006 | 0.022 | 0.797 | **0.043** | 0.012 | **0.010** | **−0.083** | 0.012 | **0.054** |
| Total internal | 0.024 | 0.036 | 0.523 | **−0.037** | 0.020 | **0.065** | 0.048 | 0.025 | 0.398 | **0.151** | 0.021 | **0.003** | 0.024 | 0.019 | 0.162 |
| N | 576 | | | 558 | | | 576 | | | 576 | | | 576 | | |
| G | 97 | | | 92 | | | 97 | | | 97 | | | 6 | | |

**Panel B** (col 4 only: D×1[S=0]):

| Outcome | (4) D×1[S=0] | SE | p |
|---|---:|---:|---:|
| Same ka | −0.047 | 0.024 | **0.057** |
| Other ka same kyoku | **0.286** | 0.103 | **0.000** |
| Other kyoku | **0.132** | 0.069 | **0.064** |
| Outside own ka | **0.418** | 0.077 | **0.000** |
| Total internal | **0.371** | 0.081 | **0.001** |

Stars: (1)(3)(4)(5) Webb boottest p (B=9,999, ka cluster for 1–4, kyoku for 5); (2) cluster-robust Wald p.  
Output: `tables/v7_main_raw.csv`, `tables/v7_main.csv`, `tables/v7_main.tex`, `tables/v7_ppml_diag.csv`.

---

## Spline + ka-year check — `Regressions/PreDraftWorkerPools_spline_ka.R`

Last run: 2026-09-28.

### (A) Spline Panel A — all five outcomes, ka cluster

D×1[S=0] + D×log(S+1)×1[S>0] with main effects (S_zero, log_S_nonzero) and pool controls.  
Same sample (N=576), FE (ka_id × pos_norm), and 1944-ka pool as v6 baseline.  
Ka cluster (G=97), Webb boottest (B=9,999).

| Outcome | D×1[S=0] | SE | p | D×log(S+1)×1[S>0] | SE | p |
|---|---:|---:|---:|---:|---:|---:|
| Same ka | 0.110 | 0.185 | 0.727 | **0.098** | 0.013 | **0.000** |
| Other ka same kyoku | 0.302 | 0.424 | 0.886 | **0.026** | 0.011 | **0.015** |
| Other kyoku | −0.033 | 0.051 | 0.526 | **0.026** | 0.007 | **0.008** |
| Outside own ka | 0.269 | 0.425 | 0.905 | **0.053** | 0.016 | **0.007** |
| Total internal | 0.380 | 0.591 | 0.886 | **0.151** | 0.018 | **0.001** |
| N | 576 | | | | | |
| G (ka) | 97 | | | | | |

D×1[S=0] is insignificant for all outcomes. Slope D×log(S+1)×1[S>0] is significant for all outcomes.  
Output: `tables/spline_ka_raw.csv`, `tables/spline_ka.csv`, `tables/spline_ka.tex`.

### (B) Ka-assignment year check — cols 1 and 4, linear spec

S_i is built from linked men at their **1944** ka position (v4/v6 default; confirmed from `men44_linked` at `year_num==1944`).  
Alternative: linked men at their **1942** ka position. No cell exclusion for 1942 pool (1944 cell IDs absent in 1942 data).

Pool summary:

| Pool | Mean S_i | % S_i = 0 | Linked men used |
|---|---:|---:|---:|
| 1944 ka | 24.16 | 14.1% | 3,332 |
| 1942 ka | 22.49 | 57.6% | 3,131 |

| Outcome | (1) kyoku, 1944ka | SE | p | (1) kyoku, 1942ka | SE | p | (4) ka, 1944ka | SE | p | (4) ka, 1942ka | SE | p |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Same ka | 0.060 | 0.037 | 0.155 | **0.111** | 0.018 | **0.027** | 0.060 | 0.036 | **0.090** | **0.111** | 0.017 | **0.000** |
| Other ka same kyoku | −0.051 | 0.032 | 0.173 | −0.061 | 0.024 | **0.085** | −0.051 | 0.042 | 0.291 | −0.061 | 0.025 | **0.041** |
| Other kyoku | −0.006 | 0.018 | 0.867 | −0.015 | 0.016 | 0.453 | −0.006 | 0.015 | 0.757 | −0.015 | 0.013 | 0.229 |
| Outside own ka | −0.056 | 0.017 | 0.112 | −0.076 | 0.016 | **0.077** | −0.056 | 0.042 | 0.247 | **−0.076** | 0.021 | **0.001** |
| Total internal | 0.003 | 0.025 | 0.896 | 0.035 | 0.023 | **0.087** | 0.003 | 0.060 | 0.965 | 0.035 | 0.029 | 0.247 |
| N | 576 | | | 576 | | | 576 | | | 576 | | |
| G | 6 | | | 6 | | | 97 | | | 97 | | |

Output: `tables/ka_year_check_raw.csv`, `tables/ka_year_check.csv`, `tables/ka_year_check.tex`.

---

## V6 changes — `Regressions/PreDraftWorkerPools_b2_v6.R`

Last run: 2026-09-28.  
Re-runs cols 2 (PPML) and 3 (OLS pos_norm pool) from b2_4specs with ka clustering; adds PPML diagnostics and col 3 consistency check. Cols 1 (OLS, kyoku) and 4 (OLS, ka) are identical to b2_4specs.

### Clustering changes

| Col | Model | Cluster (4specs) | Cluster (v6) | p-value source |
|---|---|---|---|---|
| 1 | OLS | kyoku (G=6) | kyoku (G=6) | Webb boottest |
| 2 | PPML | kyoku (G=6) | ka (G=92–73, varies) | Wald (boottest unsupported for PPML) |
| 3 | OLS pos_norm pool | kyoku (G=6) | ka (G=97) | Webb boottest |
| 4 | OLS | ka (G=97) | ka (G=97) | Webb boottest |

Col 2 main table shows OLS-scale APE derivative: b2_ols_scale = b2_ppml × mean(fitted values). SE: delta method treating mean(fitted) as fixed.

### PPML diagnostics

All outcomes converged (convStatus=TRUE). Separation (zero-sum FE groups) removed before fitting; cells after removal reported as N_ppml.

| Outcome | N_base | N_ppml | N_dropped | Converged | Iter | G_ka | b2_native | SE_native | p_wald | μ̄ | b2_ols_scale | b2_ols_on_ppml |
|---|---:|---:|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Same ka | 576 | 240 | 336 | TRUE | 6 | 22 | 0.006 | 0.008 | 0.455 | 2.20 | 0.014 | 0.040 |
| Other ka same kyoku | 576 | 461 | 115 | TRUE | 7 | 73 | −0.001 | 0.016 | 0.960 | 1.31 | −0.001 | −0.058 |
| Other kyoku | 576 | 432 | 144 | TRUE | 7 | 65 | −0.012 | 0.013 | 0.345 | 0.73 | −0.009 | −0.015 |
| Outside own ka | 576 | 558 | 18 | TRUE | 6 | 92 | −0.006 | 0.013 | 0.662 | 1.65 | −0.009 | −0.060 |
| Total internal | 576 | 558 | 18 | TRUE | 6 | 92 | −0.003 | 0.008 | 0.717 | 2.60 | −0.007 | −0.003 |

n_same_section drops 336/576 cells (58%) due to zero-sum separation; its PPML G_ka=22 only. Other outcomes lose ≤144 cells.  
OLS-on-same-sample (b2_ols_on_ppml): for outside_own_ka = −0.060 (close to OLS main spec −0.056); for same_section = 0.040 vs OLS 0.060. PPML functional form vs OLS separable from sample selection differences for most outcomes.

### Col 3 check (OLS pos_norm pool, ka cluster, G=97)

| Outcome | b2 | SE | t (analytical) | p_feols | p_boot | \|diff\| | Flag |
|---|---:|---:|---:|---:|---:|---:|---|
| Same ka | 0.100 | 0.041 | 2.43 | 0.017 | 0.087 | 0.070 | ok |
| Other ka same kyoku | −0.061 | 0.035 | −1.72 | 0.088 | 0.169 | 0.080 | ok |
| Other kyoku | 0.000 | 0.013 | 0.02 | 0.983 | 0.992 | 0.009 | ok |
| Outside own ka | −0.060 | 0.045 | −1.34 | 0.184 | 0.312 | **0.128** | **FLAG** |
| Total internal | 0.040 | 0.046 | 0.86 | 0.391 | 0.471 | 0.080 | ok |

**Outside own ka flag**: p_feols=0.184, p_boot=0.312, diff=0.128. Boottest issued "confidence inversion failed" warning (poorly chosen starting values). With G=97 clusters and t≈−1.34, both p-values are in the non-rejection region; inference conclusion unchanged. The discrepancy does not reverse any significance judgement.

### V6 main table (b2, OLS-scale APE for col 2)

| Outcome | (1) OLS, kyoku | SE | p | (2) PPML APE, ka | SE | p | (3) OLS pos_norm, ka | SE | p | (4) OLS, ka | SE | p |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Same ka | 0.060 | 0.037 | 0.155 | 0.014 | 0.018 | 0.455 | 0.100 | 0.041 | 0.087* | 0.060 | 0.036 | **0.090**† |
| Other ka same kyoku | −0.051 | 0.032 | 0.173 | −0.001 | 0.021 | 0.960 | −0.061 | 0.035 | 0.169 | −0.051 | 0.042 | 0.291 |
| Other kyoku | −0.006 | 0.018 | 0.867 | −0.009 | 0.009 | 0.345 | 0.000 | 0.013 | 0.992 | −0.006 | 0.015 | 0.757 |
| Outside own ka | −0.056 | 0.017 | 0.112 | −0.009 | 0.021 | 0.662 | −0.060 | 0.045 | 0.312† | −0.056 | 0.042 | 0.247 |
| Total internal | 0.003 | 0.025 | 0.896 | −0.007 | 0.021 | 0.717 | 0.040 | 0.046 | 0.471 | 0.003 | 0.060 | 0.965 |
| N | 576 | | | 558‡ | | | 576 | | | 576 | | |
| G | 6 | | | 92‡ | | | 97 | | | 97 | | |

†p from Webb boottest (G=97). ‡N and G for n_total_internal; same_section uses N=240, G=22.  
Stars: (1) Webb kyoku; (2) Wald ka; (3)(4) Webb ka. (* 0.10, ** 0.05, *** 0.01).  
Output files: `tables/b2_4specs_v6_raw.csv`, `tables/b2_4specs_v6.csv`, `tables/b2_4specs_v6.tex`, `tables/b2_ppml_native_v6.csv`, `tables/b2_ppml_native_v6.tex`, `tables/ppml_diagnostics_v6.csv`, `tables/b2_col3_check_v6.csv`.

---

### V5 results (b2 = D_i × log S_i+1)

Last run: 2026-09-28. N=576, G_kyoku=6, G_ka=97 unless noted.  
p_k = Webb boottest kyoku-cluster; p_ka = Webb boottest ka-cluster.

**Table 1 — b2 summary across confound panels**

| Panel | Outcome | b2 | SE | p_k | p_ka |
|---|---|---:|---:|---:|---:|
| A: Baseline | Same ka | 0.060 | 0.037 | 0.151 | 0.091 |
| A: Baseline | Outside own ka | −0.056 | 0.017 | 0.118 | 0.251 |
| A: Baseline | Total internal | 0.003 | 0.025 | 0.897 | 0.964 |
| A: Baseline | New hires | **0.099** | 0.013 | **0.019** | 0.278 |
| B: With confounds | Same ka | 0.032 | 0.022 | **0.035** | 0.338 |
| B: With confounds | Outside own ka | −0.111 | 0.047 | 0.141 | 0.121 |
| B: With confounds | Total internal | −0.079 | 0.057 | 0.228 | 0.372 |
| B: With confounds | New hires | **0.121** | 0.025 | **0.027** | 0.107 |

Note: VCOV matrix non-positive-semi-definite for all confound specs (fixest eigenvalue floor fix applied). Reflects near-multicollinearity among confound interactions and FEs at G=6.

**Table 2 — b2 summary across FE variants (with confounds)**

| Panel | Outcome | b2 | SE | p_k | p_ka |
|---|---|---:|---:|---:|---:|
| A: kyoku×ka FE | Same ka | 0.032 | 0.022 | 0.036 | 0.341 |
| A: kyoku×ka FE | Outside own ka | −0.111 | 0.047 | 0.141 | 0.127 |
| A: kyoku×ka FE | Total internal | −0.079 | 0.057 | 0.231 | 0.370 |
| A: kyoku×ka FE | New hires | **0.121** | 0.025 | **0.024** | 0.113 |
| B: kyoku FE | Same ka | 0.015 | 0.048 | 0.804 | 0.663 |
| B: kyoku FE | Outside own ka | −0.133 | 0.065 | 0.132 | **0.065** |
| B: kyoku FE | Total internal | −0.118 | 0.110 | 0.746 | 0.277 |
| B: kyoku FE | New hires | **0.104** | 0.023 | **0.023** | **0.090** |

Panel B: N=583 (7 additional cells recover after dropping ka FE singletons). Kyoku-bootstrap not identified when kyoku is the FE; kyoku-cluster SE from feols used for Panel B p_feols.

**Table 3 — b2 for new hires by rank subsample (with confounds excl. rank dummies)**

| Rank subsample | N | G_kyoku | G_ka | b2 | SE | p_k | p_ka |
|---|---:|---:|---:|---:|---:|---:|---:|
| Rank 1 | 25 | 4 | 10 | −0.415 | 0.287 | NA | NA |
| Rank 2 | 373 | 6 | 90 | **0.128** | 0.025 | **0.026** | 0.159 |
| Rank 3 | 114 | 6 | 33 | 0.020 | 0.029 | 0.565 | 0.580 |
| All | 576 | 6 | 97 | **0.121** | 0.025 | **0.026** | 0.109 |

Rank-1 boottest failed ("contrasts need ≥2 levels"); G_kyoku=4 after singleton removal from 58 cells. Pool controls log_P_sameKyoku and log_P_otherKyoku dropped by fixest as collinear in all rank subsamples.

**Table 4 — Functional form: key interaction coefficients**

*Panel A (spline): D×1[S=0] and D×log(S+1)×1[S>0]*

| Outcome | b(D×1[S=0]) | SE | p_k | b(D×logS×1[S>0]) | SE | p_k | p_ka |
|---|---:|---:|---:|---:|---:|---:|---:|
| Same ka | 0.110 | 0.100 | 0.410 | **0.098** | 0.012 | 0.070 | **0.000** |
| Outside own ka | 0.269 | 0.412 | 0.790 | 0.053 | 0.023 | 0.143 | **0.006** |
| Total internal | 0.380 | 0.482 | 0.604 | **0.151** | 0.029 | 0.079 | **0.001** |
| New hires | 0.269 | 0.180 | 0.376 | 0.074 | 0.014 | 0.171 | 0.093 |

*Panel B (terciles): D×1[T2] and D×1[T3], T1 base*

| Outcome | b1(D, T1) | p_k | b(D×T2) | p_k | b(D×T3) | p_k |
|---|---:|---:|---:|---:|---:|---:|
| Same ka | 0.112 | 0.089 | 0.277 | 0.320 | **0.255** | **0.045** |
| Outside own ka | 0.290 | 0.222 | −0.053 | 0.775 | −0.058 | 0.611 |
| Total internal | 0.403 | 0.107 | 0.224 | 0.381 | **0.197** | 0.072 |
| New hires | 0.164 | 0.093 | −0.161 | 0.280 | 0.151 | 0.208 |

**Additivity check: b2(same_ka) + b2(outside_own_ka) = b2(total_internal)**

| Panel | diff |
|---|---:|
| A_baseline | +5.7e-15 |
| B_confounds | +4.4e-15 |
| A_kyoku_ka_FE | +4.4e-15 |
| B_kyoku_FE | +6.7e-16 |

---

## V4 changes (prior version)

Generated from: `MainResults/PreDraftWorkerPools1944/code/analysis_v4.R`  
Last run: 2026-09-28

---

## V4 changes

| Change | Detail |
|---|---|
| Sample | Former-Shi offices only (unchanged from v3) |
| Pool variable `S_i` | Replaced `cumul_n_male` (all pre-1944 male incumbents). Now: men in same `ka_group` observed in **both 1942 and 1944** (linked), at cell's rank or one rank below, excluding cell i itself |
| `P_sameKyoku_i` | Same pool definition in other ka of same kyoku |
| `P_otherKyoku_i` | Same pool definition in other kyoku |
| Outcomes added | `n_outside_own_ka` = `n_same_department + n_different_department` |
| Model | Added interaction `D_i × log(S_i+1)` (b2) and replaced single pool control with `log(S_i+1)` (b3), `log(P_sameKyoku+1)` (g1), `log(P_otherKyoku+1)` (g2) |
| Output | `tables/regression_v4_raw.csv`, `tables/table_v4.csv`, `tables/table_v4.tex` |

### Estimated equation

$$y_i = b_1 D_i + b_2 D_i \log(S_i+1) + b_3 \log(S_i+1) + \gamma_1 \log(P_i^{\text{sameKyoku}}+1) + \gamma_2 \log(P_i^{\text{otherKyoku}}+1) + \alpha_{ka} + \alpha_{\text{pos}} + \varepsilon_i$$

### Sample checks

| Item | Value |
|---|---:|
| Former-Shi cells (before singleton removal) | 587 |
| Cells used in regression (after singleton removal) | 576 |
| Total drafted men in sample | 1,167 |
| Cells with S_i = 0 | 83 (14.1%) |
| Cells with no ka_group (cell_ka_group NA) | 14 |
| Mean S_i | 24.2 |
| Mean P_sameKyoku | 321.5 |
| Mean P_otherKyoku | 2,373.1 |
| G (kyoku clusters) | 6 |

**Kyoku in sample**: infrastructure (182 cells), economy (137), transport (126), water (93), education (30), port (19). G = 6 rather than 8 because `assembly` (市会/府会事務局) and `reconstruction` (復興事業局) have no cells with observed kyoku + ka in the 1944 former-Shi panel.

### b2 summary (D_i × log S_i + 1) across outcomes

| Outcome | b2 | SE | p_feols | p_boot (Webb) |
|---|---:|---:|---:|---:|
| Same ka | 0.060 | 0.037 | 0.167 | 0.151 |
| Same kyoku other ka | −0.051 | 0.032 | 0.176 | 0.165 |
| Other kyoku | −0.006 | 0.018 | 0.762 | 0.878 |
| Outside own ka | −0.056 | 0.017 | 0.021 | 0.116 |
| Total internal | 0.003 | 0.025 | 0.907 | 0.901 |
| New hires | **0.099** | 0.013 | 0.001 | **0.020** |

Hypothesis b2 < 0 for "other kyoku" and "outside own ka": signs are consistent (−0.006, −0.056) but neither survives Webb bootstrap (p_boot = 0.878, 0.116). The only significant b2 is for new hires: **b2 = 0.099, p_boot = 0.020** — positive, opposite direction to hypothesis.

---

## V3 (prior version)

Generated from: `MainResults/PreDraftWorkerPools1944/code/analysis_v3.R`
Last run: 2026-09-28

## Changes from v2

| # | Change | Why |
|---|---|---|
| 1 | Cell unit changed to `office_id × kakari × pos_norm` | Match Table 3 exactly |
| 2 | Normalization / arrival classification sourced from existing scripts | No reimplementation |
| 3 | Inference replaced with `fwildclusterboot::boottest` (Webb, B=9,999) | Remove custom WCS |
| 4 | Shi/Fu flag added (staff-linkage definition) | Robustness subsample |

## Existing-code inventory

| Table | Script | Output | Definitions reused |
|---|---|---|---|
| 3 | `Regressions/NewTable1c_TransferType.R:55–126` | — | `normalize_kyoku()`, `assign_kyoku_group()`, ka_group join |
| 3 | `MainResults/ByDistance/code/ByDistance_DraftKakari_Regressions.R:177–249` | `bydistance_draftkakari_coefficients.csv` | arrival_type classification, panel construction (cell × outcome × pool), `run_model` skeleton |

Loaded via `readLines` + `eval(parse(text=...))` in the same pattern as `load_department_helpers()` in ByDistance.

## Data and variables

Source: `Tokyo_Personnel_Master_All_Years.csv`, read with `locale(encoding="UTF-8")`.

**Cell unit**: `office_id × kakari × pos_norm` — exact match to ByDistance / Table 3.
`pos_norm` = `position` with all whitespace stripped.

**Arrival classification** (from `ByDistance_DraftKakari_Regressions.R:177–186`):
- `same_section`: same `ka_group` at origin and destination (or same raw `ka` string when ka_group missing)
- `same_department`: same `kyoku_group`, different ka
- `different_department`: different `kyoku_group`
- NA: missing `kyoku_group` at either endpoint

**`is_new_hire`**: `year_num == first_year` (first appearance in dataset), excluding the first observed office-year of the destination office. Matches ByDistance / Table 7.

**Kyoku normalization**: `normalize_kyoku()` regex map → `assign_kyoku_group()` (transport, water, port, education, infrastructure, economy, welfare, assembly, finance_admin, reconstruction, citizen, defense).

**Ka group**: left-join to `Regressions/ka_group_map.csv` on `(year, kyoku_clean, ka_clean)`. Map covers 1934–1944.

**Incumbent pool** (`cumul_n_male`): count of distinct male `staff_id` in years < 1944 at the same `office_id × kakari × pos_norm`. Only 1944 cross-section; no lagged year FE.

**Treatment** (`n_drafted_male`): drafted men in the destination cell in 1944 (`drafted == TRUE`, `!is_female`).

## Sample (1944 cross-section)

| Item | Value |
|---|---:|
| Former-Shi offices (1942 TokyoShi office_ids) | 230 |
| 1944 TokyoTo offices | 423 |
| Direct office_id overlap across merger | **18** (≈4%; IDs renumbered at 1943 merger) |
| Former-Shi offices (staff-linkage, > 50% linked workers from TokyoShi) | 373 of 423 |
| Raw panel cells (`office_id × kakari × pos_norm`, 1944) | 664 |
| Cells with observed ka_id (regression sample) | 664 |
| Cells after iterative singleton removal | **654** |
| Kyoku clusters (normalized) | **6** |

### Shi/Fu breakdown

| Category | Cells | Offices | Kyoku | Mean pool | Mean drafted | Mean new hires |
|---|---:|---:|---:|---:|---:|---:|
| Former-Shi | 640 | 152 | 6 | 2.59 | 1.92 | 0.522 |
| Former-Fu / new | 24 | 16 | 6 | 0.00 | 0.17 | 0.042 |

**Note**: 96% of cells are former-Shi. The former-Fu/new group has zero pre-draft male incumbents, implying `cumul_n_male = 0` throughout — these offices had no comparable pre-merger workforce pool. A differential Shi vs. Fu comparison is uninformative with this sample split.

## Estimation

Single specification (no robustness variants in this run):

```
feols(y ~ n_drafted_male + log(cumul_n_male + 1) | ka_id_int + pos_norm_int,
      data = d, cluster = ~kyoku_cl_int)
```

Fixed effects: `ka_id` (kyoku × ka) and `pos_norm`. Year FE omitted (1944 only; would be collinear with intercept). Integer IDs used for FE and cluster variables to avoid fwildclusterboot 0.14.3 internal encoding error on Japanese strings.

**Bootstrap**: `fwildclusterboot::boottest(param = "n_drafted_male", B = 9,999, type = "webb")`. Cluster = kyoku (G = 6). Null imposed by default. Webb 6-point weights.

## Validation

Before main results, `n_total_internal` was estimated with `cluster = ~office_id` (matching ByDistance) to verify the pipeline:

| Item | Value |
|---|---:|
| Coefficient `n_drafted_male` (1944 only, office_id cluster) | **0.612** |
| SE | 0.064 |
| p (office_id cluster) | 1.4×10⁻¹⁷ |
| ByDistance reference (all years 1938–1945, year FE, office_id cluster) | 0.408 (SE 0.076) |
| Boottest p (kyoku cluster, B=9,999) | **0.043** |

The 1944-only estimate (0.612) exceeds the full-panel estimate (0.408) because the year FE in ByDistance absorbs substantial cross-year variation. Direction is consistent. Boottest completed without error — validated.

## Main results (v3, feols, Webb boottest, G = 6 kyoku)

### Full sample (N = 654 cells)

| Outcome | Estimate | SE | p (feols) | p (boot) |
|---|---:|---:|---:|---:|
| n_total_internal | **0.612** | 0.097 | 0.0015 | **0.040** |
| n_same_section | **0.363** | 0.046 | 0.0005 | **0.005** |
| n_same_department | 0.139 | 0.098 | 0.213 | 0.056 |
| n_different_department | **0.110** | 0.013 | 0.0004 | **0.015** |
| n_new_hires | 0.236 | 0.083 | 0.036 | 0.200 |

### Former-Shi subsample (N = 630 cells; 96% of full sample)

| Outcome | Estimate | SE | p (feols) | p (boot) |
|---|---:|---:|---:|---:|
| n_total_internal | **0.611** | 0.098 | 0.0016 | **0.039** |
| n_same_section | **0.360** | 0.046 | 0.0005 | **0.006** |
| n_same_department | 0.140 | 0.099 | 0.217 | **0.048** |
| n_different_department | **0.111** | 0.013 | 0.0004 | **0.015** |
| n_new_hires | 0.233 | 0.083 | 0.037 | 0.203 |

Estimates are near-identical across samples (as expected given 96% overlap). The former-Shi split does not yield a meaningful differential test with the available data.

## Five-line summary (v3)

1. **Total internal replacement is strongly positive (0.61, p_boot = 0.04).** Each additional drafted man predicts 0.61 more total internal arrivals into the same cell — consistent with internal labor markets absorbing draft shocks.

2. **Same-section replacement drives the result (0.36, p_boot = 0.005).** Workers from the same ka group (immediate organizational neighborhood) account for the majority of the total internal response. Distance decays sharply within the organization.

3. **Same-department (0.14) is positive but marginal (p_boot = 0.056 full / 0.048 Shi).** Cross-ka transfers within the same kyoku group contribute to replacement but less reliably than same-section moves.

4. **Different-department effect is positive and significant (0.11, p_boot = 0.015)**, but small relative to same-section. Organizational distance limits replacement: more drafted cells draw proportionally more from proximate sources.

5. **New hires are not significantly increased (0.24, p_boot = 0.20).** External hiring does not respond to draft pressure in proportion to internal moves. Cells with larger pre-draft incumbent pools replace primarily internally rather than externally.

## Technical notes

### fwildclusterboot installation (R 4.5.0, Windows)
Package not on CRAN for R 4.5. Installed:
1. `summclust` binary from `https://s3alfisc.r-universe.dev`
2. Dependencies (`dqrng`, `gtools`) from `https://cran.r-project.org`
3. `fwildclusterboot` 0.14.3 binary from `https://s3alfisc.r-universe.dev`

### Japanese string encoding in boottest
`boottest.fixest` 0.14.3 internally calls `stringi` regex on fixed-effect and cluster level names. On Windows R 4.5.0, Japanese strings produce "Character encoding must be UTF-8, Latin-1 or bytes" (exposed as "non-numeric argument to binary operator"). Fix: replace `ka_id` (kyoku_ka string) and `pos_norm` (Japanese occupation title) with `as.integer(factor(...))` IDs before passing to `feols` and `boottest`. Estimates are identical; only the labeling changes.

### Singleton removal
`feols` internally drops FE singletons; `boottest` cannot account for this post-hoc deletion. Fix: iterative singleton removal across `ka_id_int` and `pos_norm_int` before fitting. Removes 10 rows (664 → 654), including 6 singleton ka_id values and 4 singleton pos_norm values.

## Diagnostics carried over from v2

### Rank distribution (1944 men)
| Rank | Workers | Share |
|---|---:|---:|
| 1 (雇/囑託/臨時/見習) | 827 | 11.5% |
| 2 (mid-level) | 6,983 | 97.1% (incl. rank 2) |
| 3 (技師/事務官/長) | 811 | 11.3% |
| Missing (Unknown/兼) | 688 | 9.6% |

### Encoding fix (v2, carried forward)
Three occupations were mis-ranked before `\u` escapes:

| Title | Workers | Old rank | New rank |
|---|---:|---:|---|
| 兼 | ~187 | 2 | NA (excluded) |
| 主事 | ~41 | 2 | 3 |
| 囑託員 | 30 | stayed 1 | 1 (correct) |

## Position assignments (all 43 normalized titles)

| Position | Rank | Track | Workers 1944 |
|---|---:|---|---:|
| 主事補 | 2 | clerical | 2,159 |
| 技手補 | 2 | technical | 1,557 |
| 理工技手 | 2 | technical | 1,420 |
| 雇 | 1 | unknown | 973 |
| 技手 | 2 | technical | 751 |
| 屬 | 2 | unknown | 646 |
| 監視 | 2 | clerical | 585 |
| Unknown | NA | unknown *(excluded)* | 583 |
| 兼 | NA | unknown *(excluded)* | 303 |
| 係長 | 3 | clerical | 301 |
| 産業技手 | 2 | technical | 299 |
| 事務官 | 3 | clerical | 283 |
| 保健婦 | 2 | unknown | 216 |
| 看護婦 | 2 | unknown | 199 |
| 所長 | 3 | clerical | 142 |
| 巡視 | 2 | clerical | 121 |
| 看護員 | 2 | unknown | 98 |
| 主事 | 3 | clerical | 48 |
| 技師 | 3 | technical | 48 |
| 所長心得 | 2 | clerical | 39 |
| 囑託員 | 1 | unknown | 30 |
| 課長技師 | 3 | technical | 25 |
| 防疫監吏 | 2 | clerical | 25 |
| 書記 | 2 | clerical | 24 |
| 課長書記官 | 3 | clerical | 24 |
| 清掃監督 | 2 | clerical | 11 |
| 所長理工技師 | 3 | technical | 9 |
| 醫員 | 2 | unknown | 7 |
| 組合雇 | 1 | unknown | 6 |
| 局長 | 3 | clerical | 4 |
| 所長事務取扱 | 2 | clerical | 3 |
| 組合技師 | 3 | technical | 3 |
| 次長 | 3 | clerical | 2 |
| 看護員見習 | 1 | unknown | 2 |
| 臨時 | 1 | unknown | 2 |
| 係長青年教育官 | 2 | clerical | 1 |
| 場長 | 3 | clerical | 1 |
| 工場長理工技手 | 2 | technical | 1 |
| 支所長技手 | 2 | technical | 1 |
| 産業技手補 | 2 | technical | 1 |
| 看護婦長 | 3 | unknown | 1 |
| 藥劑員 | 2 | unknown | 1 |
| 課長 | 3 | clerical | 1 |
