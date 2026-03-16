# 1948 Kyoku Assignment Validation Report

**Ground truth:** 昭和23年8月1日現在 (Aug 1, 1948)  
**Data:** Tokyo_Personnel_Master_All_Years_v2.csv, year 1948

---

## 1. Kyoku-Level Mismatches

| In data, NOT in ground truth | In ground truth, NOT in data |
|------------------------------|------------------------------|
| 中央卸売市場 (should be under 経済局) | 総務部 |
| 劑務院 (**OCR error for 総務部**) | 財務部 |
| 小金井産院 (hospital; likely 衛生局) | 水道局 |
| 本所性病々院 (hospital; likely 衛生局) | |
| 養育院 (should be under 民生局) | |

**Critical:** 総務部, 財務部, 水道局 are missing from the data. Their offices appear mis-assigned elsewhere.

---

## 2. Major Office Assignment Errors

### 2a. 劑務院 = 総務部 (OCR typo)
- **234 staff** assigned to "劑務院" — should be **総務部** (総務部)
- Character 劑 (dose) vs 総 (general) — likely OCR/encoding error

### 2b. 交通局 contains 水道局 and 衛生局 offices
Offices under 交通局 in data that belong elsewhere per ground truth:
- **水道局:** 下水課, 給水課, 淀橋淨水場, 水源林事務所
- **衛生局:** 公衆衛生課, 医務課, 薬務課, 防疫課, 清掃課

### 2c. 建設局 contains 総務部 and 財務部 offices
Offices under 建設局 in data that belong elsewhere:
- **総務部:** 人事課, 文書課, 監査課, 福利課, 統計課, 調査課, 特殊財産管理課, 観光課
- **財務部:** 主税課, 経理課, 豫算課(予算課)

### 2d. Duplicate office_id across kyoku (data integrity)
- **office_id 7417** appears as 総務課 under: 交通局, 建設局, 教育局, 民生局, 経済局 (5 kyoku)
- **office_id 6597** appears under: 中央卸売市場(庶務課), 建設局(庶務課), 養育院(庶務課)
- **office_id 5980** appears under: 民生局(保護課), 養育院(保護課)
- **office_id 6508** under: 交通局(工事課), 建設局(工事課)

This suggests office_id is **not** unique to (kyoku, ka) or there is a many-to-one mapping bug.

### 2e. 労働局 structure
- Data includes 区役所 (ward offices), 保健所 (health centers), 支所, 地方事務所
- Ground truth 労働局 = HQ labor bureau (総務課, 労政課, 職業課, 労働組合課, 失業保険徴収課)
- Ward offices may belong to a different administrative layer

---

## 3. Summary: Root Causes

1. **OCR/encoding:** 劑務院 → 総務部
2. **Kyoku assignment logic:** Offices from 総務部, 財務部, 水道局, 衛生局 appear under wrong kyoku (likely 建設局, 交通局)
3. **office_id semantics:** office_id may not be kyoku-specific; shared IDs across bureaus
4. **Hierarchy level:** 中央卸売市場, 養育院, 小金井産院, 本所性病々院 as top-level kyoku vs nested under 経済局/民生局/衛生局

---

## 4. Implications for First Stage (kyoku FE)

- **Kyoku FE** relies on correct office→kyoku mapping
- Mis-assignment dilutes within-kyoku variation and can create spurious between-kyoku differences
- The large F-stat drop when adding kyoku FE is consistent with:
  - (a) True absorption of relevant variation, or
  - (b) **Noise from mis-assigned offices** — offices from different true kyoku grouped together

**Recommendation:** Fix 劑務院→総務部 and validate office_id→kyoku mapping against ground truth before trusting kyoku FE. Consider dropping kyoku FE or using a corrected kyoku variable if the mapping cannot be fixed.
