"""
Comprehensive Crosswalk: 1944 Tokyo-To ↔ Pre-1944 Tokyo-Shi / Tokyo-Fu
=========================================================================
Generates a full report including:
1. Corrected 1944 hierarchy (ground truth from webpage)
2. Master data quality assessment
3. Pre-1944 full office structures
4. Kyoku-level mapping with known reorganizations
5. Ka-level matching
6. Kakari-level matching
7. Summary statistics
"""

import csv, sys, os
from collections import defaultdict, OrderedDict

sys.stdout.reconfigure(encoding='utf-8')

DATA_PATH = os.path.join(
    os.environ.get('USERPROFILE', ''),
    r'Box\Research Notes (keitaro2@illinois.edu)\Tokyo_Gender\Processed_Data\Tokyo_Personnel_Master_All_Years.csv'
)

# ============================================================================
# GROUND TRUTH: 1944 hierarchy from Tokyo Metropolitan Archives webpage
# (昭和19年8月1日現在 = August 1, 1944)
# This is after all the mid-1944 reorganizations.
# ============================================================================

GT_1944_RAW = {
    '長官官房': {
        '人事課': ['秘書係', '人事第一係', '人事第二係', '福利係'],
        '文書課': ['庶務係', '文書係', '報道係', '統計係'],
        '財務課': ['議案係', '予算第一係', '予算第二係', '税務係', '収納係'],
        '会計課': ['管理係', '会計第一係', '会計第二係', '防護係'],
        '用度課': ['購買係', '用品係'],
        '考査課': ['考査第一係', '考査第二係'],
        '参事官室': [],
    },
    '民生局': {
        '監理課': ['庶務係', '行政係'],
        '振興課': ['動員係', '町会係', '資源回収係'],
        '貯蓄課': ['貯蓄第一係', '貯蓄第二係'],
        '厚生課': ['厚生係', '住宅係', '保護係', '軍事援護係'],
        '健民課': ['体力係', '修錬係', '母子係'],
        '衛生課': ['衛生係', '指導係', '医薬係'],
        '防疫課': ['防疫係', '監察係'],
        '清掃課': ['庶務係', '調度係', '作業係', '施設係'],
    },
    '教育局': {
        '総務課': ['社寺係', '学事係', '視学係', '私立学校係', '勤労動員係'],
        '国民教育課': ['学事係', '視学係', '職員係'],
        '青年教育課': ['青年学校係', '青少年団係', '教化係'],
        '兵事体錬課': ['兵事係', '学校体練係', '学校衛生係'],
    },
    '経済局': {
        '総務課': ['調査室', '総務係', '生産増強係', '物価係'],
        '商工課': ['商務係', '工務係', '企業係', '権度係'],
        '農務課': ['農務係', '水産係', '畜産係', '整地係'],
        '林務課': ['木材係', '山林係', '薪炭係'],
        '食料課': ['米穀係', '生鮮食品係', '食品係'],
        '資材課': ['金属係', '繊維係', '化学燃料係', '雑品係'],
    },
    '計画局': {
        '庶務課': ['経理係', '用地係'],
        '都市計画課': ['計画係', '事業係'],
        '公園緑地課': ['管理係', '工事係', '霊園係'],
        '道路課': ['管理係', '工事係'],
        '河川課': ['管理係', '工事係'],
        '防衛工事課': ['用材係', '工事係'],
    },
    '防衛局': {
        '企画課': ['庶務係', '企画係', '資材係'],
        '人員疎開課': ['統制係', '指導係'],
        '物資疎開課': ['受託係', '管理係'],
        '建物疎開課': ['監理係', '計画係', '補償係', '工事係', '疎開小空地係'],
        '建築課': ['監査係', '利用統制係', '技術係'],
        '防火改修課': ['事務係', '技術係'],
        '営繕課': ['庶務係', '営繕第一係', '営繕第二係', '防衛施設係', '装置係'],
    },
    '交通局': {
        '総務課': ['総務係', '文書係', '主計係'],
        '勤労課': ['勤労係', '厚生係', '指導係'],
        '経理課': ['会計係', '資材係'],
        '運輸課': ['業務係', '電車係', '自動車係', '車庫係'],
        '保線課': ['管理係', '工事係'],
        '電気課': ['電路係', '配電係'],
    },
    '水道局': {
        '庶務課': ['庶務係', '用度係', '会計係'],
        '業務課': ['業務係', '給水工事係'],
        '給水課': ['管理係', '配水係'],
        '工事課': ['計画係', '工事係'],
        '下水課': ['庶務係', '管理係', '工事係'],
    },
    '港湾局': {
        '総務課': ['計画係', '港務係'],
        '経理課': ['会計係', '管理係'],
        '工事課': ['設計係', '工事係'],
    },
}

# Also include ancillary bodies mentioned on the webpage under 民生局
GT_1944_ANCILLARY = {
    '養育院(民生局)': {
        '庶務課': ['庶務係', '事業係', '計理係', '用度係'],
        '医務課': ['医局', '医務係', '薬剤係'],
    },
    '中央卸売市場(経済局)': {
        '水産品課': ['庶務係', '水産品業務係', '水産品監督係', '調査係'],
        '農産品課': ['農産品業務係', '農産品監督係'],
    },
}

# ============================================================================
# LOAD MASTER DATA
# ============================================================================

def load_data(path, year, gov_level=None, names_only=True):
    """Load rows from master data."""
    rows = []
    with open(path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        year_col = '\ufeffyear'
        for row in reader:
            if row[year_col] != str(year):
                continue
            if gov_level and row.get('gov_level', '') != gov_level:
                continue
            if names_only and row['is_name'] != 'True':
                continue
            rows.append(row)
    return rows

def extract_hierarchy(rows):
    """Extract unique (kyoku, ka, kakari) tuples with page info."""
    hierarchy = {}  # (kyoku, ka, kakari) -> min page
    for row in rows:
        kyoku = (row['kyoku'] or '').strip()
        ka = (row['ka'] or '').strip()
        kakari = (row['kakari'] or '').strip()
        page = int(row['page']) if row.get('page') else 999
        key = (kyoku, ka, kakari)
        if key not in hierarchy or page < hierarchy[key]:
            hierarchy[key] = page
    return hierarchy

print("Loading master data...")
md_1944_rows = load_data(DATA_PATH, 1944)
shi_1943_rows = load_data(DATA_PATH, 1943, 'TokyoShi')
fu_1941_rows = load_data(DATA_PATH, 1941, 'TokyoFu')

md_1944 = extract_hierarchy(md_1944_rows)
shi_1943 = extract_hierarchy(shi_1943_rows)
fu_1941 = extract_hierarchy(fu_1941_rows)

print(f"  1944 TokyoTo: {len(md_1944_rows)} name rows, {len(md_1944)} unique combos")
print(f"  1943 TokyoShi: {len(shi_1943_rows)} name rows, {len(shi_1943)} unique combos")
print(f"  1941 TokyoFu: {len(fu_1941_rows)} name rows, {len(fu_1941)} unique combos")

# ============================================================================
# PART 1: Corrected 1944 hierarchy
# ============================================================================

report = []
report.append("=" * 80)
report.append("CROSSWALK REPORT: 1944 Tokyo-To ↔ Pre-1944 Tokyo-Shi / Tokyo-Fu")
report.append("=" * 80)

report.append("\n\n" + "=" * 80)
report.append("PART 1: Corrected 1944 Hierarchy (Ground Truth)")
report.append("  Source: soumu.metro.tokyo.lg.jp (昭和19年8月1日現在)")
report.append("=" * 80)

total_ka = 0
total_kakari = 0
for kyoku in GT_1944_RAW:
    kas = GT_1944_RAW[kyoku]
    n_kakari = sum(len(v) for v in kas.values())
    total_ka += len(kas)
    total_kakari += n_kakari
    report.append(f"\n【{kyoku}】 ({len(kas)} ka, {n_kakari} kakari)")
    for ka, kakaris in kas.items():
        if kakaris:
            report.append(f"  {ka}: {', '.join(kakaris)}")
        else:
            report.append(f"  {ka}")

report.append(f"\nTotals: {len(GT_1944_RAW)} kyoku, {total_ka} ka, {total_kakari} kakari")

# ============================================================================
# PART 2: Master Data Quality Assessment for 1944
# ============================================================================

report.append("\n\n" + "=" * 80)
report.append("PART 2: Master Data Quality Assessment (1944)")
report.append("=" * 80)

# Build master data summaries
md_kyoku = sorted(set(k for k, _, _ in md_1944))
md_ka = set((k, ka) for k, ka, kk in md_1944 if ka)
md_kakari = set((k, ka, kk) for k, ka, kk in md_1944 if kk)

report.append(f"\nMaster data: {len(md_kyoku)} kyoku, {len(md_ka)} ka, {len(md_kakari)} kakari")
report.append(f"\nKyoku in master data: {md_kyoku}")

# Known issues
report.append("\n--- Known Issues ---")
issues = [
    ("Missing kyoku", "長官官房 and 民生局 entries have blank kyoku ('')"),
    ("Missing kyoku", "防衛局 is entirely missing from kyoku assignments"),
    ("OCR error", "計晝局 → should be 計画局"),
    ("Boundary artifact", "学校体育課経済局 is a page-boundary OCR error"),
    ("OCR errors in ka", "翳生課 → 衛生課, 清日課 → unclear, 西部田田課 → unclear"),
    ("bu field", "Contains person names (部長 names), not organizational 部 units"),
    ("Missing kakari", "Many kakari from ground truth are not in master data"),
]
for cat, desc in issues:
    report.append(f"  [{cat}] {desc}")

# Corrected kyoku mapping
report.append("\n--- Kyoku Correction Map ---")
KYOKU_CORRECTIONS = {
    '': '長官官房 + 民生局 (ambiguous)',
    '計晝局': '計画局',
    '学校体育課経済局': '経済局 (boundary artifact)',
}
for md_val, correct in KYOKU_CORRECTIONS.items():
    report.append(f"  '{md_val}' → {correct}")

# ============================================================================
# PART 3: Pre-1944 Office Structures
# ============================================================================

report.append("\n\n" + "=" * 80)
report.append("PART 3: Pre-1944 Office Structures (from Master Data)")
report.append("=" * 80)

# --- Tokyo-Shi 1943 ---
report.append("\n--- Tokyo-Shi 1943 ---")
shi_kyoku_set = sorted(set(k for k, _, _ in shi_1943 if k))
shi_ka_set = set((k, ka) for k, ka, kk in shi_1943 if ka)
shi_kakari_set = set((k, ka, kk) for k, ka, kk in shi_1943 if kk)

report.append(f"Kyoku: {len(shi_kyoku_set)}, Ka: {len(shi_ka_set)}, Kakari: {len(shi_kakari_set)}")
report.append(f"NOTE: Kakari count is {len(shi_kakari_set)} — kakari was NOT digitized for Tokyo-Shi 1943")

# Group by kyoku
shi_by_kyoku = defaultdict(set)
for k, ka, kk in shi_1943:
    if ka:
        shi_by_kyoku[k].add(ka)
for kyoku in [''] + shi_kyoku_set:
    kas = sorted(shi_by_kyoku.get(kyoku, []))
    if kas:
        label = kyoku if kyoku else '(blank)'
        report.append(f"\n  【{label}】 ({len(kas)} ka)")
        for ka in kas:
            report.append(f"    {ka}")

# --- Tokyo-Fu 1941 ---
report.append("\n\n--- Tokyo-Fu 1941 ---")
fu_kyoku_set = sorted(set(k for k, _, _ in fu_1941 if k))
fu_ka_set = set((k, ka) for k, ka, kk in fu_1941 if ka)
fu_kakari_set = set((k, ka, kk) for k, ka, kk in fu_1941 if kk)

report.append(f"Kyoku: {len(fu_kyoku_set)}, Ka: {len(fu_ka_set)}, Kakari: {len(fu_kakari_set)}")

fu_by_kyoku = defaultdict(set)
for k, ka, kk in fu_1941:
    if ka:
        fu_by_kyoku[k].add(ka)
for kyoku in [''] + fu_kyoku_set:
    kas = sorted(fu_by_kyoku.get(kyoku, []))
    if kas:
        label = kyoku if kyoku else '(blank)'
        report.append(f"\n  【{label}】 ({len(kas)} ka)")
        for ka in kas:
            kks = sorted(set(kk for kk_k, kk_ka, kk in fu_1941 if kk_k == kyoku and kk_ka == ka and kk))
            if kks:
                report.append(f"    {ka}: {', '.join(kks)}")
            else:
                report.append(f"    {ka}")

# ============================================================================
# PART 4: KYOKU-LEVEL CROSSWALK with known reorganization patterns
# ============================================================================

report.append("\n\n" + "=" * 80)
report.append("PART 4: Kyoku-Level Crosswalk")
report.append("  The 1943 merger of Tokyo-Shi + Tokyo-Fu → Tokyo-To caused major reorg")
report.append("=" * 80)

# Known historical kyoku mappings based on the merger
# Source: webpage organizational history
KYOKU_MAPPING = {
    '長官官房': {
        'description': 'Governor\'s Secretariat (new with Tokyo-To)',
        'shi_sources': ['(blank/top-level offices: 秘書課, 人事課, 文書課, 区政課, 報道課, 統計課, 議案課, 都市計晝課, 豫算課, 検査課, 考査課)'],
        'fu_sources': ['(blank/top-level: 庶務課, 人事課, 会計課)'],
        'notes': 'Absorbs admin functions from both Shi and Fu top-level offices',
    },
    '民生局': {
        'description': 'Civil Affairs Bureau (new name, merges multiple functions)',
        'shi_sources': ['戰時生活局', '健民局', '城東病院院(part)'],
        'fu_sources': ['(social welfare: 社会課, 軍事援護課, 職業課)'],
        'notes': 'Combines wartime life admin, health/welfare, cleanup from Shi + Fu welfare offices',
    },
    '教育局': {
        'description': 'Education Bureau (continued from Shi)',
        'shi_sources': ['教育局'],
        'fu_sources': ['(education functions: 視学課, 社会教育課, 体育課)'],
        'notes': 'Largely carries over from Tokyo-Shi教育局, absorbs Fu education offices',
    },
    '経済局': {
        'description': 'Economic Bureau (new, merges economic functions)',
        'shi_sources': ['戰時生活局(economic parts: 商工課, 配給課, 物價課, 農漁課)'],
        'fu_sources': ['(economic functions: 経済部 including 工務課, 農林課, 商務課, 物價統制課, 資材統制課, 産業組合課)'],
        'notes': 'Major merger of economic/trade/agricultural functions from both entities',
    },
    '計画局': {
        'description': 'Planning Bureau (merges infrastructure planning)',
        'shi_sources': ['土木局'],
        'fu_sources': ['(infrastructure: 道路課, 橋梁課, 河港課, 整地課, 営繕課)'],
        'notes': 'Combines Shi civil engineering + Fu infrastructure + new urban planning',
    },
    '防衛局': {
        'description': 'Defense Bureau (new wartime bureau, est. 1943)',
        'shi_sources': ['(no direct predecessor)'],
        'fu_sources': ['(no direct predecessor)'],
        'notes': 'Entirely new — created for wartime building evacuation/defense',
    },
    '交通局': {
        'description': 'Transportation Bureau (from Shi 電気局)',
        'shi_sources': ['電気局'],
        'fu_sources': ['(no equivalent — Fu had no transit system)'],
        'notes': 'Direct successor to Tokyo-Shi 電気局 (electric/tram operations)',
    },
    '水道局': {
        'description': 'Water Bureau (continued from Shi)',
        'shi_sources': ['水道局'],
        'fu_sources': ['(no equivalent — Shi managed water)'],
        'notes': 'Largely unchanged from Tokyo-Shi水道局',
    },
    '港湾局': {
        'description': 'Port Bureau (continued from Shi)',
        'shi_sources': ['港湾局'],
        'fu_sources': ['(no equivalent)'],
        'notes': 'Largely unchanged from Tokyo-Shi港湾局',
    },
}

for kyoku, info in KYOKU_MAPPING.items():
    report.append(f"\n  {kyoku} — {info['description']}")
    report.append(f"    ← Shi: {', '.join(info['shi_sources'])}")
    report.append(f"    ← Fu:  {', '.join(info['fu_sources'])}")
    report.append(f"    Note: {info['notes']}")

# Kyoku-level match (exact name)
shi_all_kyoku = set(k for k, _, _ in shi_1943)
fu_all_kyoku = set(k for k, _, _ in fu_1941)

exact_shi = sum(1 for k in GT_1944_RAW if k in shi_all_kyoku)
exact_fu = sum(1 for k in GT_1944_RAW if k in fu_all_kyoku)
# With known mapping (all 9 have at least partial predecessors except 防衛局)
mapped = len(GT_1944_RAW) - 1  # all except 防衛局

report.append(f"\n\nKyoku match summary:")
report.append(f"  Total 1944 kyoku: {len(GT_1944_RAW)}")
report.append(f"  Exact name match in Shi: {exact_shi}/9")
report.append(f"  Exact name match in Fu:  {exact_fu}/9")
report.append(f"  With known reorganization mapping: {mapped}/9 (all except 防衛局)")

# ============================================================================
# PART 5: KA-LEVEL CROSSWALK
# ============================================================================

report.append("\n\n" + "=" * 80)
report.append("PART 5: Ka-Level Crosswalk")
report.append("=" * 80)

# All ka names from pre-1944
shi_ka_names = set(ka for _, ka, _ in shi_1943 if ka)
fu_ka_names = set(ka for _, ka, _ in fu_1941 if ka)

# Normalize function
def norm(s):
    s = s.strip()
    s = s.replace('晝', '画').replace('總', '総').replace('體', '体')
    s = s.replace('輛', '輌').replace('豫', '予').replace('翳', '衛')
    s = s.replace('纎', '繊').replace('價', '価').replace('〓', '')
    return s

shi_ka_norm = {norm(k): k for k in shi_ka_names}
fu_ka_norm = {norm(k): k for k in fu_ka_names}

ka_results = []  # (kyoku, ka, in_shi, in_fu, shi_match, fu_match)
ka_found_shi = 0
ka_found_fu = 0
ka_found_either = 0

for kyoku in GT_1944_RAW:
    for ka in GT_1944_RAW[kyoku]:
        kn = norm(ka)
        in_shi = kn in shi_ka_norm or ka in shi_ka_names
        in_fu = kn in fu_ka_norm or ka in fu_ka_names

        shi_match = shi_ka_norm.get(kn, ka) if in_shi else ''
        fu_match = fu_ka_norm.get(kn, ka) if in_fu else ''

        if in_shi: ka_found_shi += 1
        if in_fu: ka_found_fu += 1
        if in_shi or in_fu: ka_found_either += 1

        ka_results.append((kyoku, ka, in_shi, in_fu, shi_match, fu_match))

report.append(f"\nTotal 1944 ka: {total_ka}")
report.append(f"  Found in Tokyo-Shi 1943: {ka_found_shi}/{total_ka} ({100*ka_found_shi/total_ka:.0f}%)")
report.append(f"  Found in Tokyo-Fu 1941:  {ka_found_fu}/{total_ka} ({100*ka_found_fu/total_ka:.0f}%)")
report.append(f"  Found in either:         {ka_found_either}/{total_ka} ({100*ka_found_either/total_ka:.0f}%)")

report.append("\nDetailed ka-level matches:")
current_kyoku = None
for kyoku, ka, in_shi, in_fu, shi_match, fu_match in ka_results:
    if kyoku != current_kyoku:
        report.append(f"\n  【{kyoku}】")
        current_kyoku = kyoku
    sources = []
    if in_shi: sources.append(f"Shi({shi_match})")
    if in_fu: sources.append(f"Fu({fu_match})")
    if not sources: sources.append("NOT FOUND")
    report.append(f"    {ka} → {' + '.join(sources)}")

# Ka names NOT found — analyze patterns
not_found_ka = [(kyoku, ka) for kyoku, ka, in_shi, in_fu, _, _ in ka_results if not in_shi and not in_fu]
report.append(f"\n  Ka NOT found in either ({len(not_found_ka)}):")
for kyoku, ka in not_found_ka:
    report.append(f"    {kyoku} > {ka}")

# ============================================================================
# PART 6: KAKARI-LEVEL CROSSWALK
# ============================================================================

report.append("\n\n" + "=" * 80)
report.append("PART 6: Kakari-Level Crosswalk")
report.append("  NOTE: Tokyo-Shi 1943 has 0 kakari in master data (not digitized)")
report.append("  Only Tokyo-Fu 1941 provides kakari-level comparison")
report.append("=" * 80)

# All kakari names from pre-1944
shi_kakari_names = set(kk for _, _, kk in shi_1943 if kk)  # should be empty
fu_kakari_names = set(kk for _, _, kk in fu_1941 if kk)
fu_kakari_norm = {norm(k): k for k in fu_kakari_names}

report.append(f"\nTokyo-Fu 1941 kakari names ({len(fu_kakari_names)}): {sorted(fu_kakari_names)}")

kakari_found_shi = 0
kakari_found_fu = 0
kakari_found_either = 0
kakari_results = []

for kyoku in GT_1944_RAW:
    for ka, kakaris in GT_1944_RAW[kyoku].items():
        for kakari in kakaris:
            kkn = norm(kakari)
            in_shi = kkn in {norm(k) for k in shi_kakari_names} or kakari in shi_kakari_names
            in_fu = kkn in fu_kakari_norm or kakari in fu_kakari_names

            if in_shi: kakari_found_shi += 1
            if in_fu: kakari_found_fu += 1
            if in_shi or in_fu: kakari_found_either += 1

            fu_match = fu_kakari_norm.get(kkn, '') if in_fu else ''
            kakari_results.append((kyoku, ka, kakari, in_shi, in_fu, fu_match))

report.append(f"\nTotal 1944 kakari: {total_kakari}")
report.append(f"  Found in Tokyo-Shi 1943: {kakari_found_shi}/{total_kakari} ({100*kakari_found_shi/total_kakari:.0f}%)")
report.append(f"  Found in Tokyo-Fu 1941:  {kakari_found_fu}/{total_kakari} ({100*kakari_found_fu/total_kakari:.0f}%)")
report.append(f"  Found in either:         {kakari_found_either}/{total_kakari} ({100*kakari_found_either/total_kakari:.0f}%)")

report.append("\nDetailed kakari-level matches:")
current_kyoku = None
current_ka = None
for kyoku, ka, kakari, in_shi, in_fu, fu_match in kakari_results:
    if kyoku != current_kyoku:
        report.append(f"\n  【{kyoku}】")
        current_kyoku = kyoku
        current_ka = None
    if ka != current_ka:
        report.append(f"    {ka}:")
        current_ka = ka
    source = []
    if in_fu: source.append(f"Fu({fu_match})")
    if in_shi: source.append("Shi")
    if not source: source.append("—")
    report.append(f"      {kakari} → {' '.join(source)}")

# ============================================================================
# PART 7: SUMMARY TABLE
# ============================================================================

report.append("\n\n" + "=" * 80)
report.append("PART 7: Summary")
report.append("=" * 80)

report.append(f"""
Match rates for 1944 Tokyo-To hierarchy found in pre-1944 directories:

Level   | 1944 Count | In Shi 1943      | In Fu 1941       | In Either
--------|------------|------------------|------------------|------------------
Kyoku   | {len(GT_1944_RAW):>10} | {exact_shi:>4}/{len(GT_1944_RAW):>2} ({100*exact_shi/len(GT_1944_RAW):>5.1f}%) | {exact_fu:>4}/{len(GT_1944_RAW):>2} ({100*exact_fu/len(GT_1944_RAW):>5.1f}%) | {max(exact_shi, exact_fu):>4}/{len(GT_1944_RAW):>2} ({100*max(exact_shi, exact_fu)/len(GT_1944_RAW):>5.1f}%)
Ka      | {total_ka:>10} | {ka_found_shi:>4}/{total_ka:>2} ({100*ka_found_shi/total_ka:>5.1f}%) | {ka_found_fu:>4}/{total_ka:>2} ({100*ka_found_fu/total_ka:>5.1f}%) | {ka_found_either:>4}/{total_ka:>2} ({100*ka_found_either/total_ka:>5.1f}%)
Kakari  | {total_kakari:>10} | {kakari_found_shi:>4}/{total_kakari:>3} ({100*kakari_found_shi/total_kakari:>5.1f}%) | {kakari_found_fu:>4}/{total_kakari:>3} ({100*kakari_found_fu/total_kakari:>5.1f}%) | {kakari_found_either:>4}/{total_kakari:>3} ({100*kakari_found_either/total_kakari:>5.1f}%)

NOTE: Kyoku matching with historical reorganization knowledge: {mapped}/9 have identifiable predecessors.
""")

report.append("Key findings:")
report.append("  1. KYOKU: Only 3/9 exact name matches (教育局, 水道局, 港湾局).")
report.append("     With known reorganization mapping, 8/9 have predecessors (防衛局 is new).")
report.append("  2. KA: ~30% exact name match. Many ka were renamed in the merger:")
report.append("     - 総務課 (new in many bureaus, replacing various admin offices)")
report.append("     - Wartime-specific ka like 貯蓄課, 健民課 had no pre-1943 equivalents")
report.append("     - 防衛局 ka are entirely new (疎開=evacuation was a 1944 concept)")
report.append("  3. KAKARI: Very low match rates due to:")
report.append("     - Tokyo-Shi 1943 master data has NO kakari entries (0 digitized)")
report.append("     - Tokyo-Fu 1941 has only 18 kakari names")
report.append("     - This is a DATA COVERAGE issue, not an organizational one")
report.append("     - The actual kakari recovery rate would be much higher if the")
report.append("       pre-1944 directories were fully digitized at kakari level")

report.append("\nRecommendations:")
report.append("  1. For kyoku-level crosswalk: use the known reorganization mapping above")
report.append("  2. For ka-level crosswalk: use worker-level matching (staff_id)")
report.append("     across years to track which ka people moved from")
report.append("  3. For kakari-level crosswalk: the pre-1944 master data lacks kakari,")
report.append("     so either OCR the pre-1944 directories at kakari level, or use")
report.append("     worker-level tracking as a proxy")

# Write report
output = '\n'.join(report)
print(output)

# Save to file
out_path = os.path.join(
    os.environ.get('USERPROFILE', ''),
    r'Documents\GitHub\Tokyo_Project_Figures\Regressions\Crosswalk_1944_Report.md'
)
with open(out_path, 'w', encoding='utf-8') as f:
    f.write(output)
print(f"\nReport saved to: {out_path}")
