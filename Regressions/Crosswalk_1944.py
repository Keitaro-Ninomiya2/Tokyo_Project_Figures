"""
Crosswalk: 1944 Tokyo-To office hierarchy ↔ pre-1944 Tokyo-Shi / Tokyo-Fu
============================================================================
1. Ground truth for 1944 comes from the Tokyo Metropolitan Archives webpage
   (昭和19年8月1日現在 snapshot).
2. Master data hierarchy is extracted from Tokyo_Personnel_Master_All_Years.csv.
3. Pre-1944 hierarchies: Tokyo-Shi 1943, Tokyo-Fu 1941 from the same master CSV.
4. Matching is done at kakari, ka, and kyoku levels.
"""

import csv, sys, re
from collections import defaultdict

sys.stdout.reconfigure(encoding='utf-8')

# ============================================================================
# PART 1: Ground truth 1944 hierarchy from the webpage
# (昭和19年8月1日現在 = August 1, Showa 19)
# ============================================================================

GROUND_TRUTH_1944 = """
長官官房
  人事課: 秘書係、人事第一係、人事第二係、福利係
  文書課: 庶務係、文書係、報道係、統計係
  財務課: 議案係、予算第一係、予算第二係、税務係、収納係
  会計課: 管理係、会計第一係、会計第二係、防護係
  用度課: 購買係、用品係
  考査課: 考査第一係、考査第二係
  参事官室:
民生局
  監理課: 庶務係、行政係
  振興課: 動員係、町会係、資源回収係
  貯蓄課: 貯蓄第一係、貯蓄第二係
  厚生課: 厚生係、住宅係、保護係、軍事援護係
  健民課: 体力係、修錬係、母子係
  衛生課: 衛生係、指導係、医薬係
  防疫課: 防疫係、監察係
  清掃課: 庶務係、調度係、作業係、施設係
教育局
  総務課: 社寺係、学事係、視学係、私立学校係、勤労動員係
  国民教育課: 学事係、視学係、職員係
  青年教育課: 青年学校係、青少年団係、教化係
  兵事体錬課: 兵事係、学校体練係、学校衛生係
経済局
  総務課: 調査室、総務係、生産増強係、物価係
  商工課: 商務係、工務係、企業係、権度係
  農務課: 農務係、水産係、畜産係、整地係
  林務課: 木材係、山林係、薪炭係
  食料課: 米穀係、生鮮食品係、食品係
  資材課: 金属係、繊維係、化学燃料係、雑品係
計画局
  庶務課: 経理係、用地係
  都市計画課: 計画係、事業係
  公園緑地課: 管理係、工事係、霊園係
  道路課: 管理係、工事係
  河川課: 管理係、工事係
  防衛工事課: 用材係、工事係
防衛局
  企画課: 庶務係、企画係、資材係
  人員疎開課: 統制係、指導係
  物資疎開課: 受託係、管理係
  建物疎開課: 監理係、計画係、補償係、工事係、疎開小空地係
  建築課: 監査係、利用統制係、技術係
  防火改修課: 事務係、技術係
  営繕課: 庶務係、営繕第一係、営繕第二係、防衛施設係、装置係
交通局
  総務課: 総務係、文書係、主計係
  勤労課: 勤労係、厚生係、指導係
  経理課: 会計係、資材係
  運輸課: 業務係、電車係、自動車係、車庫係
  保線課: 管理係、工事係
  電気課: 電路係、配電係
水道局
  庶務課: 庶務係、用度係、会計係
  業務課: 業務係、給水工事係
  給水課: 管理係、配水係
  工事課: 計画係、工事係
  下水課: 庶務係、管理係、工事係
港湾局
  総務課: 計画係、港務係
  経理課: 会計係、管理係
  工事課: 設計係、工事係
"""

def parse_ground_truth(text):
    """Parse the ground truth text into a hierarchy dict."""
    hierarchy = []  # list of (kyoku, ka, kakari)
    current_kyoku = None
    for line in text.strip().split('\n'):
        line = line.rstrip()
        if not line:
            continue
        if not line.startswith('  '):
            # Kyoku level
            current_kyoku = line.strip()
        else:
            # Ka level (with optional kakari)
            parts = line.strip().split(': ')
            ka = parts[0].strip()
            kakaris = []
            if len(parts) > 1 and parts[1].strip():
                kakaris = [k.strip() for k in parts[1].split('、') if k.strip()]
            if kakaris:
                for kakari in kakaris:
                    hierarchy.append((current_kyoku, ka, kakari))
            else:
                hierarchy.append((current_kyoku, ka, ''))
    return hierarchy

gt_1944 = parse_ground_truth(GROUND_TRUTH_1944)

# Build sets at each level
gt_kakari_set = set()
gt_ka_set = set()
gt_kyoku_set = set()
gt_full = []  # (kyoku, ka, kakari)

for kyoku, ka, kakari in gt_1944:
    gt_kyoku_set.add(kyoku)
    gt_ka_set.add((kyoku, ka))
    if kakari:
        gt_kakari_set.add((kyoku, ka, kakari))
        gt_full.append((kyoku, ka, kakari))

print("=" * 80)
print("PART 1: 1944 Ground Truth Hierarchy (from webpage 昭和19年8月1日現在)")
print("=" * 80)
print(f"\nKyoku count: {len(gt_kyoku_set)}")
print(f"Ka count: {len(gt_ka_set)}")
print(f"Kakari count: {len(gt_kakari_set)}")
print()

for kyoku in sorted(gt_kyoku_set):
    kas = sorted(set(ka for k, ka, _ in gt_1944 if k == kyoku))
    kakaris = [(ka, kk) for k, ka, kk in gt_1944 if k == kyoku and kk]
    print(f"【{kyoku}】 {len(kas)} ka, {len(kakaris)} kakari")
    for ka in kas:
        kk_list = [kk for k2, ka2, kk in gt_1944 if k2 == kyoku and ka2 == ka and kk]
        print(f"  {ka}: {', '.join(kk_list) if kk_list else '(no kakari)'}")

# ============================================================================
# PART 2: Master data 1944 hierarchy - compare with ground truth
# ============================================================================

DATA_PATH = r'C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Tokyo_Gender/Processed_Data/Tokyo_Personnel_Master_All_Years.csv'

def load_hierarchy(path, year, gov_level=None):
    """Load unique (kyoku, ka, kakari) from master data."""
    hierarchy = set()
    with open(path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        year_col = '\ufeffyear'
        for row in reader:
            if row[year_col] != str(year):
                continue
            if gov_level and row.get('gov_level', '') != gov_level:
                continue
            if row['is_name'] != 'True':
                continue
            kyoku = (row['kyoku'] or '').strip()
            ka = (row['ka'] or '').strip()
            kakari = (row['kakari'] or '').strip()
            hierarchy.add((kyoku, ka, kakari))
    return hierarchy

print("\n\n" + "=" * 80)
print("PART 2: Master Data 1944 vs Ground Truth")
print("=" * 80)

md_1944 = load_hierarchy(DATA_PATH, 1944)
md_kakari = set((k, ka, kk) for k, ka, kk in md_1944 if kk)
md_ka = set((k, ka) for k, ka, kk in md_1944)
md_kyoku = set(k for k, ka, kk in md_1944)

print(f"\nMaster data 1944: {len(md_kyoku)} kyoku, {len(md_ka)} ka, {len(md_kakari)} kakari")

# Kyoku-level comparison
print("\n--- Kyoku comparison ---")
print(f"Ground truth kyoku: {sorted(gt_kyoku_set)}")
print(f"Master data kyoku:  {sorted(md_kyoku)}")
gt_only = gt_kyoku_set - md_kyoku
md_only = md_kyoku - gt_kyoku_set
print(f"In GT only: {sorted(gt_only)}")
print(f"In MD only: {sorted(md_only)}")

# The master data has blank kyoku for 長官官房 and 民生局 entries.
# Let's build a mapping from master data kyoku issues
print("\n--- Kyoku mapping notes ---")
print("Master data blank kyoku ('') covers: 長官官房 + 民生局 (merged in OCR)")
print("Master data '計晝局' should be '計画局' (OCR error)")
print("Master data '学校体育課経済局' is a boundary artifact")

# Map master data kyoku to ground truth kyoku
KYOKU_MAP = {
    '': None,  # ambiguous - covers 長官官房 and 民生局
    '計晝局': '計画局',
    '学校体育課経済局': '経済局',
}

# Ka-level comparison
print("\n--- Ka comparison (ignoring kyoku assignment) ---")
gt_ka_names = set(ka for _, ka in gt_ka_set)
md_ka_names = set(ka for _, ka, _ in md_1944 if ka)
print(f"GT ka names: {len(gt_ka_names)}")
print(f"MD ka names: {len(md_ka_names)}")
common = gt_ka_names & md_ka_names
print(f"Exact match: {len(common)}")
gt_ka_only = gt_ka_names - md_ka_names
md_ka_only = md_ka_names - gt_ka_names
if gt_ka_only:
    print(f"In GT only: {sorted(gt_ka_only)}")
if md_ka_only:
    print(f"In MD only (sample): {sorted(list(md_ka_only))[:30]}")

# Kakari-level comparison (name only, ignoring hierarchy)
print("\n--- Kakari comparison (name only) ---")
gt_kakari_names = set(kk for _, _, kk in gt_kakari_set)
md_kakari_names = set(kk for _, _, kk in md_kakari)
common_kk = gt_kakari_names & md_kakari_names
print(f"GT kakari names: {len(gt_kakari_names)}")
print(f"MD kakari names: {len(md_kakari_names)}")
print(f"Exact match: {len(common_kk)}")
gt_kk_only = gt_kakari_names - md_kakari_names
md_kk_only = md_kakari_names - gt_kakari_names
if gt_kk_only:
    print(f"In GT only: {sorted(gt_kk_only)}")
if md_kk_only:
    print(f"In MD only: {sorted(md_kk_only)}")

# ============================================================================
# PART 3: Pre-1944 hierarchies from master data
# ============================================================================

print("\n\n" + "=" * 80)
print("PART 3: Pre-1944 Hierarchies")
print("=" * 80)

# Tokyo-Shi 1943
shi_1943 = load_hierarchy(DATA_PATH, 1943, 'TokyoShi')
shi_kakari = set((k, ka, kk) for k, ka, kk in shi_1943 if kk)
shi_ka = set((k, ka) for k, ka, kk in shi_1943)
shi_kyoku = set(k for k, ka, kk in shi_1943)

print(f"\nTokyo-Shi 1943: {len(shi_kyoku)} kyoku, {len(shi_ka)} ka, {len(shi_kakari)} kakari")
print(f"  Kyoku: {sorted(shi_kyoku)}")

# Tokyo-Fu 1941
fu_1941 = load_hierarchy(DATA_PATH, 1941, 'TokyoFu')
fu_kakari = set((k, ka, kk) for k, ka, kk in fu_1941 if kk)
fu_ka = set((k, ka) for k, ka, kk in fu_1941)
fu_kyoku = set(k for k, ka, kk in fu_1941)

print(f"\nTokyo-Fu 1941: {len(fu_kyoku)} kyoku, {len(fu_ka)} ka, {len(fu_kakari)} kakari")
print(f"  Kyoku: {sorted(fu_kyoku)}")

# ============================================================================
# PART 4: Crosswalk - match 1944 GT hierarchy to pre-1944
# ============================================================================

print("\n\n" + "=" * 80)
print("PART 4: Crosswalk - 1944 Ground Truth → Pre-1944 Sources")
print("=" * 80)

# Strategy: For each 1944 kakari/ka/kyoku, check if it exists in:
# (a) Tokyo-Shi 1943
# (b) Tokyo-Fu 1941
# (c) Either

# Normalize names for fuzzy matching
def normalize(s):
    """Normalize Japanese text for matching."""
    s = s.strip()
    # Common OCR variants
    s = s.replace('晝', '画')  # 計晝局 → 計画局
    s = s.replace('總', '総')
    s = s.replace('體', '体')
    s = s.replace('輛', '輌')
    s = s.replace('豫', '予')
    s = s.replace('翳', '衛')
    s = s.replace('纎', '繊')
    s = s.replace('價', '価')
    return s

# --- KYOKU LEVEL ---
print("\n=== KYOKU LEVEL ===")
print(f"1944 GT kyoku: {len(gt_kyoku_set)}")

# Pre-1944 kyoku name sets (normalized)
shi_kyoku_norm = {normalize(k): k for k in shi_kyoku}
fu_kyoku_norm = {normalize(k): k for k in fu_kyoku}

# Known kyoku mapping: Tokyo-To 1944 ← Tokyo-Shi + Tokyo-Fu merger
# 長官官房 is new (governor's secretariat, replacing fu and shi administrative offices)
# 民生局 absorbs health/welfare functions
# 防衛局 is new (wartime)
# etc.

kyoku_crosswalk = {}
for kyoku in sorted(gt_kyoku_set):
    kn = normalize(kyoku)
    in_shi = kn in shi_kyoku_norm or kyoku in shi_kyoku
    in_fu = kn in fu_kyoku_norm or kyoku in fu_kyoku

    # Also check partial matches
    shi_partial = [k for k in shi_kyoku if k and (k in kyoku or kyoku in k)]
    fu_partial = [k for k in fu_kyoku if k and (k in kyoku or kyoku in k)]

    source = []
    if in_shi:
        source.append(f"Shi({shi_kyoku_norm.get(kn, kyoku)})")
    elif shi_partial:
        source.append(f"Shi~({','.join(shi_partial)})")
    if in_fu:
        source.append(f"Fu({fu_kyoku_norm.get(kn, kyoku)})")
    elif fu_partial:
        source.append(f"Fu~({','.join(fu_partial)})")

    if not source:
        source.append("NEW/NO MATCH")

    kyoku_crosswalk[kyoku] = ' + '.join(source)
    print(f"  {kyoku} → {kyoku_crosswalk[kyoku]}")

kyoku_found_shi = sum(1 for k in gt_kyoku_set if normalize(k) in shi_kyoku_norm or k in shi_kyoku)
kyoku_found_fu = sum(1 for k in gt_kyoku_set if normalize(k) in fu_kyoku_norm or k in fu_kyoku)
kyoku_found_either = sum(1 for k in gt_kyoku_set
                         if normalize(k) in shi_kyoku_norm or k in shi_kyoku
                         or normalize(k) in fu_kyoku_norm or k in fu_kyoku)

print(f"\nKyoku match rate:")
print(f"  In Tokyo-Shi 1943: {kyoku_found_shi}/{len(gt_kyoku_set)} ({100*kyoku_found_shi/len(gt_kyoku_set):.0f}%)")
print(f"  In Tokyo-Fu 1941:  {kyoku_found_fu}/{len(gt_kyoku_set)} ({100*kyoku_found_fu/len(gt_kyoku_set):.0f}%)")
print(f"  In either:         {kyoku_found_either}/{len(gt_kyoku_set)} ({100*kyoku_found_either/len(gt_kyoku_set):.0f}%)")

# --- KA LEVEL ---
print("\n=== KA LEVEL ===")
print(f"1944 GT ka: {len(gt_ka_set)}")

# Build pre-1944 ka name sets
shi_ka_names = set(ka for _, ka, _ in shi_1943 if ka)
fu_ka_names = set(ka for _, ka, _ in fu_1941 if ka)
shi_ka_norm = {normalize(k): k for k in shi_ka_names}
fu_ka_norm = {normalize(k): k for k in fu_ka_names}

ka_found_shi = 0
ka_found_fu = 0
ka_found_either = 0

print("\nKa-level crosswalk (by ka name, ignoring kyoku assignment):")
for kyoku, ka in sorted(gt_ka_set):
    kan = normalize(ka)
    in_shi = kan in shi_ka_norm or ka in shi_ka_names
    in_fu = kan in fu_ka_norm or ka in fu_ka_names

    source = []
    if in_shi:
        source.append("Shi")
        ka_found_shi += 1
    if in_fu:
        source.append("Fu")
        ka_found_fu += 1
    if in_shi or in_fu:
        ka_found_either += 1

    if not source:
        source.append("NOT FOUND")

    print(f"  {kyoku} > {ka} → {' + '.join(source)}")

print(f"\nKa match rate (by name):")
print(f"  In Tokyo-Shi 1943: {ka_found_shi}/{len(gt_ka_set)} ({100*ka_found_shi/len(gt_ka_set):.0f}%)")
print(f"  In Tokyo-Fu 1941:  {ka_found_fu}/{len(gt_ka_set)} ({100*ka_found_fu/len(gt_ka_set):.0f}%)")
print(f"  In either:         {ka_found_either}/{len(gt_ka_set)} ({100*ka_found_either/len(gt_ka_set):.0f}%)")

# --- KAKARI LEVEL ---
print("\n=== KAKARI LEVEL ===")
print(f"1944 GT kakari: {len(gt_kakari_set)}")

# Build pre-1944 kakari name sets
shi_kakari_names = set(kk for _, _, kk in shi_kakari)
fu_kakari_names = set(kk for _, _, kk in fu_kakari)
shi_kakari_norm = {normalize(k): k for k in shi_kakari_names}
fu_kakari_norm = {normalize(k): k for k in fu_kakari_names}

kakari_found_shi = 0
kakari_found_fu = 0
kakari_found_either = 0

print("\nKakari-level crosswalk (by kakari name, ignoring ka/kyoku assignment):")
for kyoku, ka, kakari in sorted(gt_kakari_set):
    kkn = normalize(kakari)
    in_shi = kkn in shi_kakari_norm or kakari in shi_kakari_names
    in_fu = kkn in fu_kakari_norm or kakari in fu_kakari_names

    source = []
    if in_shi:
        source.append("Shi")
        kakari_found_shi += 1
    if in_fu:
        source.append("Fu")
        kakari_found_fu += 1
    if in_shi or in_fu:
        kakari_found_either += 1

    if not source:
        source.append("NOT FOUND")

    print(f"  {kyoku} > {ka} > {kakari} → {' + '.join(source)}")

print(f"\nKakari match rate (by name):")
print(f"  In Tokyo-Shi 1943: {kakari_found_shi}/{len(gt_kakari_set)} ({100*kakari_found_shi/len(gt_kakari_set):.0f}%)")
print(f"  In Tokyo-Fu 1941:  {kakari_found_fu}/{len(gt_kakari_set)} ({100*kakari_found_fu/len(gt_kakari_set):.0f}%)")
print(f"  In either:         {kakari_found_either}/{len(gt_kakari_set)} ({100*kakari_found_either/len(gt_kakari_set):.0f}%)")

# ============================================================================
# PART 5: Summary
# ============================================================================

print("\n\n" + "=" * 80)
print("SUMMARY: Share of 1944 hierarchy found in pre-1944 directories")
print("=" * 80)

print(f"""
Level     | 1944 Count | In Shi 1943     | In Fu 1941      | In Either
----------|------------|-----------------|-----------------|------------------
Kyoku     | {len(gt_kyoku_set):>10} | {kyoku_found_shi:>4}/{len(gt_kyoku_set)} ({100*kyoku_found_shi/len(gt_kyoku_set):>3.0f}%) | {kyoku_found_fu:>4}/{len(gt_kyoku_set)} ({100*kyoku_found_fu/len(gt_kyoku_set):>3.0f}%) | {kyoku_found_either:>4}/{len(gt_kyoku_set)} ({100*kyoku_found_either/len(gt_kyoku_set):>3.0f}%)
Ka        | {len(gt_ka_set):>10} | {ka_found_shi:>4}/{len(gt_ka_set)} ({100*ka_found_shi/len(gt_ka_set):>3.0f}%) | {ka_found_fu:>4}/{len(gt_ka_set)} ({100*ka_found_fu/len(gt_ka_set):>3.0f}%) | {ka_found_either:>4}/{len(gt_ka_set)} ({100*ka_found_either/len(gt_ka_set):>3.0f}%)
Kakari    | {len(gt_kakari_set):>10} | {kakari_found_shi:>4}/{len(gt_kakari_set)} ({100*kakari_found_shi/len(gt_kakari_set):>3.0f}%) | {kakari_found_fu:>4}/{len(gt_kakari_set)} ({100*kakari_found_fu/len(gt_kakari_set):>3.0f}%) | {kakari_found_either:>4}/{len(gt_kakari_set)} ({100*kakari_found_either/len(gt_kakari_set):>3.0f}%)
""")

print("\nNOTES:")
print("- Pre-1944 matching uses MASTER DATA kakari assignments, which are sparse")
print("  (Tokyo-Shi 1943 has very few kakari-level entries in the master data)")
print("- Tokyo-Fu 1941 has slightly more kakari data but still limited")
print("- The low kakari match rate is likely due to sparse digitization of kakari")
print("  in the pre-1944 data, NOT because the kakari didn't exist")
print("- Kyoku/Ka matching is by NAME only (ignoring organizational reassignment)")
print("- 防衛局 is new in 1943 (wartime defense bureau) - no pre-1943 equivalent")
print("- 長官官房 replaced Tokyo-Fu and Tokyo-Shi administrative offices")
