"""
Ka-level crosswalk verification:
1944 Tokyo-To ka ← Pre-1944 Tokyo-Shi / Tokyo-Fu ka
"""
import sys
sys.stdout.reconfigure(encoding='utf-8')

crosswalk = [
    # (1944_kyoku, 1944_ka, shi_kyoku, shi_ka, fu_dept, fu_ka, notes)

    # === 長官官房 (7 ka) ===
    ('長官官房', '人事課',   '総務局', '人事課',   '総務部', '人事課', 'Direct match both'),
    ('長官官房', '文書課',   '総務局', '文書課',   None, None, 'Shi only'),
    ('長官官房', '財務課',   '総務局', '議案課',   None, None, 'Shi 議案課 merged w/ 経理局/主計課 budget fn'),
    ('長官官房', '会計課',   '経理局', '会計課',   '総務部', '会計課', 'Shi 経理局 → 長官官房'),
    ('長官官房', '用度課',   '経理局', '用品課+購買課', None, None, 'Shi 経理局 purchasing merged'),
    ('長官官房', '考査課',   None, None,         None, None, 'NEW in 1942'),
    ('長官官房', '参事官室', None, None,         None, None, 'NEW in 1944'),

    # === 民生局 (8 ka) ===
    ('民生局', '監理課',   '厚生局', '監理課',   None, None, 'Shi 厚生局/清掃部'),
    ('民生局', '振興課',   '市民局', '町会課',   None, None, 'Civic mobilization reorganized'),
    ('民生局', '貯蓄課',   '戦時生活局', '貯蓄奨励課', None, None, 'Wartime savings; from 1942 戦時生活局'),
    ('民生局', '厚生課',   '厚生局', '保護課+福利課', '社会部', '社会課', 'Welfare merged'),
    ('民生局', '健民課',   '市民局', '体力課',   '社会部', '体育課', 'Physical fitness'),
    ('民生局', '衛生課',   '厚生局', '衛生課',   '衛生部', '衛生課', 'Direct match both'),
    ('民生局', '防疫課',   '厚生局', '防疫課',   None, None, 'Shi direct'),
    ('民生局', '清掃課',   '厚生局', '作業課+計画課', None, None, 'Shi 厚生局/清掃部 operations'),

    # === 教育局 (4 ka) ===
    ('教育局', '総務課',   '教育局', '庶務課',   None, None, 'Renamed 庶務課 → 総務課'),
    ('教育局', '国民教育課', '教育局', '学務課+視学課', '学務部', '学務課+視学課', 'Primary ed merged'),
    ('教育局', '青年教育課', '教育局', '青年教育課', '社会部', '社会教育課', 'Direct match Shi'),
    ('教育局', '兵事体錬課', '教育局', '学校体育課', '社会部', '社寺兵事課+体育課', 'Military PE merged'),

    # === 経済局 (6 ka) ===
    ('経済局', '総務課',   '経済局', '庶務課',   None, None, 'Renamed 庶務課 → 総務課'),
    ('経済局', '商工課',   '経済局', '商工課',   '経済部', '商務課+工務課', 'Shi direct; Fu merged'),
    ('経済局', '農務課',   '経済局', '農漁課',   '経済部', '農林課', 'Renamed'),
    ('経済局', '林務課',   None, None,         '経済部', '農林課(林業)', 'Split from Fu forestry'),
    ('経済局', '食料課',   '経済局', '配給課',   None, None, 'Shi rationing → food supply'),
    ('経済局', '資材課',   '戦時生活局', '物價課+配給課', '経済部', '資材統制課', 'Materials control'),

    # === 計画局 (6 ka) ===
    ('計画局', '庶務課',   '土木局', '庶務課',   '土木部', '経理課', 'Shi civil eng admin'),
    ('計画局', '都市計画課', '総務局', '都市計画課', None, None, 'Shi 総務局 → 計画局'),
    ('計画局', '公園緑地課', '市民局', '公園課',   None, None, 'Shi 市民局/公園課 expanded'),
    ('計画局', '道路課',   '土木局', '道路管理課+道路建設課', '土木部', '道路課', 'Merged two road ka'),
    ('計画局', '河川課',   '土木局', '河川課',   '土木部', '河港課', 'Direct; Fu 河港課'),
    ('計画局', '防衛工事課', '土木局', '治水工事課', None, None, 'Repurposed for wartime'),

    # === 防衛局 (7 ka) ===
    ('防衛局', '企画課',   '防衛局', '計画課',   None, None, 'Shi 防衛局/計画課 renamed'),
    ('防衛局', '人員疎開課', None, None,         None, None, 'NEW wartime evacuation'),
    ('防衛局', '物資疎開課', None, None,         None, None, 'NEW wartime evacuation'),
    ('防衛局', '建物疎開課', None, None,         None, None, 'NEW wartime demolition'),
    ('防衛局', '建築課',   None, None,         None, None, 'NEW (or from 経理局 building fn)'),
    ('防衛局', '防火改修課', '防衛局', '防火改修課', None, None, 'Direct match'),
    ('防衛局', '営繕課',   '経理局', '営繕課',   '総務部', '営繕課', 'Moved from 経理局'),

    # === 交通局 (6 ka) ===
    ('交通局', '総務課',   '電気局', '庶務課',   None, None, 'Renamed 庶務課 → 総務課'),
    ('交通局', '勤労課',   '電気局', '労務課',   None, None, 'Renamed 労務課 → 勤労課'),
    ('交通局', '経理課',   '電気局', '経理課',   None, None, 'Direct match'),
    ('交通局', '運輸課',   '電気局', '運転課+乗客課', None, None, 'Transit ops merged'),
    ('交通局', '保線課',   '電気局', '保線課',   None, None, 'Direct match'),
    ('交通局', '電気課',   '電気局', '配線課+電力課', None, None, 'Electric ops merged'),

    # === 水道局 (5 ka) ===
    ('水道局', '庶務課',   '水道局', '庶務課',   None, None, 'Continued (rare in data)'),
    ('水道局', '業務課',   '水道局', '営業課',   None, None, 'Renamed 営業課 → 業務課'),
    ('水道局', '給水課',   '水道局', '給水課',   None, None, 'Direct match'),
    ('水道局', '工事課',   '水道局', '拡張課+計画課', None, None, 'Construction merged'),
    ('水道局', '下水課',   '水道局', '下水課',   None, None, 'Direct (水課 in data)'),

    # === 港湾局 (3 ka) ===
    ('港湾局', '総務課',   '港湾局', '庶務課',   None, None, 'Renamed 庶務課 → 総務課'),
    ('港湾局', '経理課',   '港湾局', '経理課',   None, None, 'Direct match'),
    ('港湾局', '工事課',   '港湾局', '工事課+築港課', None, None, 'Merged construction'),
]

print('=' * 100)
print('COMPLETE KA-LEVEL CROSSWALK: 1944 Tokyo-To <- Pre-1944 Shi/Fu')
print('=' * 100)

matched_shi = 0
matched_fu = 0
matched_either = 0
total = len(crosswalk)

current_kyoku = None
for row in crosswalk:
    kyoku44, ka44, shi_ky, shi_ka, fu_dept, fu_ka, notes = row
    if kyoku44 != current_kyoku:
        current_kyoku = kyoku44
        print(f'\n  [{kyoku44}]')

    has_shi = shi_ka is not None
    has_fu = fu_ka is not None
    if has_shi: matched_shi += 1
    if has_fu: matched_fu += 1
    if has_shi or has_fu: matched_either += 1

    shi_str = f'{shi_ky}/{shi_ka}' if has_shi else '---'
    fu_str = f'{fu_dept}/{fu_ka}' if has_fu else '---'
    status = 'OK' if (has_shi or has_fu) else 'NEW'

    print(f'  {status:3s} {ka44:12s} <- Shi: {shi_str:45s} Fu: {fu_str:30s} | {notes}')

print(f'\n{"=" * 100}')
print(f'SUMMARY: {total} ka total')
print(f'  Matched to Shi: {matched_shi}/{total} ({100*matched_shi/total:.0f}%)')
print(f'  Matched to Fu:  {matched_fu}/{total} ({100*matched_fu/total:.0f}%)')
print(f'  Matched either: {matched_either}/{total} ({100*matched_either/total:.0f}%)')
print(f'  NEW (no predecessor): {total - matched_either}/{total} ({100*(total-matched_either)/total:.0f}%)')

print(f'\nBy kyoku:')
kyoku_counts = {}
for row in crosswalk:
    ky = row[0]
    has = row[2] is not None or row[4] is not None
    if ky not in kyoku_counts:
        kyoku_counts[ky] = [0, 0]
    kyoku_counts[ky][0] += 1
    if has: kyoku_counts[ky][1] += 1
for ky, (tot, m) in kyoku_counts.items():
    unmatched = tot - m
    print(f'  {ky:12s}: {m}/{tot} matched' + (f' ({unmatched} NEW)' if unmatched else ''))
