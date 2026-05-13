"""
Build ka_group from worker flows across years.
Rule: if >=40% of a (kyoku, ka) cell's workers in year t appear in the same
      (kyoku, ka) cell in year t+1, link them as the same ka_group.
Uses union-find for transitive closure.
"""
import csv, sys
from collections import defaultdict, Counter
sys.stdout.reconfigure(encoding='utf-8')

THRESHOLD = 0.40
MIN_WORKERS = 5  # minimum workers to consider a flow meaningful

DATA_PATH = (
    r'C:\Users\Keitaro Ninomiya\Box\Research Notes (keitaro2@illinois.edu)'
    r'\Tokyo_Gender\Processed_Data\Tokyo_Personnel_Master_All_Years.csv'
)

# ── Load worker-year records ──
print("Loading data...")
worker_years = defaultdict(dict)  # staff_id -> {year: (kyoku, ka)}
with open(DATA_PATH, 'r', encoding='utf-8') as f:
    reader = csv.DictReader(f)
    year_col = '\ufeffyear'
    for row in reader:
        yr = int(row[year_col])
        if yr < 1934 or yr > 1945:
            continue
        if row['is_name'] != 'True':
            continue
        ka = (row.get('ka') or '').strip()
        if not ka:
            continue
        kyoku = (row.get('kyoku') or '').strip()
        worker_years[row['staff_id']][yr] = (kyoku, ka)

print(f"  {len(worker_years)} workers with ka data")

# ── Union-Find ──
parent = {}

def find(x):
    while parent[x] != x:
        parent[x] = parent[parent[x]]
        x = parent[x]
    return x

def union(a, b):
    ra, rb = find(a), find(b)
    if ra != rb:
        parent[rb] = ra

# ── Build flows for each consecutive year pair ──
years = sorted({yr for w in worker_years.values() for yr in w})
print(f"  Years: {years}")

links = []  # (from_node, to_node, n_workers, pct)

for i in range(len(years) - 1):
    yr_from, yr_to = years[i], years[i + 1]

    # Count flows: (from_kyoku, from_ka) -> (to_kyoku, to_ka)
    flows = Counter()
    from_totals = Counter()
    for sid, hist in worker_years.items():
        if yr_from in hist and yr_to in hist:
            src = hist[yr_from]
            dst = hist[yr_to]
            flows[(src, dst)] += 1
            from_totals[src] += 1

    # For each source (kyoku, ka), find dominant destination
    for src, total in from_totals.items():
        if total < MIN_WORKERS:
            continue
        # Get all destinations
        dests = {dst: cnt for (s, dst), cnt in flows.items() if s == src}
        top_dst, top_cnt = max(dests.items(), key=lambda x: x[1])
        pct = top_cnt / total

        src_node = (yr_from, src[0], src[1])
        dst_node = (yr_to, top_dst[0], top_dst[1])

        # Initialize in union-find
        for n in (src_node, dst_node):
            if n not in parent:
                parent[n] = n

        if pct >= THRESHOLD:
            union(src_node, dst_node)
            links.append((src_node, dst_node, top_cnt, total, pct))

print(f"\n  {len(links)} links at >= {THRESHOLD*100:.0f}% threshold")

# Also initialize singleton nodes for any (year, kyoku, ka) not yet linked
for sid, hist in worker_years.items():
    for yr, (kyoku, ka) in hist.items():
        node = (yr, kyoku, ka)
        if node not in parent:
            parent[node] = node

# ── Assign group IDs ──
groups = defaultdict(list)
for node in parent:
    root = find(node)
    groups[root].append(node)

# Sort groups by size (number of distinct year-nodes)
sorted_groups = sorted(groups.items(), key=lambda x: -len(x[1]))

print(f"  {len(sorted_groups)} ka_groups total")
print(f"  {sum(1 for _, members in sorted_groups if len(members) > 1)} multi-year groups")
print(f"  {sum(1 for _, members in sorted_groups if len(members) == 1)} singleton groups")

# ── Report multi-year groups ──
print(f"\n{'='*90}")
print(f"KA GROUPS (multi-year chains, {THRESHOLD*100:.0f}% threshold)")
print(f"{'='*90}")

group_id = 0
ka_group_map = {}  # (year, kyoku, ka) -> group_id

for root, members in sorted_groups:
    members_sorted = sorted(members, key=lambda x: (x[0], x[1], x[2]))
    gid = group_id
    group_id += 1
    for m in members_sorted:
        ka_group_map[m] = gid

    if len(members) <= 1:
        continue

    # Show chain
    year_span = sorted(set(m[0] for m in members_sorted))
    print(f"\n  Group {gid} ({len(members)} nodes, {year_span[0]}-{year_span[-1]}):")
    for yr, kyoku, ka in members_sorted:
        # Count workers in this cell
        n = sum(1 for sid, hist in worker_years.items()
                if yr in hist and hist[yr] == (kyoku, ka))
        print(f"    {yr} [{kyoku or '(blank)':20s}] {ka:30s} ({n} workers)")

# ── Focus on 1943->1944 links ──
print(f"\n{'='*90}")
print(f"1943->1944 LINKS (merger boundary)")
print(f"{'='*90}")

merger_links = [(s, d, n, t, p) for s, d, n, t, p in links
                if s[0] == 1943 and d[0] == 1944]
merger_links.sort(key=lambda x: -x[3])

for src, dst, n, total, pct in merger_links:
    same_group = find(src) == find(dst)
    print(f"  {src[1] or '(blank)':15s}/{src[2]:25s} -> "
          f"{dst[1] or '(blank)':15s}/{dst[2]:25s}  "
          f"{n:>3d}/{total:>3d} ({pct*100:>5.1f}%)  grp={ka_group_map.get(src, '?')}")

# ── Export ka_group_map as CSV ──
import os
out_path = os.path.join(
    os.environ.get('USERPROFILE', ''),
    r'Documents\GitHub\Tokyo_Project_Figures\Regressions\ka_group_map.csv'
)
with open(out_path, 'w', encoding='utf-8', newline='') as f:
    f.write('year,kyoku,ka,ka_group\n')
    for (yr, kyoku, ka), gid in sorted(ka_group_map.items()):
        f.write(f'{yr},{kyoku},{ka},{gid}\n')
print(f"\nExported {len(ka_group_map)} rows to ka_group_map.csv")

# ── Summary of 1944 ka coverage ──
print(f"\n{'='*90}")
print("1944 ka_group coverage")
print(f"{'='*90}")

nodes_1944 = [(yr, ky, ka) for (yr, ky, ka) in ka_group_map if yr == 1944]
n_with_predecessor = 0
for node in sorted(nodes_1944, key=lambda x: (x[1], x[2])):
    gid = ka_group_map[node]
    members = groups[find(node)]
    pre44 = [m for m in members if m[0] < 1944]
    has_pred = len(pre44) > 0
    if has_pred:
        n_with_predecessor += 1
    pred_str = ', '.join(f"{m[0]} {m[1] or '(blank)'}/{m[2]}" for m in sorted(pre44)[-2:])
    if not pred_str:
        pred_str = '--- NEW ---'
    n = sum(1 for sid, hist in worker_years.items()
            if 1944 in hist and hist[1944] == (node[1], node[2]))
    print(f"  [{node[1] or '(blank)':15s}] {node[2]:30s} ({n:>4d}) grp={gid:>3d}  <- {pred_str}")

print(f"\n  Total 1944 ka: {len(nodes_1944)}")
print(f"  With pre-1944 predecessor: {n_with_predecessor}/{len(nodes_1944)}")
print(f"  New (no predecessor): {len(nodes_1944) - n_with_predecessor}/{len(nodes_1944)}")
