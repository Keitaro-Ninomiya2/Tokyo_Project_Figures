================================================================================
CROSSWALK REPORT: 1944 Tokyo-To ↔ Pre-1944 Tokyo-Shi / Tokyo-Fu
================================================================================


================================================================================
PART 1: Corrected 1944 Hierarchy (Ground Truth)
  Source: soumu.metro.tokyo.lg.jp (昭和19年8月1日現在)
================================================================================

【長官官房】 (7 ka, 21 kakari)
  人事課: 秘書係, 人事第一係, 人事第二係, 福利係
  文書課: 庶務係, 文書係, 報道係, 統計係
  財務課: 議案係, 予算第一係, 予算第二係, 税務係, 収納係
  会計課: 管理係, 会計第一係, 会計第二係, 防護係
  用度課: 購買係, 用品係
  考査課: 考査第一係, 考査第二係
  参事官室

【民生局】 (8 ka, 23 kakari)
  監理課: 庶務係, 行政係
  振興課: 動員係, 町会係, 資源回収係
  貯蓄課: 貯蓄第一係, 貯蓄第二係
  厚生課: 厚生係, 住宅係, 保護係, 軍事援護係
  健民課: 体力係, 修錬係, 母子係
  衛生課: 衛生係, 指導係, 医薬係
  防疫課: 防疫係, 監察係
  清掃課: 庶務係, 調度係, 作業係, 施設係

【教育局】 (4 ka, 14 kakari)
  総務課: 社寺係, 学事係, 視学係, 私立学校係, 勤労動員係
  国民教育課: 学事係, 視学係, 職員係
  青年教育課: 青年学校係, 青少年団係, 教化係
  兵事体錬課: 兵事係, 学校体練係, 学校衛生係

【経済局】 (6 ka, 22 kakari)
  総務課: 調査室, 総務係, 生産増強係, 物価係
  商工課: 商務係, 工務係, 企業係, 権度係
  農務課: 農務係, 水産係, 畜産係, 整地係
  林務課: 木材係, 山林係, 薪炭係
  食料課: 米穀係, 生鮮食品係, 食品係
  資材課: 金属係, 繊維係, 化学燃料係, 雑品係

【計画局】 (6 ka, 13 kakari)
  庶務課: 経理係, 用地係
  都市計画課: 計画係, 事業係
  公園緑地課: 管理係, 工事係, 霊園係
  道路課: 管理係, 工事係
  河川課: 管理係, 工事係
  防衛工事課: 用材係, 工事係

【防衛局】 (7 ka, 22 kakari)
  企画課: 庶務係, 企画係, 資材係
  人員疎開課: 統制係, 指導係
  物資疎開課: 受託係, 管理係
  建物疎開課: 監理係, 計画係, 補償係, 工事係, 疎開小空地係
  建築課: 監査係, 利用統制係, 技術係
  防火改修課: 事務係, 技術係
  営繕課: 庶務係, 営繕第一係, 営繕第二係, 防衛施設係, 装置係

【交通局】 (6 ka, 16 kakari)
  総務課: 総務係, 文書係, 主計係
  勤労課: 勤労係, 厚生係, 指導係
  経理課: 会計係, 資材係
  運輸課: 業務係, 電車係, 自動車係, 車庫係
  保線課: 管理係, 工事係
  電気課: 電路係, 配電係

【水道局】 (5 ka, 12 kakari)
  庶務課: 庶務係, 用度係, 会計係
  業務課: 業務係, 給水工事係
  給水課: 管理係, 配水係
  工事課: 計画係, 工事係
  下水課: 庶務係, 管理係, 工事係

【港湾局】 (3 ka, 6 kakari)
  総務課: 計画係, 港務係
  経理課: 会計係, 管理係
  工事課: 設計係, 工事係

Totals: 9 kyoku, 52 ka, 149 kakari


================================================================================
PART 2: Master Data Quality Assessment (1944)
================================================================================

Master data: 8 kyoku, 287 ka, 122 kakari

Kyoku in master data: ['', '交通局', '学校体育課経済局', '教育局', '水道局', '港湾局', '経済局', '計晝局']

--- Known Issues ---
  [Missing kyoku] 長官官房 and 民生局 entries have blank kyoku ('')
  [Missing kyoku] 防衛局 is entirely missing from kyoku assignments
  [OCR error] 計晝局 → should be 計画局
  [Boundary artifact] 学校体育課経済局 is a page-boundary OCR error
  [OCR errors in ka] 翳生課 → 衛生課, 清日課 → unclear, 西部田田課 → unclear
  [bu field] Contains person names (部長 names), not organizational 部 units
  [Missing kakari] Many kakari from ground truth are not in master data

--- Kyoku Correction Map ---
  '' → 長官官房 + 民生局 (ambiguous)
  '計晝局' → 計画局
  '学校体育課経済局' → 経済局 (boundary artifact)


================================================================================
PART 3: Pre-1944 Office Structures (from Master Data)
================================================================================

--- Tokyo-Shi 1943 ---
Kyoku: 20, Ka: 163, Kakari: 0
NOTE: Kakari count is 0 — kakari was NOT digitized for Tokyo-Shi 1943

  【(blank)】 (13 ka)
    C番議室
    人事課
    区政課
    報道課
    文書課
    東京市役所
    検査課
    秘書課
    統計課
    考査課
    議案課
    豫算課
    都市計晝課

  【中央卸売市場】 (10 ka)
    初等教育課
    労務課
    各区役所
    庶務課
    庶務部長室
    新聞記者室
    第一課
    議事課
    長長掛長掛長掛庫場
    電話事務所

  【健民局】 (16 ka)
    上中屋署
    中所
    体力課
    千部出張所
    厚生課
    古石場
    庶務課
    所
    技術課
    機械工訓育所長兼機械工訓育所
    母子課
    管理課
    補導課
    軍事援護課
    防疫課
    馬場

  【土木局】 (5 ka)
    庶務課
    江戸川区出張所
    治水課
    道路建設課
    道路管理課

  【城東病院院】 (8 ka)
    会計課
    健民局母子課
    医務課
    庶務課
    松戸療養所
    監護課
    管理課
    衛生試験所

  【市会事務局】 (6 ka)
    大和三八〇富署
    庶務課
    橋場
    福室
    茅場
    議事課

  【後醍院】 (5 ka)
    中野区役所
    大森区役所
    淀橋区役所
    砧派出所
    蒲田区役所

  【戰時生活局】 (10 ka)
    動員課
    商工課
    庶務課
    物價課
    町会課
    管理課
    貯薄奬勵課
    農漁課
    配給第一課
    配給第二課

  【教育局】 (10 ka)
    中等教育課
    保線課
    印刷所
    学校体育課
    学校施設課
    庶務課
    教化課
    教育研究所
    施設課
    詞練課

  【民局】 (1 ka)
    古石場

  【水道局】 (13 ka)
    下谷区役所
    収納課
    可籍兵事課・財務課
    審議室
    工務課
    幸場
    擴張課
    本室
    王子区役所
    給水課
    豫算課
    購買課
    長書仕役書役書室

  【水違局】 (6 ka)
    小智海合宿所
    小習酒合病所
    庶務課
    所
    技術課
    水源林事務所

  【港湾局】 (6 ka)
    会計課
    営業課
    小室
    庶務課
    港務所
    築港課

  【港準局】 (6 ka)
    下水課
    営業課
    庶務課
    第三課
    詰所
    長室部掛掛室室室室室

  【経〓局】 (2 ka)
    茅場
    軍事援護課

  【経理局】 (13 ka)
    収納課
    向島区役所
    営繕課
    地理課
    学校警繕課
    書理課
    書計課
    杉並区役所
    用品課
    荒川区役所
    葛飾区役所
    購買課
    足立区役所

  【電気局】 (33 ka)
    下谷区役所
    主計課
    京橋区役所
    会計課
    作業課
    保線課
    労務課
    品川区役所
    大場
    小石川区役所
    日本橋区役所
    本所区役所
    本郷区役所
    深川区役所
    牛込区役所
    監督課
    目黒区役所
    研究課
    神田区役所
    経理課
    芝区役所
    荏原区役所
    親切課
    計画課
    試験課
    調査課
    赤坂区役所
    車輛課
    運転課
    電力課
    電気研究所
    麹町区役所
    麻布区役所


--- Tokyo-Fu 1941 ---
Kyoku: 1, Ka: 184, Kakari: 18

  【(blank)】 (1 ka)
    庶務課

  【一府会事務局】 (183 ka)
    の部資材統制課
    ラバヤ出張所
    一分室
    三ツ木診療所
    三宅島出張所
    三河島共同住宅管理事務所
    三河島診療所
    上野出張所
    上野税務出張所
    世田谷出張所
    事務所
    事務所荒木田出張所
    二四三第二会議室
    二西巣鴨診療所
    五反田出張所
    京済運河建設事務所
    人事課
    代用品相談所
    会計課
    佐室
    体育課社寺兵事課
    傷痍軍人職業補導所
    八王子土木出張所
    八王子市役所
    八王子支所
    八王子職業紹介所
    六郷出張所
    出張所
    北多府中番場
    北多摩方面事務所
    北多摩税務出張所
    北室
    千住出張所
    南多摩方面事務所
    南多摩税務出張所
    南葛飾税務出張所
    南足立税務出張所
    吉野養魚場
    向島診療所
    吾婿分所
    品川診療所
    商務課: 女子商業講習所係, 工業組合監査指導係
    商江会議所
    営繕課
    国民精神文化講習所
    地方課
    城所
    場
    多磨砂利採取工場
    大井分所
    大井診療所
    大島分場
    大島分所
    大島診療所
    大崎診療所
    大緑地建設事務所
    奥多摩支場
    官幣大社明治神宮外苑管理署
    家具王養成所
    小室
    小松川方面雲務所
    小笠原試育所
    島嶼物産販売斡旋所
    工務課: 工業組合監査指導係
    幹部機械工養成所
    府中土木出張所
    府立八王子性病豫防所
    府立北多摩保健所
    府立協同組合講習所
    府立府中性病豫防所
    府立恩方診療所
    府立板橋健康相談所
    府立板橋性病豫防所
    府立染織試験場
    府立機械工養成所
    府立淀橋健康相談所
    府立淀橋性病豫防所
    府立立川健康相談所
    府立立川性病豫防所
    府立西多摩保健所
    府立農事試験場
    府立農会技術員養成所
    府立青年学校教員養成所
    府立青梅性病豫防所
    庶務課
    所
    拓務訓練所
    拓務訓練所大緑地建設事務所
    教員養成所
    整地課: 多摩川沿岸水利改良係, 検定係
    木場
    杉並分所
    杉並診療所
    東京労働紹介所
    東京市役所
    東京職業紹介所
    板橋分所
    染織試験場
    業取締所繭検定所
    業試験場
    橋梁課
    水産試験場
    河港課: 庶務係, 技術係
    淀橋出張所
    淀橋診療所
    深川出張所
    済生会三河島診療所
    済部資材統制課
    済部農林課
    瀝青乳劑製造工場
    熊川砂利採取工場
    物價統制課: 物價統制係
    玉川診療所
    王子出張所
    王子診療所
    産業組合課
    田場
    目黒臨時診療所
    目黒診療所
    砂町診療所
    社会教育課
    社会課
    社寺兵事課: 児童係
    神津島診療所
    種畜場
    立川職業紹介所
    第一河川出張所
    第一道路出張所
    第三河川出張所
    第三道路出張所
    第二河川出張所
    第二道路出張所
    第四道路出張所
    管理署
    経済〓物價統湖課
    経済部工務課
    経済部産業組合課
    経済部資材統制課
    経済部農林課
    経理課: 契約係, 庶務係, 用地係, 経理係, 負擔金係
    繭検定所
    職業補導所
    職業課
    芝浦出張所
    茅場
    荏原税務出張所
    荏原診療所
    蒲田出張所
    虎ノ門税務出張所
    蠶業取締所
    蠶業取締所福生支所
    蠶業取締所立川支所
    蠶業試験場
    西多摩方面事務所
    西多摩税務出張所
    西巣鴨出張所
    西巣鴨診療所
    視学課: 検定係
    診療所
    調布飛行場建設事務所
    調査課: 検定係
    資材統制課
    軍事援護課
    農事講習所
    農林産業検査所大島出張所
    農林産物検査所
    農林産物検査所府中出張所
    農林産物検査所板橋出張所
    農林産物検査所深川出張所
    農林産物検査所青梅出張所
    農林課
    込王子職業紹介所
    道路課: 工事係
    還事講習所
    部農林課
    郵澤部産業組合課
    金町分所
    青梅土木出張所
    青梅職業紹介所
    頒泣北多摩保健所
    願館出張所
    飯田橋税務出張所
    龜戸出張所


================================================================================
PART 4: Kyoku-Level Crosswalk
  The 1943 merger of Tokyo-Shi + Tokyo-Fu → Tokyo-To caused major reorg
================================================================================

  長官官房 — Governor's Secretariat (new with Tokyo-To)
    ← Shi: (blank/top-level offices: 秘書課, 人事課, 文書課, 区政課, 報道課, 統計課, 議案課, 都市計晝課, 豫算課, 検査課, 考査課)
    ← Fu:  (blank/top-level: 庶務課, 人事課, 会計課)
    Note: Absorbs admin functions from both Shi and Fu top-level offices

  民生局 — Civil Affairs Bureau (new name, merges multiple functions)
    ← Shi: 戰時生活局, 健民局, 城東病院院(part)
    ← Fu:  (social welfare: 社会課, 軍事援護課, 職業課)
    Note: Combines wartime life admin, health/welfare, cleanup from Shi + Fu welfare offices

  教育局 — Education Bureau (continued from Shi)
    ← Shi: 教育局
    ← Fu:  (education functions: 視学課, 社会教育課, 体育課)
    Note: Largely carries over from Tokyo-Shi教育局, absorbs Fu education offices

  経済局 — Economic Bureau (new, merges economic functions)
    ← Shi: 戰時生活局(economic parts: 商工課, 配給課, 物價課, 農漁課)
    ← Fu:  (economic functions: 経済部 including 工務課, 農林課, 商務課, 物價統制課, 資材統制課, 産業組合課)
    Note: Major merger of economic/trade/agricultural functions from both entities

  計画局 — Planning Bureau (merges infrastructure planning)
    ← Shi: 土木局
    ← Fu:  (infrastructure: 道路課, 橋梁課, 河港課, 整地課, 営繕課)
    Note: Combines Shi civil engineering + Fu infrastructure + new urban planning

  防衛局 — Defense Bureau (new wartime bureau, est. 1943)
    ← Shi: (no direct predecessor)
    ← Fu:  (no direct predecessor)
    Note: Entirely new — created for wartime building evacuation/defense

  交通局 — Transportation Bureau (from Shi 電気局)
    ← Shi: 電気局
    ← Fu:  (no equivalent — Fu had no transit system)
    Note: Direct successor to Tokyo-Shi 電気局 (electric/tram operations)

  水道局 — Water Bureau (continued from Shi)
    ← Shi: 水道局
    ← Fu:  (no equivalent — Shi managed water)
    Note: Largely unchanged from Tokyo-Shi水道局

  港湾局 — Port Bureau (continued from Shi)
    ← Shi: 港湾局
    ← Fu:  (no equivalent)
    Note: Largely unchanged from Tokyo-Shi港湾局


Kyoku match summary:
  Total 1944 kyoku: 9
  Exact name match in Shi: 3/9
  Exact name match in Fu:  0/9
  With known reorganization mapping: 8/9 (all except 防衛局)


================================================================================
PART 5: Ka-Level Crosswalk
================================================================================

Total 1944 ka: 52
  Found in Tokyo-Shi 1943: 16/52 (31%)
  Found in Tokyo-Fu 1941:  8/52 (15%)
  Found in either:         17/52 (33%)

Detailed ka-level matches:

  【長官官房】
    人事課 → Shi(人事課) + Fu(人事課)
    文書課 → Shi(文書課)
    財務課 → NOT FOUND
    会計課 → Shi(会計課) + Fu(会計課)
    用度課 → NOT FOUND
    考査課 → Shi(考査課)
    参事官室 → NOT FOUND

  【民生局】
    監理課 → NOT FOUND
    振興課 → NOT FOUND
    貯蓄課 → NOT FOUND
    厚生課 → Shi(厚生課)
    健民課 → NOT FOUND
    衛生課 → NOT FOUND
    防疫課 → Shi(防疫課)
    清掃課 → NOT FOUND

  【教育局】
    総務課 → NOT FOUND
    国民教育課 → NOT FOUND
    青年教育課 → NOT FOUND
    兵事体錬課 → NOT FOUND

  【経済局】
    総務課 → NOT FOUND
    商工課 → Shi(商工課)
    農務課 → NOT FOUND
    林務課 → NOT FOUND
    食料課 → NOT FOUND
    資材課 → NOT FOUND

  【計画局】
    庶務課 → Shi(庶務課) + Fu(庶務課)
    都市計画課 → Shi(都市計晝課)
    公園緑地課 → NOT FOUND
    道路課 → Fu(道路課)
    河川課 → NOT FOUND
    防衛工事課 → NOT FOUND

  【防衛局】
    企画課 → NOT FOUND
    人員疎開課 → NOT FOUND
    物資疎開課 → NOT FOUND
    建物疎開課 → NOT FOUND
    建築課 → NOT FOUND
    防火改修課 → NOT FOUND
    営繕課 → Shi(営繕課) + Fu(営繕課)

  【交通局】
    総務課 → NOT FOUND
    勤労課 → NOT FOUND
    経理課 → Shi(経理課) + Fu(経理課)
    運輸課 → NOT FOUND
    保線課 → Shi(保線課)
    電気課 → NOT FOUND

  【水道局】
    庶務課 → Shi(庶務課) + Fu(庶務課)
    業務課 → NOT FOUND
    給水課 → Shi(給水課)
    工事課 → NOT FOUND
    下水課 → Shi(下水課)

  【港湾局】
    総務課 → NOT FOUND
    経理課 → Shi(経理課) + Fu(経理課)
    工事課 → NOT FOUND

  Ka NOT found in either (35):
    長官官房 > 財務課
    長官官房 > 用度課
    長官官房 > 参事官室
    民生局 > 監理課
    民生局 > 振興課
    民生局 > 貯蓄課
    民生局 > 健民課
    民生局 > 衛生課
    民生局 > 清掃課
    教育局 > 総務課
    教育局 > 国民教育課
    教育局 > 青年教育課
    教育局 > 兵事体錬課
    経済局 > 総務課
    経済局 > 農務課
    経済局 > 林務課
    経済局 > 食料課
    経済局 > 資材課
    計画局 > 公園緑地課
    計画局 > 河川課
    計画局 > 防衛工事課
    防衛局 > 企画課
    防衛局 > 人員疎開課
    防衛局 > 物資疎開課
    防衛局 > 建物疎開課
    防衛局 > 建築課
    防衛局 > 防火改修課
    交通局 > 総務課
    交通局 > 勤労課
    交通局 > 運輸課
    交通局 > 電気課
    水道局 > 業務課
    水道局 > 工事課
    港湾局 > 総務課
    港湾局 > 工事課


================================================================================
PART 6: Kakari-Level Crosswalk
  NOTE: Tokyo-Shi 1943 has 0 kakari in master data (not digitized)
  Only Tokyo-Fu 1941 provides kakari-level comparison
================================================================================

Tokyo-Fu 1941 kakari names (13): ['児童係', '多摩川沿岸水利改良係', '契約係', '女子商業講習所係', '工事係', '工業組合監査指導係', '庶務係', '技術係', '検定係', '物價統制係', '用地係', '経理係', '負擔金係']

Total 1944 kakari: 149
  Found in Tokyo-Shi 1943: 0/149 (0%)
  Found in Tokyo-Fu 1941:  20/149 (13%)
  Found in either:         20/149 (13%)

Detailed kakari-level matches:

  【長官官房】
    人事課:
      秘書係 → —
      人事第一係 → —
      人事第二係 → —
      福利係 → —
    文書課:
      庶務係 → Fu(庶務係)
      文書係 → —
      報道係 → —
      統計係 → —
    財務課:
      議案係 → —
      予算第一係 → —
      予算第二係 → —
      税務係 → —
      収納係 → —
    会計課:
      管理係 → —
      会計第一係 → —
      会計第二係 → —
      防護係 → —
    用度課:
      購買係 → —
      用品係 → —
    考査課:
      考査第一係 → —
      考査第二係 → —

  【民生局】
    監理課:
      庶務係 → Fu(庶務係)
      行政係 → —
    振興課:
      動員係 → —
      町会係 → —
      資源回収係 → —
    貯蓄課:
      貯蓄第一係 → —
      貯蓄第二係 → —
    厚生課:
      厚生係 → —
      住宅係 → —
      保護係 → —
      軍事援護係 → —
    健民課:
      体力係 → —
      修錬係 → —
      母子係 → —
    衛生課:
      衛生係 → —
      指導係 → —
      医薬係 → —
    防疫課:
      防疫係 → —
      監察係 → —
    清掃課:
      庶務係 → Fu(庶務係)
      調度係 → —
      作業係 → —
      施設係 → —

  【教育局】
    総務課:
      社寺係 → —
      学事係 → —
      視学係 → —
      私立学校係 → —
      勤労動員係 → —
    国民教育課:
      学事係 → —
      視学係 → —
      職員係 → —
    青年教育課:
      青年学校係 → —
      青少年団係 → —
      教化係 → —
    兵事体錬課:
      兵事係 → —
      学校体練係 → —
      学校衛生係 → —

  【経済局】
    総務課:
      調査室 → —
      総務係 → —
      生産増強係 → —
      物価係 → —
    商工課:
      商務係 → —
      工務係 → —
      企業係 → —
      権度係 → —
    農務課:
      農務係 → —
      水産係 → —
      畜産係 → —
      整地係 → —
    林務課:
      木材係 → —
      山林係 → —
      薪炭係 → —
    食料課:
      米穀係 → —
      生鮮食品係 → —
      食品係 → —
    資材課:
      金属係 → —
      繊維係 → —
      化学燃料係 → —
      雑品係 → —

  【計画局】
    庶務課:
      経理係 → Fu(経理係)
      用地係 → Fu(用地係)
    都市計画課:
      計画係 → —
      事業係 → —
    公園緑地課:
      管理係 → —
      工事係 → Fu(工事係)
      霊園係 → —
    道路課:
      管理係 → —
      工事係 → Fu(工事係)
    河川課:
      管理係 → —
      工事係 → Fu(工事係)
    防衛工事課:
      用材係 → —
      工事係 → Fu(工事係)

  【防衛局】
    企画課:
      庶務係 → Fu(庶務係)
      企画係 → —
      資材係 → —
    人員疎開課:
      統制係 → —
      指導係 → —
    物資疎開課:
      受託係 → —
      管理係 → —
    建物疎開課:
      監理係 → —
      計画係 → —
      補償係 → —
      工事係 → Fu(工事係)
      疎開小空地係 → —
    建築課:
      監査係 → —
      利用統制係 → —
      技術係 → Fu(技術係)
    防火改修課:
      事務係 → —
      技術係 → Fu(技術係)
    営繕課:
      庶務係 → Fu(庶務係)
      営繕第一係 → —
      営繕第二係 → —
      防衛施設係 → —
      装置係 → —

  【交通局】
    総務課:
      総務係 → —
      文書係 → —
      主計係 → —
    勤労課:
      勤労係 → —
      厚生係 → —
      指導係 → —
    経理課:
      会計係 → —
      資材係 → —
    運輸課:
      業務係 → —
      電車係 → —
      自動車係 → —
      車庫係 → —
    保線課:
      管理係 → —
      工事係 → Fu(工事係)
    電気課:
      電路係 → —
      配電係 → —

  【水道局】
    庶務課:
      庶務係 → Fu(庶務係)
      用度係 → —
      会計係 → —
    業務課:
      業務係 → —
      給水工事係 → —
    給水課:
      管理係 → —
      配水係 → —
    工事課:
      計画係 → —
      工事係 → Fu(工事係)
    下水課:
      庶務係 → Fu(庶務係)
      管理係 → —
      工事係 → Fu(工事係)

  【港湾局】
    総務課:
      計画係 → —
      港務係 → —
    経理課:
      会計係 → —
      管理係 → —
    工事課:
      設計係 → —
      工事係 → Fu(工事係)


================================================================================
PART 7: Summary
================================================================================

Match rates for 1944 Tokyo-To hierarchy found in pre-1944 directories:

Level   | 1944 Count | In Shi 1943      | In Fu 1941       | In Either
--------|------------|------------------|------------------|------------------
Kyoku   |          9 |    3/ 9 ( 33.3%) |    0/ 9 (  0.0%) |    3/ 9 ( 33.3%)
Ka      |         52 |   16/52 ( 30.8%) |    8/52 ( 15.4%) |   17/52 ( 32.7%)
Kakari  |        149 |    0/149 (  0.0%) |   20/149 ( 13.4%) |   20/149 ( 13.4%)

NOTE: Kyoku matching with historical reorganization knowledge: 8/9 have identifiable predecessors.

Key findings:
  1. KYOKU: Only 3/9 exact name matches (教育局, 水道局, 港湾局).
     With known reorganization mapping, 8/9 have predecessors (防衛局 is new).
  2. KA: ~30% exact name match. Many ka were renamed in the merger:
     - 総務課 (new in many bureaus, replacing various admin offices)
     - Wartime-specific ka like 貯蓄課, 健民課 had no pre-1943 equivalents
     - 防衛局 ka are entirely new (疎開=evacuation was a 1944 concept)
  3. KAKARI: Very low match rates due to:
     - Tokyo-Shi 1943 master data has NO kakari entries (0 digitized)
     - Tokyo-Fu 1941 has only 18 kakari names
     - This is a DATA COVERAGE issue, not an organizational one
     - The actual kakari recovery rate would be much higher if the
       pre-1944 directories were fully digitized at kakari level

Recommendations:
  1. For kyoku-level crosswalk: use the known reorganization mapping above
  2. For ka-level crosswalk: use worker-level matching (staff_id)
     across years to track which ka people moved from
  3. For kakari-level crosswalk: the pre-1944 master data lacks kakari,
     so either OCR the pre-1944 directories at kakari level, or use
     worker-level tracking as a proxy