suppressPackageStartupMessages({library(tidyverse); library(fixest)})
set.seed(1944)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out <- file.path(root, "MainResults", "PreDraftWorkerPools1944")
tab <- file.path(out, "tables")
dir.create(tab, recursive = TRUE, showWarnings = FALSE)
data_path <- file.path(Sys.getenv("USERPROFILE"), "Box",
  "Research Notes (keitaro2@illinois.edu)", "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv")

norm_pos <- function(x) {
  out <- str_remove_all(coalesce(x, ""), "\\s+")
  if_else(out == "", "Unknown", out)
}
rank_pos <- function(x) case_when(
  x %in% c("Unknown", "\u517c") ~ NA_integer_,                              # 兼
  str_detect(x, "\u96c7|\u56d1\u8a17|\u81e8\u6642|\u898b\u7fd2") ~ 1L,     # 雇|囑託|臨時|見習
  str_detect(x, "\u6280\u5e2b|\u4e8b\u52d9\u5b98|\u66f8\u8a18\u5b98") |    # 技師|事務官|書記官
    str_ends(x, "\u9577") | x == "\u4e3b\u4e8b" ~ 3L,                       # 長 | 主事
  TRUE ~ 2L)
track_pos <- function(x, r) case_when(
  r == 1L & str_detect(x, "\u6280") ~ "technical",   # 技
  r == 1L ~ "unknown",
  str_detect(x, "\u6280") ~ "technical",              # 技
  str_detect(x, paste0("\u66f8\u8a18|\u4e3b\u4e8b|\u4e8b\u52d9|",           # 書記|主事|事務|
    "\u4fc2\u9577|\u8ab2\u9577|\u6240\u9577|\u5c40\u9577|\u6b21\u9577|",     # 係長|課長|所長|局長|次長|
    "\u5834\u9577|\u76e3\u8996|\u5de1\u8996|\u76e3\u540f|\u76e3\u7763")) ~   # 場長|監視|巡視|監吏|監督
    "clerical",
  TRUE ~ "unknown")
observed <- function(x) !is.na(x) & x != ""
label <- function(x) if_else(observed(x), x, "<missing>")

raw <- read_csv(data_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
named <- raw %>% filter(is_name, observed(staff_id)) %>% mutate(
  source_order = row_number(),
  completeness = rowSums(observed(pick(kyoku, ka, kakari, office_id, position))))
draft_any <- named %>% group_by(staff_id, year) %>%
  summarise(drafted_any = any(drafted %in% TRUE), .groups = "drop")

# A deterministic primary listing: most complete hierarchy, then earliest/topmost.
workers <- named %>%
  arrange(staff_id, year, desc(completeness), page, y, x, source_order) %>%
  distinct(staff_id, year, .keep_all = TRUE) %>%
  left_join(draft_any, by = c("staff_id", "year")) %>%
  rename(court_rank = rank) %>%
  mutate(
    drafted = drafted_any, year = as.integer(year), occupation = norm_pos(position),
    position_rank = rank_pos(occupation), track = track_pos(occupation, position_rank),
    male = gender_modern == "male", kyoku_obs = observed(kyoku), ka_obs = observed(ka),
    kakari_obs = observed(kakari), kyoku_l = label(kyoku), ka_l = label(ka),
    kakari_l = label(kakari), unit = paste(kyoku_l, ka_l, kakari_l, sep = " || "),
    ka_cluster = if_else(kyoku_obs & ka_obs, paste(kyoku, ka, sep = " || "), NA_character_))
w42 <- workers %>% filter(year == 1942, gov_level == "TokyoShi")
w44 <- workers %>% filter(year == 1944, gov_level == "TokyoTo")
m42 <- w42 %>% filter(male); m44 <- w44 %>% filter(male)
ids42 <- unique(w42$staff_id); ids44 <- unique(w44$staff_id)
linked <- intersect(ids42, ids44)
draft44 <- m44 %>% filter(drafted) %>% pull(staff_id) %>% unique()

# Required audits.
w44 %>% filter(drafted, gender_modern == "female") %>%
  select(staff_id, name, position, occupation, gender_modern, kyoku, ka, kakari, office, office_id) %>%
  write_csv(file.path(tab, "drafted_women_1944_audit.csv"))
named %>% filter(year == 1944, gov_level == "TokyoTo") %>% mutate(
  occupation = norm_pos(position), position_rank = rank_pos(occupation),
  track = track_pos(occupation, position_rank),
  rank_flag = if_else(occupation %in% c("Unknown", "\u517c"), "missing_by_rule", "")) %>%
  group_by(occupation, position_rank, track, rank_flag) %>%
  summarise(workers_1944 = n_distinct(staff_id), .groups = "drop") %>%
  arrange(desc(workers_1944)) %>% write_csv(file.path(tab, "position_rank_track_map_1944.csv"))
w44 %>% mutate(rank_report = coalesce(as.character(position_rank), "missing")) %>%
  count(rank_report, name = "workers_1944") %>% mutate(share = workers_1944/sum(workers_1944)) %>%
  write_csv(file.path(tab, "rank_distribution_1944.csv"))
named %>% filter(year == 1942) %>% group_by(gov_level) %>%
  summarise(rows=n(), workers=n_distinct(staff_id), .groups="drop") %>%
  write_csv(file.path(tab, "government_coverage_1942.csv"))

link_rates <- w44 %>% mutate(is_linked = staff_id %in% ids42) %>% group_by(kyoku_l) %>%
  summarise(workers_1944=n(), linked_1942=sum(is_linked), link_rate=mean(is_linked), .groups="drop")
write_csv(link_rates, file.path(tab, "link_rate_by_kyoku_1944.csv"))
high_link <- link_rates %>% filter(kyoku_l != "<missing>", link_rate > .5) %>% pull(kyoku_l)

raw44 <- raw %>% filter(year==1944, gov_level=="TokyoTo", is_name, observed(staff_id))
dup_audit <- tibble(
  raw_rows=nrow(raw44), unique_staff_ids=n_distinct(raw44$staff_id),
  excess_rows=nrow(raw44)-n_distinct(raw44$staff_id),
  exact_duplicate_excess=nrow(raw44)-nrow(distinct(raw44)),
  ids_multiple_offices=raw44 %>% group_by(staff_id) %>% summarise(z=n_distinct(office_id),.groups="drop") %>% summarise(z=sum(z>1)) %>% pull(),
  ids_multiple_positions=raw44 %>% group_by(staff_id) %>% summarise(z=n_distinct(position),.groups="drop") %>% summarise(z=sum(z>1)) %>% pull(),
  primary_rule="most complete hierarchy; ties earliest page, topmost y/x, source order")
write_csv(dup_audit, file.path(tab, "duplicate_post_audit_1944.csv"))

# Linked workers use 1942 assignments; unlinked draftees use 1944 assignments.
sources <- bind_rows(
  m42 %>% filter(staff_id %in% linked, !is.na(position_rank)),
  m44 %>% filter(staff_id %in% setdiff(draft44, linked), !is.na(position_rank))) %>%
  distinct(staff_id, .keep_all=TRUE) %>% transmute(
    staff_id, sy=year, skyoku=kyoku_l, ska=ka_l, skakari=kakari_l, sunit=unit,
    socc=occupation, srank=position_rank, strack=track,
    skyoku_obs=kyoku_obs, ska_obs=ka_obs, skakari_obs=kakari_obs)

cells <- m44 %>% filter(!is.na(position_rank)) %>%
  group_by(kyoku_l, ka_l, kakari_l, unit, occupation) %>% summarise(
    cell_rank=first(position_rank), cell_track=first(track),
    Draft=n_distinct(staff_id[drafted]),
    Staff=n_distinct(staff_id[staff_id %in% linked & !drafted]),
    workers_1944=n_distinct(staff_id), kyoku_obs=first(kyoku_obs),
    ka_obs=first(ka_obs), kakari_obs=first(kakari_obs), ka_cluster=first(ka_cluster),
    .groups="drop") %>% mutate(cell_id=row_number())

# Missing kyoku/ka gives unknown. Missing kakari prevents own-kakari only.
distance <- function(sk, sa, su, dk, da, du, sko, sao, sko2, dko, dao, dko2) case_when(
  !sko | !sao | !dko | !dao ~ "unknown",
  sko2 & dko2 & su == du ~ "own_kakari",
  sk == dk & sa == da ~ "same_ka", sk == dk ~ "same_kyoku", TRUE ~ "other_kyoku")

cross <- crossing(cells %>% transmute(cell_id, dkyoku=kyoku_l, dka=ka_l, dunit=unit,
  docc=occupation, drank=cell_rank, dtrack=cell_track, dkyoku_obs=kyoku_obs,
  dka_obs=ka_obs, dkakari_obs=kakari_obs), sources) %>%
  filter(sunit != dunit | socc != docc) %>% mutate(
    dist=distance(skyoku,ska,sunit,dkyoku,dka,dunit,skyoku_obs,ska_obs,skakari_obs,
                  dkyoku_obs,dka_obs,dkakari_obs),
    lat=srank==drank & socc==docc,
    pro_track=srank==drank-1L & (strack==dtrack | (srank==1L & strack=="unknown" & dtrack %in% c("clerical","technical"))),
    pro_nt=srank==drank-1L)

wide_pool <- function(flag, prefix, keep_own=TRUE) cross %>% filter({{flag}}, keep_own | dist!="own_kakari", dist!="unknown") %>%
  count(cell_id, dist) %>% pivot_wider(names_from=dist, values_from=n, values_fill=0, names_prefix=prefix)
lat <- wide_pool(lat, "lat_", FALSE); pro <- wide_pool(pro_track, "pro_"); pro_nt <- wide_pool(pro_nt,"pro_nt_")
latv <- paste0("lat_",c("same_ka","same_kyoku","other_kyoku"))
prov <- paste0("pro_",c("own_kakari","same_ka","same_kyoku","other_kyoku"))
ntv <- paste0("pro_nt_",c("own_kakari","same_ka","same_kyoku","other_kyoku"))
ensure <- function(d,v){for(x in setdiff(v,names(d))) d[[x]]<-0; d}
panel <- cells %>% left_join(lat,by="cell_id") %>% left_join(pro,by="cell_id") %>% left_join(pro_nt,by="cell_id") %>%
  ensure(c(latv,prov,ntv)) %>% mutate(across(all_of(c(latv,prov,ntv)),~replace_na(.x,0)))

arr <- m44 %>% filter(!is.na(position_rank)) %>% left_join(m42 %>% transmute(staff_id,
  okyoku=kyoku_l, oka=ka_l, ounit=unit, oocc=occupation, orank=position_rank,
  okyoku_obs=kyoku_obs, oka_obs=ka_obs, okakari_obs=kakari_obs),by="staff_id") %>% mutate(
  arrival=!is.na(orank) & paste(unit,occupation)!=paste(ounit,oocc),
  dist=distance(okyoku,oka,ounit,kyoku_l,ka_l,unit,okyoku_obs,oka_obs,okakari_obs,kyoku_obs,ka_obs,kakari_obs),
  type=case_when(!arrival~NA_character_,position_rank==orank~"lateral",position_rank>orank~"promotion",TRUE~"other"),
  first_year=map_int(staff_id,~min(workers$year[workers$staff_id==.x])), new_hire=first_year==1944)
outs <- arr %>% group_by(unit,occupation) %>% summarise(
  lateral_same_ka=sum(type=="lateral"&dist=="same_ka",na.rm=T), lateral_same_kyoku=sum(type=="lateral"&dist=="same_kyoku",na.rm=T),
  lateral_other_kyoku=sum(type=="lateral"&dist=="other_kyoku",na.rm=T), promotion_same_ka=sum(type=="promotion"&dist=="same_ka",na.rm=T),
  promotion_same_kyoku=sum(type=="promotion"&dist=="same_kyoku",na.rm=T), promotion_other_kyoku=sum(type=="promotion"&dist=="other_kyoku",na.rm=T),
  new_hires=sum(new_hire),.groups="drop")
panel <- panel %>% left_join(outs,by=c("unit","occupation")) %>% mutate(
  across(c(starts_with("lateral_"),starts_with("promotion_"),new_hires),~replace_na(.x,0)),
  logStaff=log1p(Staff),across(all_of(c(latv,prov,ntv)),log1p,.names="log_{.col}"))
write_csv(panel,file.path(tab,"analysis_panel.csv"))

# OLS with clustered covariance and Webb six-point restricted-null bootstrap.
vcov_cluster <- function(X,e,g){G=n_distinct(g); if(G<2)return(matrix(NA_real_,ncol(X),ncol(X))); b=solve(crossprod(X)); s=rowsum(X*e,g); G/(G-1)*(nrow(X)-1)/(nrow(X)-ncol(X))*b%*%crossprod(s)%*%b}
ols <- function(d,y,pools,spec,boot=FALSE,cluster="kyoku_l"){
  d=d %>% filter(observed(.data[[cluster]])); lp=paste0("log_",pools)
  rhs=c("Draft",paste0("Draft:",lp),"Draft:logStaff",lp,"logStaff")
  fe=c(if(n_distinct(d$kyoku_l)>1)"factor(kyoku_l)",if(n_distinct(d$occupation)>1)"factor(occupation)")
  f=as.formula(paste(y,"~",paste(c(rhs,fe),collapse="+"))); mf=model.frame(f,d); X0=model.matrix(f,mf); yy=model.response(mf)
  q=qr(X0); X=X0[,sort(q$pivot[seq_len(q$rank)]),drop=F]; fit=lm.fit(X,yy); names(fit$coefficients)=colnames(X)
  g=d[[cluster]][as.integer(rownames(mf))]; G=n_distinct(g)
  # Return a clean failure row when too few clusters for inference
  if(G<2) return(tibble(specification=spec,outcome=y,estimator="OLS",
    term=names(fit$coefficients)[names(fit$coefficients)%in%c("Draft",paste0("Draft:",lp),"Draft:logStaff",lp,"logStaff")],
    estimate=NA_real_,std_error=NA_real_,p_value=NA_real_,wild_p_value=NA_real_,
    observations=nrow(X),clusters=G,diagnostic="Inference unavailable: fewer than two clusters"))
  V=vcov_cluster(X,fit$residuals,g); se=sqrt(pmax(diag(V),0))
  keep=names(fit$coefficients)%in%c("Draft",paste0("Draft:",lp),"Draft:logStaff",lp,"logStaff")
  z=tibble(specification=spec,outcome=y,estimator="OLS",term=names(fit$coefficients)[keep],estimate=fit$coefficients[keep],
    std_error=se[keep],p_value=2*pt(abs(fit$coefficients[keep]/se[keep]),max(G-1,1),lower.tail=F),wild_p_value=NA_real_,
    observations=nrow(X),clusters=G,diagnostic="")
  # Wild Cluster Score Bootstrap (WCS) with Webb 6-point weights.
  # Compares t-statistics, not raw coefficients. No restricted-model refitting.
  # t*_b = (wm' phi) / se[j]  where phi_g = (s_mat %*% b_mat[,j])_g are cluster-level scores.
  if(boot && G>=2){B=9999L; W=c(-sqrt(1.5),-1,-sqrt(.5),sqrt(.5),1,sqrt(1.5)); wm=matrix(sample(W,G*B,TRUE),G,B)
    b_mat=solve(crossprod(X)); s_mat=rowsum(X*fit$residuals,g)
    for(term in intersect(c(paste0("Draft:",lp),"Draft:logStaff"),colnames(X))){
      j=match(term,colnames(X)); sej=se[j]
      if(!is.finite(sej)||sej==0){z$wild_p_value[z$term==term]=NA_real_;next}
      phi=drop(s_mat%*%b_mat[,j,drop=FALSE])
      t_obs=fit$coefficients[j]/sej; t_boot=drop(crossprod(wm,phi))/sej
      z$wild_p_value[z$term==term]=mean(abs(t_boot)>=abs(t_obs))}}
  z}
ppml <- function(d,y,pools){lp=paste0("log_",pools); f=as.formula(paste(y,"~",paste(c("Draft",paste0("Draft:",lp),"Draft:logStaff",lp,"logStaff"),collapse="+"),"|kyoku_l+occupation"))
  m=tryCatch(fepois(f,d,cluster=~kyoku_l,warn=FALSE,notes=FALSE),error=function(e)e); if(inherits(m,"error"))return(tibble(specification="robust_b_ppml",outcome=y,estimator="PPML",term="<model failed>",estimate=NA,std_error=NA,p_value=NA,wild_p_value=NA,observations=nrow(d),clusters=n_distinct(d$kyoku_l),diagnostic=conditionMessage(m)))
  ct=as.data.frame(coeftable(m))%>%rownames_to_column("term")%>%filter(term%in%c("Draft",paste0("Draft:",lp),"Draft:logStaff",lp,"logStaff")); transmute(ct,specification="robust_b_ppml",outcome=y,estimator="PPML",term,estimate=Estimate,std_error=`Std. Error`,p_value=`Pr(>|z|)`,wild_p_value=NA_real_,observations=nobs(m),clusters=n_distinct(d$kyoku_l),diagnostic="")}
run <- function(ys,pools,fam) map_dfr(ys,function(y)bind_rows(ols(panel,y,pools,"main_exact_match",T),ols(panel,y,pools,"robust_a_exact_match_identical"),ppml(panel,y,pools),ols(filter(panel,Staff>=3),y,pools,"robust_c_staff_ge_3"),ols(filter(panel,ka_obs),y,pools,"robust_e_observed_ka"),ols(filter(panel,kyoku_l%in%high_link),y,pools,"robust_f_kyoku_link_gt_50"),ols(filter(panel,ka_obs),y,pools,"sensitivity_cluster_nested_ka",cluster="ka_cluster")))%>%mutate(family=fam,.before=1)
latr=run(paste0("lateral_",c("same_ka","same_kyoku","other_kyoku")),latv,"lateral")
pror=bind_rows(run(paste0("promotion_",c("same_ka","same_kyoku","other_kyoku")),prov,"promotion"),map_dfr(paste0("promotion_",c("same_ka","same_kyoku","other_kyoku")),~ols(panel,.x,ntv,"robust_d_ignore_track"))%>%mutate(family="promotion",.before=1))
newr=run("new_hires",c(latv,prov),"new_hires")
write_csv(latr,file.path(tab,"regression_lateral.csv"));write_csv(pror,file.path(tab,"regression_promotion.csv"));write_csv(newr,file.path(tab,"regression_new_hires.csv"))

write_tex <- function(x,path,cap){x=x%>%filter(str_detect(term,"^Draft($|:log_)")); cs=unique(x$outcome); L=c("\\begin{table}[!htbp]","\\centering",paste0("\\caption{",cap,"}"),paste0("\\begin{tabular}{ll",str_dup("r",length(cs)),"}"),"\\hline",paste(c("Specification","Term",cs),collapse=" & "),"\\\\","\\hline");for(i in seq_len(nrow(distinct(x,specification,term)))){k=distinct(x,specification,term)[i,];v=map_chr(cs,~{q=x%>%filter(specification==k$specification,term==k$term,outcome==.x)%>%slice(1);if(!nrow(q)||is.na(q$estimate))return("");paste0(sprintf("%.3f",q$estimate)," (",sprintf("%.3f",q$std_error),"; p=",sprintf("%.3f",q$p_value),ifelse(is.na(q$wild_p_value),"",paste0("; Webb p=",sprintf("%.3f",q$wild_p_value))),")")});L=c(L,paste(c(k$specification,k$term,v),collapse=" & "),"\\\\")};writeLines(c(L,"\\hline","\\end{tabular}","\\end{table}"),path,useBytes=T)}
write_tex(latr,file.path(tab,"regression_lateral.tex"),"Lateral arrivals");write_tex(pror,file.path(tab,"regression_promotion.tex"),"Promotion arrivals");write_tex(newr,file.path(tab,"regression_new_hires.tex"),"New hires")

arrivals=arr%>%filter(arrival); latcand=cross%>%filter(lat); procand=cross%>%filter(pro_nt)
diagnostics=tibble(item=c("raw_named_rows_1942","unique_workers_1942","raw_named_rows_1944","unique_workers_1944","unique_raw_titles_1944","drafted_men_1942","drafted_women_1942","drafted_men_1944","drafted_women_1944","rank_missing_workers_1944","rank_missing_men_1944","rank1_1944","rank2_1944","rank3_1944","linked_workers_1944","share_workers_1944_linked_1942","analysis_cells","cells_staff_below_3","cells_observed_ka","arrivals","arrivals_unknown_distance","share_arrivals_unknown_distance","lateral_pool_candidates","lateral_pool_unknown_distance","share_lateral_pool_unknown_distance","promotion_pool_candidates","promotion_pool_unknown_distance","share_promotion_pool_unknown_distance","kyoku_clusters","nested_ka_clusters","high_link_kyoku_count","exact_duplicate_excess_1944","ids_multiple_positions_1944","ids_multiple_offices_1944"),value=c(nrow(named%>%filter(year==1942)),nrow(w42),nrow(raw44),nrow(w44),n_distinct(norm_pos(raw44$position)),sum(w42$drafted&w42$gender_modern=="male"),sum(w42$drafted&w42$gender_modern=="female"),sum(w44$drafted&w44$gender_modern=="male"),sum(w44$drafted&w44$gender_modern=="female"),sum(is.na(w44$position_rank)),sum(is.na(m44$position_rank)),sum(w44$position_rank==1,na.rm=T),sum(w44$position_rank==2,na.rm=T),sum(w44$position_rank==3,na.rm=T),sum(ids44%in%ids42),mean(ids44%in%ids42),nrow(panel),sum(panel$Staff<3),sum(panel$ka_obs),nrow(arrivals),sum(arrivals$dist=="unknown"),mean(arrivals$dist=="unknown"),nrow(latcand),sum(latcand$dist=="unknown"),mean(latcand$dist=="unknown"),nrow(procand),sum(procand$dist=="unknown"),mean(procand$dist=="unknown"),n_distinct(panel$kyoku_l),n_distinct(na.omit(panel$ka_cluster)),length(high_link),dup_audit$exact_duplicate_excess,dup_audit$ids_multiple_positions,dup_audit$ids_multiple_offices))
write_csv(diagnostics,file.path(tab,"diagnostics.csv"))
summary=tibble(statistic=c("cells","male_workers_1944","male_draftees_1944","linked_workers_1944","share_1944_linked_to_1942","mean_staff",paste0("mean_",latv),paste0("mean_",prov)),value=c(nrow(panel),nrow(m44),sum(m44$drafted),sum(ids44%in%ids42),mean(ids44%in%ids42),mean(panel$Staff),map_dbl(latv,~mean(panel[[.x]])),map_dbl(prov,~mean(panel[[.x]]))))
write_csv(summary,file.path(tab,"summary_statistics.csv"))
summary_tex <- c("\\begin{tabular}{lr}", "\\hline", "Statistic & Value \\\\", "\\hline",
  paste0(summary$statistic, " & ", sprintf("%.4f",summary$value), " \\\\"),
  "\\hline", "\\end{tabular}")
writeLines(summary_tex, file.path(tab,"summary_statistics.tex"))
cat("V2 complete\n");print(diagnostics,n=Inf)
