suppressPackageStartupMessages({
  library(tidyverse)
})

data_path <- file.path(
  Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv"
)
raw <- read_csv(data_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
named <- raw %>% filter(is_name, !is.na(staff_id), staff_id != "")

cat("1942 government coverage (rows and distinct workers):\n")
print(named %>% filter(year == 1942) %>% group_by(gov_level) %>% summarise(rows=n(), workers=n_distinct(staff_id), .groups="drop"))

d44 <- named %>% filter(year == 1944, gov_level == "TokyoTo")
dup <- d44 %>% add_count(staff_id, name = "id_rows") %>% filter(id_rows > 1)
cat("\n1944 duplicate structure:\n")
print(tibble(
  raw_rows = nrow(d44), unique_ids = n_distinct(d44$staff_id),
  duplicate_excess_rows = nrow(d44) - n_distinct(d44$staff_id),
  ids_with_multiple_rows = n_distinct(dup$staff_id),
  exact_duplicate_excess = nrow(d44) - nrow(distinct(d44)),
  ids_multiple_offices = dup %>% group_by(staff_id) %>% summarise(n=n_distinct(office_id),.groups="drop") %>% summarise(n=sum(n>1)) %>% pull(n),
  ids_multiple_positions = dup %>% group_by(staff_id) %>% summarise(n=n_distinct(position),.groups="drop") %>% summarise(n=sum(n>1)) %>% pull(n),
  ids_multiple_units = dup %>% group_by(staff_id) %>% summarise(n=n_distinct(paste(kyoku,ka,kakari,sep="||")),.groups="drop") %>% summarise(n=sum(n>1)) %>% pull(n)
), width=Inf)

cat("\nDistribution of rows per duplicated ID:\n")
print(d44 %>% count(staff_id) %>% count(n, name="ids") %>% arrange(n), n=Inf)

cat("\nHow often fields vary within duplicated IDs:\n")
print(dup %>% group_by(staff_id) %>% summarise(
  offices=n_distinct(office_id), positions=n_distinct(position),
  kyoku=n_distinct(kyoku), ka=n_distinct(ka), kakari=n_distinct(kakari),
  names=n_distinct(name), pages=n_distinct(page), drafted_values=n_distinct(drafted),
  .groups="drop") %>% summarise(across(everything(), ~sum(.x>1))), width=Inf)

cat("\nFirst 20 multi-assignment examples:\n")
print(dup %>% group_by(staff_id) %>% filter(n_distinct(paste(office_id,position,kyoku,ka,kakari))>1) %>% ungroup() %>% select(staff_id,name,office_id,kyoku,ka,kakari,position,page,image,x,y) %>% arrange(staff_id,page,y) %>% slice_head(n=60), n=60)

# Link rates by 1944 kyoku under a deterministic primary-row rule: prefer the
# most complete hierarchy, then the earliest page and topmost record.
choose_primary <- function(d) {
  d %>% mutate(completeness = rowSums(!is.na(pick(kyoku, ka, kakari, office_id, position)) & pick(kyoku, ka, kakari, office_id, position) != "")) %>%
    arrange(staff_id, desc(completeness), page, y, x) %>%
    distinct(staff_id, year, .keep_all=TRUE)
}
w44 <- choose_primary(d44)
ids42 <- named %>% filter(year==1942) %>% pull(staff_id) %>% unique()
cat("\n1944 link rates by selected primary kyoku:\n")
print(w44 %>% mutate(kyoku_report=coalesce(kyoku,"<missing>"), is_linked=staff_id %in% ids42) %>% group_by(kyoku_report) %>% summarise(workers=n(), linked=sum(is_linked), link_rate=mean(is_linked),.groups="drop") %>% arrange(link_rate), n=Inf)
