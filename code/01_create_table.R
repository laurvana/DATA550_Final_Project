# Situate yourself 
here::i_am(
  "code/01_create_table.R"
)

# Load in the data
load("data/shape_zcta_restrict.RData")

# Create a Stratifying Variable
shape_zcta_restrict$pct_poverty_med <- ifelse(shape_zcta_restrict$pct_poverty < median(shape_zcta_restrict$pct_poverty, na.rm=TRUE), 
                                              "Below Median Poverty Status", "Above Median Poverty Status")

# Restrict Data set based on NAs
shape_zcta_restrict2 <- shape_zcta_restrict[!is.na(shape_zcta_restrict$pct_poverty_med),]

# Create Labels
labelled::var_label(shape_zcta_restrict2) <- list(
  pct_work_pub_transp = "% of Workers who use Public Transportation",
  pct_poverty = "% Poverty Status",
  pct_no_vehicle = "% Households without Vehicle",
  dist_to_transit_stop = "Distance to Closest Transit Stop (m)",
  pub_transp_per_capita = "Per Capita Expense on Public Transportation"
)

# Load package to create table 1
library(gtsummary)

# Create a Table 1
table_one <- shape_zcta_restrict2 |>
  select("pub_transp_per_capita", "dist_to_transit_stop", "pct_work_pub_transp", "pct_no_vehicle", "pct_poverty_med") |>
  tbl_summary(by = pct_poverty_med, 
              type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})", "{median} [{min}, {max}]")) |>
  add_overall()%>%
  bold_labels()

# save table
saveRDS(
  table_one,
  file = here::here("output/table_one.rds")
)
