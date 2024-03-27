library(dplyr)

column_rename <- c(
  'Fail rate' = 'fail_rate',
  'Number of jobs' = 'number_of_jobs',
  'Wasted memory fraction' = 'mem_wasted_frac',
  'Wasted CPU fraction' = 'cpu_wasted_frac',
  'Wasted CPU (hrs)' = 'cpu_wasted_hrs',
  'Requested CPU (hrs)' = 'cpu_avail_hrs',
  'Requested memory (GB x hrs)' = 'mem_avail_gb_hrs',
  'Wasted memory (GB x hrs)' = 'mem_wasted_gb_hrs',
  'Wasted money' = 'wasted_cost',
  'Accounting name' = 'accounting_name',
  'User name' = 'USER_NAME'
)

team_map <- tibble::enframe(
  c(
    'team152' = 'Anderson group',
    'team281' = 'Martin group',
    'team282' = 'Davenport group',
    'team354' = 'Lehner group',
    'team227' = 'Parts group',
    'team29-grp' = 'Hurles group',
    'team170' = 'Gaffney group',
    'team151' = 'Soranzo group'
  ),
  name = 'team_code',
  value = 'team_name'
)

rename_group_column <- function(df) {
  df %>%
    left_join(team_map, by = c('accounting_name' = 'team_code')) %>%
    mutate(accounting_name = ifelse(is.na(team_name), accounting_name, team_name)) %>%
    select(-team_name)
}

make_dt <- function(df){
  if('wasted_cost' %in% colnames(df))
    df <- dplyr::arrange(df, desc(wasted_cost))

  dt <- DT::datatable(
    df,
    options = list(dom = table_view_opts),
    colnames = column_rename[column_rename %in% colnames(df)]
  )

  if('mem_avail_gb_hrs' %in% colnames(df)){
    dt <- DT::formatRound(dt, 'Requested memory (GB x hrs)', 0)
  }

  if('mem_wasted_gb_hrs' %in% colnames(df)){
    dt <- DT::formatRound(dt, 'Wasted memory (GB x hrs)', 0)
  }

  if('mem_wasted_frac' %in% colnames(df))
    dt <- DT::formatPercentage(dt, 'Wasted memory fraction', 2)

  if('cpu_avail_hrs' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Requested CPU (hrs)', 0)

  if('cpu_wasted_hrs' %in% colnames(df))
    dt <- DT::formatRound(dt, 'Wasted CPU (hrs)', 0)

  if('cpu_wasted_frac' %in% colnames(df))
    dt <- DT::formatPercentage(dt, 'Wasted CPU fraction', 2)

  if('wasted_cost' %in% colnames(df))
    dt <- DT::formatCurrency(dt, 'Wasted money', currency = '£', digits = 1)

  if('fail_rate' %in% colnames(df))
    dt <- DT::formatPercentage(dt, 'Fail rate', 1)

  return(dt)
}
