library(magrittr)

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

rename_groups <- function(names) {
  rules <- c(
    '^team152$' = 'Anderson group',
    '^team281$' = 'Martin group',
    '^team282$' = 'Davenport group',
    '^team354$' = 'Lehner group',
    '^team227$' = 'Parts group',
    '^team29-grp$' = 'Hurles group'
  )
  stringr::str_replace_all(names, rules)
}

rename_group_column <- function(df) {
  mutate(df, accounting_name = rename_groups(accounting_name))
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
