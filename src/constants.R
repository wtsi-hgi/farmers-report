cpu_hour <- 0.00254  # in £
cpu_second <- cpu_hour / 60 / 60

ram_gb_hour <- 0.000217  # in £
ram_mb_second <- ram_gb_hour / 1024 / 60 / 60

awesomeness_explanation <- "
Awesome-ness is a 0-to-10 score representing a team performance:
Teams are first independently ranked (descending) on both wasted CPU fraction and wasted RAM fraction. The awesomeness score is the mean of those two (suitably normalised) ranks. Therefore, to get an awesomeness score of 10, a team should have low wasted percentage CPU and low wasted percentage RAM in the same time period. Note: we penalise teams  which only request small amounts of CPU or RAM resources (relative to other teams) by halving their ranks before calculating their awesomeness.
"

awesomeness_formula <- "
$$
awesomeness = 10 \\times \\frac{rank_{wasted\\ CPU\\ fraction} + rank_{wasted\\ RAM\\ fraction}}{ 2 \\times number\\ of\\ LSF\\ groups}
$$

$$
rank_{metric} = \\begin{cases}
 metric \\leq \\dfrac {median(metric)} {2} ,& \\dfrac {rank(-metric)} {2} \\\\
 otherwise  ,& rank(-metric)
\\end{cases}
$$
"

adjustments_explanation <- "
Assume that successful processes requiring 1 cpu do not waste cpu.
"

time_buckets <- c("day", "week", "month")
elastic_bucket_aggregations <- c("terms", "multi_terms", "date_histogram")

success <- 'Success'
failed <- 'Failed'

raw_stats_elastic_columns <- c(
  'Job',
  'NUM_EXEC_PROCS', 'AVAIL_CPU_TIME_SEC', 'WASTED_CPU_SECONDS',
  'MEM_REQUESTED_MB', 'MEM_REQUESTED_MB_SEC', 'WASTED_MB_SECONDS'
)

elastic_column_map <- c(
  'cpu_avail_sec' = 'AVAIL_CPU_TIME_SEC',
  'mem_avail_mb_sec' = 'MEM_REQUESTED_MB_SEC',
  'mem_wasted_mb_sec' = 'WASTED_MB_SECONDS',
  'cpu_wasted_sec' = 'WASTED_CPU_SECONDS',
  'raw_mem_wasted_mb_sec' = 'RAW_WASTED_MB_SECONDS',
  'raw_cpu_wasted_sec' = 'RAW_WASTED_CPU_SECONDS',
  'procs' = 'NUM_EXEC_PROCS',
  'job_status' = 'Job',
  'job_name' = 'JOB_NAME',
  'accounting_name' = 'ACCOUNTING_NAME'
)

column_rename <- c(
  'Fail rate' = 'fail_rate',
  'Number of jobs' = 'number_of_jobs',
  'Wasted memory fraction' = 'mem_wasted_frac',
  'Wasted CPU fraction' = 'cpu_wasted_frac',
  'Wasted CPU (hrs)' = 'cpu_wasted_hrs',
  'Requested CPU (hrs)' = 'cpu_avail_hrs',
  'Requested memory (GB x hrs)' = 'mem_avail_gb_hrs',
  'Requested memory (GB)' = 'mem_avail_gb',
  'Wasted memory (GB x hrs)' = 'mem_wasted_gb_hrs',
  'Wasted money' = 'wasted_cost',
  'Accounting name' = 'accounting_name',
  'User name' = 'USER_NAME',
  'Awesome-ness' = 'awesomeness',
  'Job type' = 'job_type',
  'Queue name' = 'QUEUE_NAME',
  'Median Wait Time' = 'median_wait_time',
  'Median Run Time' = 'median_run_time',
  'Wait Time' = 'PENDING_TIME_SEC',
  'Run Time' = 'RUN_TIME_SEC',
  'Best efficiency' = 'best_eff',
  'Number of requested CPUs' = 'procs',
  'CPU efficiency' = 'Job_Efficiency',
  'RAM efficiency' = 'Memory_Efficiency',
  'Job status' = 'job_status',
  'Max used memory (GB)' = 'max_mem_used_gb',
  'Step' = 'step'
)

column_rename_inv <- setNames(names(column_rename), column_rename)

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
