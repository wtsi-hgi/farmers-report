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
If process allocates only 1 cpu and uses a fraction of it, we still consider it as wasting resources.
However it would be difficult to optimize that process.
Let's assume that successful processes requiring 1 cpu do not waste cpu.
"

time_buckets <- c("day", "week", "month")
elastic_bucket_aggregations <- c("terms", "multi_terms", "date_histogram")

elastic_column_map <- c(
  'cpu_avail_sec' = 'AVAIL_CPU_TIME_SEC',
  'mem_avail_mb_sec' = 'MEM_REQUESTED_MB_SEC',
  'mem_wasted_mb_sec' = 'WASTED_MB_SECONDS',
  'cpu_wasted_sec' = 'WASTED_CPU_SECONDS',
  'procs' = 'NUM_EXEC_PROCS',
  'job_status' = 'Job',
  'job_name' = 'JOB_NAME',
  'accounting_name' = 'ACCOUNTING_NAME'
)
