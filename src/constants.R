index <- "user-data-ssg-isg-lsf-analytics-*"

cpu_hour <- 0.00254  # in £
cpu_second <- cpu_hour / 60 / 60

ram_gb_hour <- 0.000217  # in £
ram_mb_second <- ram_gb_hour / 1024 / 60 / 60

awesomeness_explanation <- "
Awesome-ness is a 0-to-10 complex score representing a team performance. 
A team gets score 10 if it has the lowest fraction of wasted resources for both CPU and RAM. 
The more heavy jobs team submits the harder it is to be efficient and vice versa. 
For this reason teams get penalised if they consume very little resources.
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