library(yaml)

read_config <- function(config_file = Sys.getenv("FARMERS_CONFIG", "config.yaml"), section = 'elastic') {
    if (!file.exists(config_file))
        stop("No config file found!")
    
    config <- yaml.load_file(config_file)
    if (section %in% names(config)){
      config$elastic <- config[[section]]
      if (all(c("host", "username", "password", "port", "index") %in% names(config$elastic)))
          return (config)
    }
    
    stop("No Elastic credentials found in the config file!")
}
