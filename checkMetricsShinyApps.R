# Small script to derive the kernel usage in seconds for the app and also get the days when it was used

# check the CPU usage time (return in nanoseconds)
df <- as.data.frame(rsconnect::showMetrics(
  "docker_container_cpu",
  c("usage_in_kernelmode"),
  server = "shinyapps.io",
  account  = "johannesschielein",
  appName = "random-coffee",
  from = "3w",interval = "1d"
))
# convert usage to secondds
df$df_usage_in_kernelmode_seconds<-as.numeric(df$usage_in_kernelmode)/1e+9

# create a string with the dates to see at which day people connected (German date format)
df$mydates<-seq(Sys.Date()-21, Sys.Date(), by="days")
df$mydates<-format(df$mydates,"%a %d.%m.%Y")

View(df)
