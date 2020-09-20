Plot3 <- function(filename="household_power_consumption.txt")
{
  library(dplyr)
  
  # Read the data from working directory
  data <- read.csv2(filename)
  
  # Prepare data formats date 
  data <- mutate(data, newdate = as.Date(Date,, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")))
  data_plot <- subset(data, newdate>="2007-02-01" & newdate<"2007-02-03")
  data_plot_corr <- mutate(data_plot, Global_active_power = as.numeric(Global_active_power))
  data_plot_corr <- mutate(data_plot_corr, Sub_metering_1 = as.numeric(Sub_metering_1))
  data_plot_corr <- mutate(data_plot_corr, Sub_metering_2 = as.numeric(Sub_metering_2))
  data_plot_corr <- mutate(data_plot_corr, Sub_metering_3 = as.numeric(Sub_metering_3))
  data_plot_corr <- mutate(data_plot_corr, datetime = strptime(paste(data_plot_corr$newdate, data_plot_corr$Time), "%Y-%m-%d %H:%M:%S"))
  data_plot_corr$datetime <- as.POSIXct(data_plot_corr$datetime)
  
  # Plot the data
  par(mfrow = c(1, 1))
  plot(data_plot_corr$Sub_metering_1 ~ data_plot_corr$datetime, type = "l",ylab = "Energy sub metering Wh", xlab = "")
  lines(data_plot_corr$Sub_metering_2 ~ data_plot_corr$datetime, col = "Red")
  lines(data_plot_corr$Sub_metering_3 ~ data_plot_corr$datetime, col = "Blue")
  legend("topright", lty = 1, col = c("black", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  # Copy to PNG File and close
  dev.copy(png, file = "plot3.png", height = 480, width = 480)
  dev.off()
}