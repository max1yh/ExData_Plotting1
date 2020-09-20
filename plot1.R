Plot1 <- function(filename="household_power_consumption.txt")
{
  library(dplyr)
  
  # Read the data from working directory
  data <- read.csv2(filename)
  
  # Prepare data formats date 
  data <- mutate(data, newdate = as.Date(Date,, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")))
  data_plot <- subset(data, newdate>="2007-02-01" & newdate<"2007-02-03")
  data_plot_corr <- mutate(data_plot, Global_active_power = as.numeric(Global_active_power))
  
  # Plot the data
  par(mfrow = c(1, 1))
  hist(data_plot_corr$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kW)")
  
  # Copy to PNG File
  dev.copy(png, file = "plot1.png", height = 480, width = 480)
  
  # Close the plot file
  dev.off()
  
  # Return the data, just in case ;-)
  # data_plot_corr
}
