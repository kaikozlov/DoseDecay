library(tidyverse)

# Constants
# Your constants
lbweight = [] # in lbs

# Your drug's constants
drugname = "[]"
initialdose = [] # in mg
mineffconc = [] # in mg/kg
maxrecdose = [] # in mg
halflife = [] # in hours

# Time Information
maxtime = [] # in hours
start_datetime = "[]" # in mm-dd xx:xx 24 hour time


# Additional Dose List. Syntax: Month-Day Time, How much? (mg)
additionaldoses <- tibble(
  datetime = c(
    "[]", # in mm-dd xx:xx 24 hour time
      ),
  dose = c([],) # in mg
)

# Formatting!
kgweight = lbweight * 0.453592
drugname = str_to_title(drugname)
current_year <- year(now())
start_datetime <- paste(current_year, start_datetime, sep = "-")

# Add the current year to the datetime for correct parsing
additionaldoses$datetime <- paste(current_year, additionaldoses$datetime, sep =
                                    "-")
start_time <- ymd_hm(start_datetime)  # Parses a string in "year-month-day hour:minute" format
additionaldoses$datetime <- ymd_hm(additionaldoses$datetime)

# Calculate hours from start time
additionaldoses$hours_from_start <- as.numeric(difftime(additionaldoses$datetime, start_time, units = "hours"))

# Generate a time sequence
time_seq <- seq(0, maxtime, by = 0.1)

concentration <- map_dbl(time_seq, ~ {
  current_time <- .  # current time point in the sequence
  
  # Initial dose decay
  initial_conc <- initialdose * 0.5 ^ (current_time / halflife)
  
  # Calculate contributions from additional doses
  additional_conc <- sum(
    additionaldoses$dose * ifelse(
      current_time >= additionaldoses$hours_from_start,
      0.5 ^ ((
        current_time - additionaldoses$hours_from_start
      ) / halflife),
      0
    )
  )
  
  # Total concentration
  total_conc <- initial_conc + additional_conc
  total_conc
})

# Data frame for ggplot
concentration_df <- tibble(hrdtime = time_seq, mgconc = concentration)

# Define hourly breaks in hours from start
hourly_breaks <- seq(0, maxtime, by = 6)  # Hourly intervals

# Calculate corresponding datetime for each break
hourly_labels_datetime <- start_time + hours(hourly_breaks)

# Format labels - Show full date every 24 hours, only time otherwise
hourly_labels <- format(hourly_labels_datetime, "%H:%M")  # e.g., "14:00"

#Establish graph day intercepts
day_marks_df <- tibble(xintercept = seq(0, maxtime, by = 24)) %>%
  mutate(color = "Days")

#Dynamic Y-Axis Scale Determination
if(maxrecdose<=50){
  scalestep=5
  } else if(maxrecdose<=100){
    scalestep=10
  } else if(maxrecdose<=200){
    scalestep=20
  } else if(maxrecdose<=500){
    scalestep=50
  }  else if(maxrecdose<=1000){
    scalestep=100
  }  else if(maxrecdose<=2000){
    scalestep=200
  }  else if(maxrecdose<=5000){
    scalestep=500
  }  else if(maxrecdose<=10000){
    scalestep=1000
  }  else if(maxrecdose<=20000){
    scalestep=2000
  } else{
    scalestep=5000
  }

# Decay function for the drug
decay_function <- function(t) {
  initialdose * 0.5 ^ (t / halflife)
}

# Plotting
ggplot(concentration_df, aes(x = hrdtime, y = mgconc)) +
  # Real and Predicted Lines
  geom_line(aes(color = "Cumulative Remaining Dose")) +
  geom_function(
    fun = decay_function,
    aes(color = "Initial Dose Decay"),
    alpha = 0.5,
    linetype = "dashed"
  ) +
  
  # Pretty Stuff
  geom_hline(
    aes(yintercept = mineffconc * kgweight, color = "Minimum Effective Dose"),
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_hline(aes(yintercept = maxrecdose, color = "Maximum Recommended Dose"),
             linetype = "dashed") +
  geom_vline(data = day_marks_df,
             aes(xintercept = xintercept, color = color),
             alpha = 0.5) +
  geom_vline(aes(xintercept = 5 * halflife, color = "Predicted 5xHL Steady State"),
             linetype = "dashed", alpha=0.5) +
  
  labs(
    title = paste(drugname, "Remaining (mg) vs Time (Hours)"),
    y = "Remaining Drug (mg)",
    x = "Hours",
    color = "Legend"
  ) +
  scale_color_manual(
    values = c(
      "Cumulative Remaining Dose" = "black",
      "Minimum Effective Dose" = "limegreen",
      "Maximum Recommended Dose" = "red",
      "Predicted 5xHL Steady State" = "gold",
      "Days" = "darkblue",
      # This value is used directly in geom_vline
      "Initial Dose Decay" = "purple"
    )
  ) +
  scale_x_continuous(
    expand = c(0.005, 0),
    limits = c(0, maxtime),
    breaks = seq(0, maxtime, 6),
    sec.axis = sec_axis( ~ ./24, labels = hourly_labels, breaks = hourly_breaks/24),
    name = "Hours Since First Dose"
  ) +
  scale_y_continuous(
    expand = c(0.0035, 0),
    limits = c(0, maxrecdose),
    breaks = seq(0, maxrecdose, scalestep),
  ) +
  theme_bw() +
  theme(
    legend.justification = "top",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x.top = element_text(
      size = 8,
      margin = margin(t = 0, b = 2),
      angle = 0,
      vjust = 0.5,
      hjust = .5
    ),
    axis.title.x.top = element_text(size = 10, margin = margin(t = 0, b = 0))
  )
