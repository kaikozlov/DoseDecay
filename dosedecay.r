library(tidyverse)

# Constants
# Your constants
lbweight = []

# Your drug's constants
drugname = "[]"
initialdose = [] # in mg
mineffconc = [] # in mg/kg
maxrecdose = [] # in mg
halflife = [] # in hours


# Time Info
maxtime = [] # in hours
start_datetime = "[]" # in mm-dd xx:xx 24 hour time
current_year <- year(now())

# Additional Dose List. Syntax: datetime= "mm-dd hh:mm", dose = mg
additionaldoses <- tibble(
  datetime = c(
    "[]",
  ),
  dose = c([],)
)

# Formatting!
kgweight = lbweight * 0.453592
drugname = str_to_title(drugname)
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
  mutate(color = "Day Marks")

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
    aes(yintercept = 0.5 * kgweight, color = "Minimum Effective Dose"),
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_hline(aes(yintercept = maxrecdose, color = "Maximum Recommended Dose"),
             linetype = "dashed") +
  geom_vline(data = day_marks_df,
             aes(xintercept = xintercept, color = color),
             alpha = 0.5) +
  
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
      "Day Marks" = "darkblue",
      # This value is used directly in geom_vline
      "Initial Dose Decay" = "purple"
    )
  ) +
  scale_x_continuous(
    expand = c(.025, .025),
    limits = c(0, maxtime),
    breaks = seq(0, maxtime, 6),
    sec.axis = sec_axis( ~ . / 24, labels = hourly_labels, breaks = hourly_breaks /
                           24),
    name = "Time of Day"
  ) +  # Adding secondary axis
  
  scale_y_continuous(
    expand = c(.025, .025),
    limits = c(0, maxrecdose),
    breaks = seq(0, maxrecdose, 50)
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
