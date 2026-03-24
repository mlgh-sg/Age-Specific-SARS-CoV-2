# Load necessary libraries
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(lubridate)
library(grid)

# Assuming you have data frames 'SGMobility2021' and 'SGMobility2022'
# If you need to read them from CSV files, you can use:
SGMobility2021 <- read.csv("C:/Users/user/Desktop/Covid19-Age/Simu_results/Estimated_Source/SGMobility2021.csv")
SGMobility2022 <- read.csv("C:/Users/user/Desktop/Covid19-Age/Simu_results/Estimated_Source/SGMobility2022.csv")

# Combine the two datasets (assuming they have the same column names for the six variables)
SGMobility2021$Date <- as.Date(SGMobility2021$date, format = "%d/%m/%Y")
SGMobility2021$Date <- gsub("^0(\\d{3})", "\\1", SGMobility2021$Date)
SGMobility2021$Date <- gsub("^([0-9]{2})-(.*)", "20\\1-\\2", SGMobility2021$Date)

# Now check the output to confirm the correction
head(SGMobility2021$Date)
SGMobility2022$Date <- as.Date(SGMobility2022$date)

# Filter the data for the date range between "2021-07-01" and "2021-12-29"
SGMobility2021_filtered <- SGMobility2021 %>%
  filter(SGMobility2021$Date >= as.Date("2021-07-01") & SGMobility2021$Date <= as.Date("2021-12-29"))
SGMobility2022_filtered <- SGMobility2022

# Combine the two data sets
SGMobility <- rbind(SGMobility2021_filtered, SGMobility2022_filtered)

# Copy data from SGMobility$date[121:end] to SGMobility$Date[121:end]
SGMobility$Date[183:nrow(SGMobility)] <- SGMobility$date[183:nrow(SGMobility)]

# Select columns 10 to 15 (using column indices for columns 10 to 15)
SGMobility_selected <- SGMobility[, c(10:16)]

# Ensure the 'Date' column is in Date format (if it isn't already)
SGMobility_selected$Date <- as.Date(SGMobility_selected$Date)

# Reshape the data into long format to stack the six columns into a "Variable" column
SGMobility_long <- SGMobility_selected %>%
  pivot_longer(cols = -Date, names_to = "location", values_to = "Value")


# Define custom labels for months with first letter and year below (for January and July)
custom_labels <- function(x) {
  # Extract the first letter of the month (e.g., "J", "F", "M", ...) and year
  months <- toupper(substr(format(x, "%b"), 1, 1))  # Get first letter of the month
  years <- format(x, "%Y")  # Get the year (e.g., "2021", "2022", etc.)
  
  # Initialize a vector to store the labels
  labels <- sapply(x, function(date) {
    month_letter <- toupper(substr(format(date, "%b"), 1, 1))  # First letter of the month
    year <- format(date, "%Y")  # Year
    
    # For January and July, return the month letter with year below it
    if (format(date, "%m") == "01" || format(date, "%m") == "07") {
      return(paste(month_letter, "\n", year))  # Display "J" or "J" with the year below for January and July
    } else {
      return(month_letter)  # For other months, show only the first letter of the month
    }
  })
  return(labels)
}

# Plotting the line graphs for all six columns
ggplot(SGMobility_long, aes(x = Date, y = Value, color = location)) +
  geom_line(linewidth = 0.8) +
  labs(
    x = NULL,
    y = "Mobility Trend",
    color = "Location"
  ) +
  scale_x_date(
    limits = c(as.Date("2021-07-01"), as.Date("2022-10-15")),
    breaks = seq(as.Date("2021-07-01"), as.Date("2022-10-15"), by = "1 month"),
    labels = custom_labels
  ) +
 
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, 20)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
   
    legend.text = element_text(size = 15),

    plot.title = element_text(size = 18, face = "italic"), # 更改为 "italic"
    panel.grid.major = element_line(color = "grey", size = 0.5),
    panel.grid.minor = element_line(color = "grey", size = 0.25),
    
    legend.position = "right",
    legend.box = "vertical",
    legend.spacing.y = unit(0.5, "cm"),
    
    plot.caption = element_text(size = 9, hjust = 1, face = "italic"),
    plot.margin = unit(c(1, 2, 1, 1), "cm")
  )

###### Code for creating contact matrices for various period in 2021, 2022, 2023, mainly aligned with period just before rise, fall or interventions
### 2021-10-13 just before rise

dim(ConMatTran_samples)
MeanMatrix1013 <- matrix(1:49, nrow = 7, ncol = 7)
for (i in 1:A){
  for (j in 1:A){
    MeanMatrix1013[i,j] = median(ConMatTran_samples[,,31+110*((i-1)*7+(j-1))])
  }
}
View(MeanMatrix1013)

### 2021-12-19 just before fall
MeanMatrix1219 <- matrix(1:49, nrow = 7, ncol = 7)
for (i in 1:A){
  for (j in 1:A){
    MeanMatrix1219[i,j] = median(ConMatTran_samples[,,42+110*((i-1)*7+(j-1))])
  }
}
View(MeanMatrix1219)


### 2022-02-09 just before rises
MeanMatrix0209 <- matrix(1:49, nrow = 7, ncol = 7)
for (i in 1:A){
  for (j in 1:A){
    MeanMatrix0209[i,j] = median(ConMatTran_samples[,,48+110*((i-1)*7+(j-1))])
  }
}
View(MeanMatrix0209)

### 2022-07-13 just before rises
MeanMatrix0713 <- matrix(1:49, nrow = 7, ncol = 7)
for (i in 1:A){
  for (j in 1:A){
    MeanMatrix0713[i,j] = median(ConMatTran_samples[,,70+110*((i-1)*7+(j-1))])
  }
}
View(MeanMatrix0713)

### 2022-10-05 just before rises
MeanMatrix1005 <- matrix(1:49, nrow = 7, ncol = 7)
for (i in 1:A){
  for (j in 1:A){
    MeanMatrix1005[i,j] = median(ConMatTran_samples[,,82+110*((i-1)*7+(j-1))])
  }
}
View(MeanMatrix1005)

### 2022-06-29 just before interventions
MeanMatrix0629 <- matrix(1:49, nrow = 7, ncol = 7)
for (i in 1:A){
  for (j in 1:A){
    MeanMatrix0629[i,j] = median(ConMatTran_samples[,,68+110*((i-1)*7+(j-1))])
  }
}
View(MeanMatrix0629)

######################
# Function to generate the heatmap for a given contact matrix and date
generate_heatmap <- function(contact_matrix, date, x_labels = 1:7, y_labels = 1:7) { 
  # Convert the matrix into a data frame suitable for ggplot
  contact_matrix_df <- melt(contact_matrix) 
  colnames(contact_matrix_df) <- c("Index_Age_Group", "Contact_Age_Group", "Contact_Intensity") 
  
  # Convert the age groups to factors to ensure proper discrete scales
  contact_matrix_df$Index_Age_Group <- factor(contact_matrix_df$Index_Age_Group, levels = as.character(x_labels))
  contact_matrix_df$Contact_Age_Group <- factor(contact_matrix_df$Contact_Age_Group, levels = as.character(y_labels))
  
  # Create the heatmap
  p <- ggplot(contact_matrix_df, aes(x = Index_Age_Group, y = Contact_Age_Group, fill = Contact_Intensity)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue", limits = c(0, 4)) +  # Color scale from 0 to 4
    labs(
      title = date  # Title with the date at the top
    ) +
    scale_x_discrete(
      breaks = as.character(x_labels),  # Breaks for x-axis (1-7)
      labels = as.character(x_labels)   # Labels for x-axis (1-7)
    ) +
    scale_y_discrete(
      breaks = as.character(y_labels),  # Breaks for y-axis (1-7)
      labels = as.character(y_labels)   # Labels for y-axis (1-7)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),  # Adjust labels' angle and size for x-axis
      axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),  # Adjust labels' angle and size for y-axis
      axis.title.x = element_blank(),  # Hide x-axis title
      axis.title.y = element_blank(),  # Hide y-axis title
      plot.title = element_text(hjust = 0.5, size = 14),  # Title centered
      axis.ticks = element_line(size = 0.5),  # Add axis ticks
      legend.position = "none",  # Remove legend
      plot.margin = margin(5, 5, 5, 5)  # Margins for spacing
    )
  return(p)
}

# Generate the heatmaps for each date with two-line annotations
heatmap_1 <- generate_heatmap(
  MeanMatrix1013,
  "Week of 13 October 2021\nTime before rise of observed SARS-CoV-2 data"
) +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

heatmap_2 <- generate_heatmap(
  MeanMatrix1219,
  "Week of 19 December 2021\nTime before fall of observed SARS-CoV-2 data"
)+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
heatmap_3 <- generate_heatmap(
  MeanMatrix0209,
  "Week of 9 February 2022\nTime before rise of observed SARS-CoV-2 data"
)+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
heatmap_4 <- generate_heatmap(
  MeanMatrix0629,
  "Week of 29 June 2022\nTime before interventions of vaccine"
)+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
heatmap_5 <- generate_heatmap(
  MeanMatrix0713,
  "Week of 13 July 2022\nTime before rise of observed SARS-CoV-2 data"
)+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
heatmap_6 <- generate_heatmap(
  MeanMatrix1005,
  "Week of 5 October 2022\nTime before rise of observed SARS-CoV-2 data"
)+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

# Create a list of all heatmap plots
heatmap_list <- list(heatmap_1, heatmap_2, heatmap_3, heatmap_4, heatmap_5, heatmap_6)

# Create the grid layout for the heatmaps (3x2 grid)
heatmap_grid <- grid.arrange(
  heatmap_1, heatmap_2, heatmap_3, heatmap_4, heatmap_5, heatmap_6,
  ncol = 2, nrow = 3,
  heights = c(1, 1, 1),  # Equal height for all rows
  bottom = textGrob("Age group of contacting individuals", gp = gpar(fontsize = 13)),  # Label for x-axis of bottom graphs
  left = textGrob("Age group of contacts", rot = 90, gp = gpar(fontsize = 13))  # Label for y-axis of left graphs
)

# Create the legend manually with numbers, ensuring the limits match the data range
legend <- ggplot() + 
  geom_tile(aes(x = 1, y = 1, fill = 1), width = 1, height = 1) + 
  scale_fill_gradient(low = "white", high = "blue", limits = c(0, 4)) + # Ensure limits are properly set
  theme_void() + 
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.direction = "horizontal",  # Make the legend horizontal
    legend.title.align = 0.5,  # Center the title horizontally
    legend.text.align = 0.5   # Center the text labels horizontally
  ) +
  guides(fill = guide_colorbar(
    title = "Estimated Contact Intensity (Posterior median)",  # Title for the legend
    barheight = unit(0.8, "cm"),  # Set legend bar height (reduces size)
    barwidth = unit(10, "cm"),     # Set legend bar width to adjust size (make it wider horizontally)
    ticks = TRUE,                 # Show ticks on the legend
    title.position = "top",       # Position title at the top of the colorbar
    title.hjust = 0.5             # Center the title horizontally
  ))

# Create the grid layout for the heatmaps (3x2 grid)
heatmap_grid <- grid.arrange(
  heatmap_1, heatmap_2, heatmap_3, heatmap_4, heatmap_5, heatmap_6,
  ncol = 2, nrow = 3,
  heights = c(1, 1, 1),  # Equal height for all rows
  bottom = textGrob("Age group of contacting individuals", gp = gpar(fontsize = 13)),  # Label for x-axis of bottom graphs
  left = textGrob("Age group of contacts", rot = 90, gp = gpar(fontsize = 13))  # Label for y-axis of left graphs
)

# Add the legend below the heatmaps (make the legend smaller)
grid.arrange(
  heatmap_grid,  # The heatmap grid
  legend,        # The horizontal legend below the grid
  ncol = 1,      # 1 column (grid and legend)
  heights = c(10, 1)  # Adjust height to make the legend smaller and properly placed below the heatmaps
)
