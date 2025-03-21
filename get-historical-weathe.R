# Preamble ----------------------------------------------------------------
# Eric Parajon (using code modified modified from John D. Martin III)

# This code chunk loads (and installs required packages) and sets up the underlying url call, and parameters for the API call

if (!require(httr2)) install.packages("httr2",dependencies = TRUE) # Checking if httr package is installed, and installing it (and required dependencies) if not
library(httr2) # Loads the httr2 package; used for working with web APIs

if (!require(jsonlite)) install.packages("jsonlite",dependencies = TRUE)  # Checking if jsonlite package is installed, and installing it (and required dependencies) if not
library(jsonlite) # Loads the jsonlite; used to convert JSON data to R objects

base_url <- "https://archive-api.open-meteo.com/v1/archive" # Open-Meteo API url 
params <- list( # Setting up parameters for the API query
  latitude = 40.7128, # Latitude and longitude of New York City, NY (City Hall Park)
  longitude = -74.0060, 
  start_date = "2014-01-02", # Pulling data starting on January 2nd 2014 
  end_date = "2024-12-31", # Pulling data ending on December 31st 2024.
  temperature_unit = "fahrenheit", # Accessing Fahrenheit temperatures. 
  hourly = "temperature_2m,precipitation,relative_humidity_2m,dew_point_2m", # Selecting relevant hourly weather variables. See API documentation for full list of options
  timezone = "America/New_York" # Setting timezone 
)

# API Call ----------------------------------------------------------------
# This code chunk pulls data from the Open-Meteo API (application programming interface)
# It additionally includes error checking functionality to determine if the response is valid

response <- tryCatch( # Wrapping the API query in a tryCatch block to capture any errors that occur during the request
  request(base_url) |> # Requesting at the given URL
    req_url_query(!!!params) |>   # Adds query parameters; more specifically  the Splice operator (!!!) is used to unpack a list of values (in this case taken from the above params) into individual arguments to use in the function. Without it the list would be passed as a single argument leading to an error
    req_error(is_error = ~ { # This code checks if the API request was successful (a status_code of 200) and returns an error if it was not, and stops the code running (~ defines the start of the function)
      if (.x$status_code != 200) # Checks the current value being processed (.x) to determine if status differs from 200  
        stop("Failed to retrieve data. Status code:", .x$status_code) # error code reporting
      FALSE  # Ensures successful responses don't trigger an error
    }) |>  
    req_perform(),   # Sends request to API to fetch results back to R
  error = function(e) { # Creating a function to deal with messages from the tryCatch: This defines what happens if an error (such as an incorrect url, or a network issue) occurs
    stop("Invalid URL or network error:", e$message) # This line helps parse and report the error message (from e the error object) and stops the code from running if there is an error 
  }
)

# Checking data from API call for debugging purposes (first 5 variables)
head(resp_body_json(response), 5) 

# Returning content as UTF-8 string

json_data <- resp_body_string(response) 

# checking output of small portion of json_data for debugging purposes (first 100 characters)
print(substr(json_data, 1, 100))

# This code flattens the nested JSON data returned from the API into an R object which is easier to work with
weather_data <- fromJSON(json_data, flatten = TRUE)

# This code chunk checks if the API response was valid and returned the expected hourly variables
if (!"hourly" %in% names(weather_data)) {
  stop("Unexpected data format: 'hourly' field not found in the API response.")
}

# Data Frame Creation ----------------------------------------------------------------
# This code chunk parses the JSON response and creates a data frame from the hourly object of the underlying weather_data

# This code pulls the hourly data from the weather_data object and assigns it to the hourly object for further use below
hourly <- weather_data$hourly

# Inspecting hourly object
str(hourly)

# Creating a dataframe of relevant variables
df <- data.frame(
  time = as.POSIXct(hourly$time, format = "%Y-%m-%dT%H:%M", tz = "America/New_York"), # formats the time variable to interpret time like 2024-01-02T00:00
  temperature = hourly$temperature_2m, # temperatue measured in F
  precipitation = hourly$precipitation, # precipitation measured in mm
  relative_humidity = hourly$relative_humidity_2m, # relative humidity %
  dew_point = hourly$dew_point_2m # dew point measured in F
)

# Inspecting the newly generated dataframe
str(df)

# Visualization ----------------------------------------------------------------
# This code chunk visualizes key weather parameters over time using base R

png(filename = "weather_plots.png", width = 1400, height = 1400) # Line sets the download parameters for the weather_plots 

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1)) # This line specifies the organization of the following plots first arranging them on a 2x2 grid, then setting the margins (bottom, left, top, right)

# Plot Panel 1: Temperature over Time
plot(df$time, df$temperature, type = "l", col = "blue", # Creating a lineplot (type="l") of temperature over time
     main = "Temperature over Time", # title 
     xlab = "Time", ylab = "Temperature (°F)") # axis labels

# Plot Panel 2: Precipitation over Time
plot(df$time, df$precipitation, type = "l", col = "blue", # Again a lineplot this time of precipitation
     main = "Precipitation (rain + snow) over Time", # title
     xlab = "Time", ylab = "Precipitation (mm)") # axis labels

# Plot Panel 3: Relative Humidity over Time
plot(df$time, df$relative_humidity, type = "l", col = "blue",
     main = "Relative Humidity over Time", # title
     xlab = "Time", ylab = "Relative Humidity (%)") # axis labels

# Plot Panel 4: Dew Point over Time
plot(df$time, df$dew_point, type = "l", col = "blue",
     main = "Dew Point over Time", # title
     xlab = "Time", ylab = "Dew Point (°F)") # axis labels

# Add a title to all four panels (the title is outside the panels). Mtext is used to add text to the outer margins of the plot, the remaining arguements control placements and aesthetics
mtext("Weather Data Overview", side = 3, line = - 2, outer = TRUE,cex = 2.2, col = "black", font = 2)  # side determines horizontal placement, line vertical, outer makes it outside the plotting area, cex controls the font size, col sets the text color to black, font sets the font to bold

par(fig = c(0.5, 1, 0.5, 1), new = TRUE) # This defines a new plotting region in the top-right area of the second panel so I can add the source and author information, by using new=TRUE the existing four weather plots are kept and the new text can be added on top
mtext("Source: Open-Meteo Historical Weather API | Author: Eric Parajon (using code modified from John D. Martin III)", 
      side = 3, line = 2, cex = 0.8, col = "black", adj = 1) # adj = 1 for right alignment

dev.off() # Closes the plot to save it

# Saving data and completing the code----------------------------------------------------------------
# This code chunk saves the processed data as an RDS file.

saveRDS(df, file = "historical_weather_data.rds")

# This chunk of code ensures the correct output files are in place 

# First, I create a vector of default values NAs to store name(s) of missing files, this is two elements long to account for the two required files. It's initialized with NA values which are then replaced with the filenames if the files are missing

missing <- c(NA, NA)

# Check if the weather_plots.png file exists (evaluates to TRUE) if it does not write the first element of missing with the name of the file 
if (file.exists("weather_plots.png")==FALSE) {
  missing[1] <- "weather_plots.png"
} 

# Check if the historical_weather_data.rds file exists and if it doesn't add the name of the file to missing
if (file.exists("historical_weather_data.rds")==FALSE) {
  missing[2] <- "historical_weather_data.rds"
}

# Check if both the above files are present and if so output the completion message; if not indicate which file is missing
if (sum(!is.na(missing)) == 0) { # This line ensures both values in the vector are NA (by counting the number of non NA values) before printing the completion message 
  cat("Data download, visualization, and saving complete.\n") # Using calls to concatenate to print output
} else {
  # List missing file(s) if required
  cat("The required file(s) are missing:\n")
  cat(na.omit(missing), sep = "\n") # na.omit(missing) removes the NA values from the missing vector, so that only the missing file names are printed
}

