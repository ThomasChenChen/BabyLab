#Add library 
library(eyetrackingR)
library(readxl)
library(dplyr)

# Set the working directory to the specified folder (set).You need to change your working directory. 
setwd("G:/My Drive/StaffStudies/StaffDA/NayeliExperiment/ProcessData/NewVersion")

# Read the TSV file
SNExp <- read.table("SNExp.tsv", header = TRUE, sep = "\t", quote = "", dec = ".", fill = TRUE, comment.char = "")

# Read the Order1_TargetSides.xlsx
DataOrder <- read_excel("Order1_TargetSides.xlsx") %>%
  rename_with(~gsub(" ", ".", .))
# Assuming your dataset is stored in a variable named `DataOrder`:
names(DataOrder)[names(DataOrder) == "Presented.Media.name"] <- "Presented.Stimulus.name"
#Combine to dataset
SNExp <- SNExp %>%
  left_join(DataOrder %>% select(Presented.Stimulus.name, TargetLeftRight), 
            by = "Presented.Stimulus.name")


# Remove by name (most common), the variables which I don't need 
SelectedSNExpData  <- select(SNExp, Recording.timestamp, Participant.name, Event, Gaze.point.X, Gaze.point.Y, Validity.left, Validity.right, Presented.Stimulus.name, Stimuli, Target, Trial.type, TargetLeftRight)

# Remove participants (keep only two for test)
SelectedSNExpData <- SelectedSNExpData %>%  
filter(Participant.name %in% c("N0896", "N0897"))
  
#This R script fills in missing "Event" values with "VideoStimulusStart" based on the timestamp and the preceding row's "Event".
empty_cells <- which(
  (is.na(SelectedSNExpData$Gaze.point.X) | SelectedSNExpData$Gaze.point.X == "" | 
     SelectedSNExpData$Gaze.point.X == "N/A" | SelectedSNExpData$Gaze.point.X == "NA") &
    (is.na(SelectedSNExpData$Gaze.point.Y) | SelectedSNExpData$Gaze.point.Y == "" | 
       SelectedSNExpData$Gaze.point.Y == "N/A" | SelectedSNExpData$Gaze.point.Y == "NA")
)
for (i in empty_cells) {
  # Ensure we don't go out of bounds
  if (i < nrow(SelectedSNExpData) && (i + 1) <= nrow(SelectedSNExpData)) {
    # Check if the Recording.timestamp of the current row and the next row are similar
    # (either exactly the same or with a difference of 1)
    time_diff <- abs(SelectedSNExpData$Recording.timestamp[i] - SelectedSNExpData$Recording.timestamp[i + 1])
    if (time_diff <= 1) {
      # Check if the Event of the current row is "VideoStimulusStart"
      if (SelectedSNExpData$Event[i] == "VideoStimulusStart") {
        # Set the Event of the next row to "VideoStimulusStart"
        SelectedSNExpData$Event[i + 1] <- "VideoStimulusStart"
      }
    }
  }
}
# Print the modified DataFrame to verify the changes
print(SelectedSNExpData)

#Add Trial number
counter_dict <- list()
pb <- txtProgressBar(min = 0, max = nrow(SelectedSNExpData ), style = 3)

for(i in 1:nrow(SelectedSNExpData )) {
  setTxtProgressBar(pb, i)
  current_value <- SelectedSNExpData $Presented.Stimulus.name[i]
  
  if(!is.na(current_value) && current_value != "") {
    base_name <- sub("\\d+$", "", current_value)
    
    if(is.null(counter_dict[[current_value]])) {
      if(is.null(counter_dict[[base_name]])) {
        counter_dict[[base_name]] <- 1
      }
      counter_dict[[current_value]] <- counter_dict[[base_name]]
    }
    
    SelectedSNExpData $Presented.Stimulus.name[i] <- paste0(base_name, counter_dict[[current_value]])
    
    if(i < nrow(SelectedSNExpData ) && 
       SelectedSNExpData $Presented.Stimulus.name[i+1] != current_value) {
      counter_dict[[base_name]] <- counter_dict[[base_name]] + 1
    }
  }
}
close(pb)
SelectedSNExpData1 <- SelectedSNExpData
############################################################################  
# # Find the row number of the first occurrence of "Blap_Bear_Nov_C"
# first_occurrence_row <- which(SelectedSNExpData$Presented.Stimulus.name == "Blap_Bear_Nov_C")[1]
# 
# # Print the row number
# cat("The first occurrence of 'Blap_Bear_Nov_C' is in row:", first_occurrence_row, "\n")
# 
# # Print the entire row where "Blap_Bear_Nov_C" first appears
# first_occurrence_data <- SelectedSNExpData[first_occurrence_row, ]
# print(first_occurrence_data)


############################################################################
#Remove additional trails in the later steps
#Keep match values
# List of allowed values to keep
# the problem is from here
allowed_values <- c(
  "A_Bee_Bosa_Nov_C", "A_Car_NObB_Fam_L", "A_Fish_NAniD_Fam_L",
  "A_Gip_Ball_Nov_W", "A_NAnA_Dog_Fam_W", "A_Toothbrush_Sibu_Nov_C",
  "A_NObjC_Hat_Fam_W", "A_Cusk_Horse_Nov_L", "Blap_Bear_Nov_C",
  "Lion_NAniH_Fam_W", "NAniE_Elephant_Fam_L", "Nappy_NObiG_Fam_L",
  "NObiF_Bed_Fam_C", "Pig_Koba_Nov_W", "Roga_Sock_Nov_L",
  "Shoe_Hux_Nov_WA"
)
# Modified filtering logic with progress bar
pb <- txtProgressBar(min = 0, max = nrow(SelectedSNExpData1), style = 3)
keep_rows <- logical(nrow(SelectedSNExpData1))

for (i in 1:nrow(SelectedSNExpData1)) {
  name <- SelectedSNExpData1$Presented.Stimulus.name[i]
  keep_rows[i] <- any(sapply(allowed_values, function(allowed) {
    grepl(paste0("^", allowed, "[0-9]*$"), name)
  }))
  setTxtProgressBar(pb, i)
}
close(pb)
# Apply the filter
SelectedSNExpData1 <- SelectedSNExpData1[keep_rows, ]
# Check what was kept
cat("Rows kept:", nrow(SelectedSNExpData1), "\n")
# Replace the original dataset
SNExp1 <- SelectedSNExpData1


#Add AOI
# Create the AOI data frame
left_aoi <- data.frame(L = 155, R = 880, T = 110, B = 970)
right_aoi <- data.frame(L = 1035, R = 1760, T = 110, B = 970)
SNExp1 <- add_aoi(SNExp1, left_aoi, "Gaze.point.X", "Gaze.point.Y", "LeftImage")
SNExp1 <- add_aoi(SNExp1, right_aoi, "Gaze.point.X", "Gaze.point.Y", "RightImage")
SNExp1$AOI <- ifelse(is.na(SNExp1$RightImage) | is.na(SNExp1$LeftImage), 
                    NA, 
                    ifelse(SNExp1$RightImage & !SNExp1$LeftImage, 
                           ifelse(SNExp1$TargetLeftRight == "Right", "AOITarget", "AOIDistractor"),
                           ifelse(!SNExp1$RightImage & SNExp1$LeftImage, 
                                  ifelse(SNExp1$TargetLeftRight == "Left", "AOITarget", "AOIDistractor"),
                                  NA)))

# Create the AOI columns
SNExp1$AOITarget <- SNExp1$AOI == "AOITarget"
SNExp1$AOIDistractor <- SNExp1$AOI == "AOIDistractor"

#Create the TrackLoss
SNExp1 <- SNExp1 %>%
  mutate(TrackLoss = if_else(Validity.left == "Invalid" | Validity.right == "Invalid", TRUE, FALSE))

############################################################################
#load data
dataset <- make_eyetrackingr_data(SNExp1, 
                                  participant_column = "Participant.name",
                                  trial_column = "Presented.Stimulus.name",
                                  time_column = "Recording.timestamp",
                                  trackloss_column = "TrackLoss",
                                  aoi_columns = c('AOITarget','AOIDistractor'),
                                  treat_non_aoi_looks_as_missing = TRUE
)

dataset <- subset_by_window(dataset, window_start_msg = "VideoStimulusStart", msg_col = "Event", rezero= TRUE)

dataset_clean <- clean_by_trackloss(dataset, 
                                    participant_prop_thresh = 1, trial_prop_thresh = .25, 
                                    window_start_time = 900, window_end_time = 4900)

# zoom in on response window
word_window <- subset_by_window(dataset_clean, rezero = FALSE,
                                window_start_time = 900, window_end_time = 4900)

word_time <- make_time_sequence_data(word_window, time_bin_size = 100, 
                                     predictor_columns = "AOITarget", aois = c("TRUE"))
plot(word_time, predictor_column = "AOITarget")