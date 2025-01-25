#Add library 
library(eyetrackingR)
library(readxl)
library(dplyr)

# Set the working directory to the specified folder (set). 
setwd("G:/My Drive/StaffExperiment/NayeliExperiment/ProcessData/NewVersion")

# Read the TSV file
SNExp <- read.table("SNExp.tsv", header = TRUE, sep = "\t", quote = "", dec = ".", fill = TRUE, comment.char = "")

# Read the Order1_TargetSides.xlsx
DataOrder <- read_excel("Order1_TargetSides.xlsx") %>%
  rename_with(~gsub(" ", ".", .))
# Assuming your dataset is stored in a variable named `DataOrder`:
names(DataOrder)[names(DataOrder) == "Presented.Media.name"] <- "Presented.Stimulus.name"

# Remove by name (most common), the variables which I don't need 
SNExp <- select(SNExp, Recording.timestamp, Participant.name, Event, Gaze.point.X, Gaze.point.Y, Validity.left, Validity.right, Presented.Stimulus.name, Stimuli, Target, Trial.type)
# Remove participants (keep only two for test)
SNExp <- SNExp %>%
  filter(Participant.name %in% c("N0896", "N0897"))
#Keep match values
# List of allowed values
allowed_values <- c(
  "A_Bee_Bosa_Nov_C", "A_Car_NObB_Fam_L", "A_Fish_NAniD_Fam_L",
  "A_Gip_Ball_Nov_W", "A_NAnA_Dog_Fam_W", "A_Toothbrush_Sibu_Nov_C",
  "A_NObjC_Hat_Fam_W", "A_Cusk_Horse_Nov_L", "Blap_Bear_Nov_C",
  "Lion_NAniH_Fam_W", "NAniE_Elephant_Fam_L", "Nappy_NObiG_Fam_L",
  "NObiF_Bed_Fam_C", "Pig_Koba_Nov_W", "Roga_Sock_Nov_L",
  "Shoe_Hux_Nov_WA"
)

# Filter rows
SNExp <- SNExp[SNExp$Presented.Stimulus.name %in% allowed_values,]

#Combine to dataset
SNExp <- SNExp %>%
  left_join(DataOrder %>% select(Presented.Stimulus.name, TargetLeftRight), 
            by = "Presented.Stimulus.name")

#Add AOI
# Create the AOI data frame
left_aoi <- data.frame(L = 155, R = 880, T = 110, B = 970)
right_aoi <- data.frame(L = 1035, R = 1760, T = 110, B = 970)
SNExp <- add_aoi(SNExp, left_aoi, "Gaze.point.X", "Gaze.point.Y", "LeftImage")
SNExp <- add_aoi(SNExp, right_aoi, "Gaze.point.X", "Gaze.point.Y", "RightImage")
SNExp$AOI <- ifelse(is.na(SNExp$RightImage) | is.na(SNExp$LeftImage), 
                          NA, 
                          ifelse(SNExp$RightImage & !SNExp$LeftImage, 
                                 ifelse(SNExp$TargetLeftRight == "Right", "Target", "Distractor"),
                                 ifelse(!SNExp$RightImage & SNExp$LeftImage, 
                                        ifelse(SNExp$TargetLeftRight == "Left", "Target", "Distractor"),
                                        NA)))

# Create the AOI columns
SNExp$AOITarget <- SNExp$AOI == "Target"
SNExp$AOIDistractor <- SNExp$AOI == "Distractor"

#Create the TrackLoss
SNExp <- SNExp %>%
  mutate(TrackLoss = if_else(Validity.left == "Invalid" | Validity.right == "Invalid", TRUE, FALSE))

#Add Trial number
SNExp_new <- SNExp
counter_dict <- list()
pb <- txtProgressBar(min = 0, max = nrow(SNExp_new), style = 3)

for(i in 1:nrow(SNExp_new)) {
  setTxtProgressBar(pb, i)
  current_value <- SNExp_new$Presented.Stimulus.name[i]
  
  if(!is.na(current_value) && current_value != "") {
    base_name <- sub("\\d+$", "", current_value)
    
    if(is.null(counter_dict[[current_value]])) {
      if(is.null(counter_dict[[base_name]])) {
        counter_dict[[base_name]] <- 1
      }
      counter_dict[[current_value]] <- counter_dict[[base_name]]
    }
    
    SNExp_new$Presented.Stimulus.name[i] <- paste0(base_name, counter_dict[[current_value]])
    
    if(i < nrow(SNExp_new) && 
       SNExp_new$Presented.Stimulus.name[i+1] != current_value) {
      counter_dict[[base_name]] <- counter_dict[[base_name]] + 1
    }
  }
}
close(pb)

#load data
dataset <- make_eyetrackingr_data(SNExp, 
                                  participant_column = "Participant.name",
                                  trial_column = "Presented.Stimulus.name",
                                  time_column = "Recording.timestamp",
                                  trackloss_column = "TrackLoss",
                                  aoi_columns = c('AOITarget','AOIDistractor'),
                                  treat_non_aoi_looks_as_missing = TRUE
)
