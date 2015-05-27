# A script to generate manuscript performance stats from James's SQL output and
# output a csv file with stats appended.

# Import data
raw_data <- read.csv("~/Dropbox/paper_history2015_05_15.csv")
editors <- read.csv("~/Dropbox/all_editors.csv")

# Find and record the date of acceptance and accepted revision for each manuscript
raw_data$accept_dt <- ""
raw_data$accept_stage <- ""
raw_data[raw_data$full_decision == "Accept Full Submission",]$accept_dt <- as.character(raw_data[raw_data$full_decision == "Accept Full Submission",]$full_decision_dt)
raw_data[raw_data$full_decision == "Accept Full Submission",]$accept_stage <- "full"
raw_data[raw_data$rev1_decision == "Accept Full Submission",]$accept_dt <- as.character(raw_data[raw_data$rev1_decision == "Accept Full Submission",]$rev1_decision_dt)
raw_data[raw_data$rev1_decision == "Accept Full Submission",]$accept_stage <- "r1"
raw_data[raw_data$rev2_decision == "Accept Full Submission",]$accept_dt <- as.character(raw_data[raw_data$rev2_decision == "Accept Full Submission",]$rev2_decision_dt)
raw_data[raw_data$rev2_decision == "Accept Full Submission",]$accept_stage <- "r2"
raw_data[raw_data$rev3_decision == "Accept Full Submission",]$accept_dt <- as.character(raw_data[raw_data$rev3_decision == "Accept Full Submission",]$rev3_decision_dt)
raw_data[raw_data$rev3_decision == "Accept Full Submission",]$accept_stage <- "r3"
raw_data[raw_data$rev4_decision == "Accept Full Submission",]$accept_dt <- as.character(raw_data[raw_data$rev4_decision == "Accept Full Submission",]$rev4_decision_dt)
raw_data[raw_data$rev4_decision == "Accept Full Submission",]$accept_stage <- "r4"

# Convert all the dates in raw_data to date values
raw_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26)] <- lapply(raw_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26)], as.Date, format = "%Y-%m-%d")

# Determine the publication date for each paper i.e. the date on which the paper first appeared online
raw_data$publish_dt <- ""
raw_data[is.na(raw_data$vor_dt) == TRUE & is.na(raw_data$poa_dt) == FALSE,]$publish_dt <- as.character(raw_data[is.na(raw_data$vor_dt) == TRUE & is.na(raw_data$poa_dt) == FALSE,]$poa_dt)
raw_data[is.na(raw_data$vor_dt) == FALSE & is.na(raw_data$poa_dt) == TRUE,]$publish_dt <- as.character(raw_data[is.na(raw_data$vor_dt) == FALSE & is.na(raw_data$poa_dt) == TRUE,]$vor_dt)
raw_data[is.na(raw_data$vor_dt) == FALSE & is.na(raw_data$poa_dt) == FALSE,]$publish_dt <- as.character(raw_data[is.na(raw_data$vor_dt) == FALSE & is.na(raw_data$poa_dt) == FALSE,]$poa_dt)

# Substitute editor p_id for surnames
get_editor <- function(x, editor_list) {
  if (!is.na(x) & x %in% editor_list$p_id) {
    return(paste(as.character(editor_list[editor_list$p_id == x,]$first_nm), as.character(editor_list[editor_list$p_id == x,]$last_nm)))
  } else {
    return (NA)
  }
}

raw_data$senior_editor <- sapply(raw_data$senior_editor, get_editor, editor_list = editors)
raw_data$reviewing_editor <- sapply(raw_data$reviewing_editor, get_editor, editor_list = editors)

# Flag appealed submissions
raw_data$appeal_flag <- ""
raw_data[raw_data$full_decision == "Reject Full Submission" & 
           is.na(raw_data$rev1_qc_dt) == FALSE,]$appeal_flag  <- "appealed"
raw_data[raw_data$rev1_decision == "Reject Full Submission" & 
           is.na(raw_data$rev2_qc_dt) == FALSE,]$appeal_flag  <- "appealed"

# Calculate initial submission received to initial decision
raw_data$isub_rcvd_to_isub_dec <- ""
raw_data[raw_data$initial_decision != "Simple Withdraw",]$isub_rcvd_to_isub_dec <- raw_data[raw_data$initial_decision != "Simple Withdraw",]$initial_decision_dt -
  raw_data[raw_data$initial_decision != "Simple Withdraw",]$initial_qc_dt

# Calculate days initial submission received to acceptance
raw_data$isub_rcvd_to_accept <- as.numeric(raw_data$accept_dt - raw_data$initial_qc_dt)

# Calculate full submission received to full submission first decision
raw_data$fsub_rcvd_to_first_dec <- ""
raw_data[raw_data$full_decision != "Simple Withdraw",]$fsub_rcvd_to_first_dec <- raw_data[raw_data$full_decision != "Simple Withdraw",]$full_decision_dt - 
  raw_data[raw_data$full_decision != "Simple Withdraw",]$full_qc_dt

# Calculate days full submission received to acceptance
raw_data$fsub_dec_to_accept <- as.numeric(raw_data$accept_dt - raw_data$full_decision_dt)

# Calculate days spent revising for papers accepted at r1
raw_data$revise_time <- ""
raw_data[raw_data$accept_stage == "r1" & raw_data$appeal_flag != "appealed",]$revise_time  <- 
  as.numeric(raw_data[raw_data$accept_stage == "r1" & raw_data$appeal_flag != "appealed",]$rev1_qc_dt - 
               raw_data[raw_data$accept_stage == "r1" & raw_data$appeal_flag != "appealed",]$full_decision_dt)

# Calculate days spent revising for papers accepted at r2
raw_data[raw_data$accept_stage == "r2" & raw_data$appeal_flag != "appealed",]$revise_time  <- 
  as.numeric(raw_data[raw_data$accept_stage == "r2" & raw_data$appeal_flag != "appealed",]$rev1_qc_dt - 
               raw_data[raw_data$accept_stage == "r2" & raw_data$appeal_flag != "appealed",]$full_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r2" & raw_data$appeal_flag != "appealed",]$rev2_qc_dt - 
               raw_data[raw_data$accept_stage == "r2" & raw_data$appeal_flag != "appealed",]$rev1_decision_dt)

# Calculate days spent revising for papers accepted at r3
raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag != "appealed",]$revise_time  <- 
  as.numeric(raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag != "appealed",]$rev1_qc_dt - 
               raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag != "appealed",]$full_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag != "appealed",]$rev2_qc_dt - 
               raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag != "appealed",]$rev1_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag != "appealed",]$rev3_qc_dt - 
               raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag != "appealed",]$rev2_decision_dt)

# Calculate days spent revising for papers accepted at r4
raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$revise_time  <- 
  as.numeric(raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$rev1_qc_dt - 
               raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$full_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$rev2_qc_dt - 
               raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$rev1_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$rev3_qc_dt - 
               raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$rev2_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$rev4_qc_dt - 
               raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag != "appealed",]$rev3_decision_dt)

# 0 days spent revising for appeals accepted at r1
raw_data[raw_data$accept_stage == "r1" & raw_data$appeal_flag == "appealed",]$revise_time  <- 0

# Calculate days spent revising for appeals accepted at r2
raw_data[raw_data$accept_stage == "r2" & raw_data$appeal_flag == "appealed",]$revise_time  <- 
  as.numeric(raw_data[raw_data$accept_stage == "r2" & raw_data$appeal_flag == "appealed",]$rev2_qc_dt - 
               raw_data[raw_data$accept_stage == "r2" & raw_data$appeal_flag == "appealed",]$rev1_decision_dt)

# Calculate days spent revising for appeals accepted at r3
raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag == "appealed",]$revise_time  <- 
  as.numeric(raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag == "appealed",]$rev2_qc_dt - 
               raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag == "appealed",]$rev1_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag == "appealed",]$rev3_qc_dt - 
               raw_data[raw_data$accept_stage == "r3" & raw_data$appeal_flag == "appealed",]$rev2_decision_dt)

# Calculate days spent revising for appeals accepted at r4
raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag == "appealed",]$revise_time  <- 
  as.numeric(raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag == "appealed",]$rev2_qc_dt - 
               raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag == "appealed",]$rev1_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag == "appealed",]$rev3_qc_dt - 
               raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag == "appealed",]$rev2_decision_dt) + 
  as.numeric(raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag == "appealed",]$rev4_qc_dt - 
               raw_data[raw_data$accept_stage == "r4" & raw_data$appeal_flag == "appealed",]$rev3_decision_dt)

# Calculate days accepted to PoA publication
raw_data$accept_to_poa  <- as.numeric(raw_data$poa_dt - raw_data$accept_dt)

# Calculate days accepted to VoR publication
raw_data$accept_to_vor <- as.numeric(raw_data$vor_dt - raw_data$accept_dt)

# Output the modified raw_data to a new .csv file and directory
dir.create(paste("~/Dropbox/R Projects/elife-monthly-stats/stats_", Sys.Date(), sep = ""))
write.csv(raw_data, file = paste("~/Dropbox/R Projects/elife-monthly-stats/stats_", Sys.Date(),"/manuscript_data_", Sys.Date(), ".csv", sep = ""), row.names = FALSE)
