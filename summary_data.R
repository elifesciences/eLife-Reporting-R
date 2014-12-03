# A script to output summary manuscript data for each month from the output of 
# manusctipt_stats.r

# Define directort to import output of manuscript_data.R from
root_dir  <- paste("~/Dropbox/R Projects/elife-monthly-stats/stats_", Sys.Date(), sep = "")

# Import output of manuscript_stats.r
manuscript_data <- read.csv(paste(root_dir, "/manuscript_data_", Sys.Date(), ".csv", sep = ""))

# Define the ranges for months that we're working with
months <- format(seq(as.Date("2012-05-01"), Sys.Date(), by = "month"), format = "%b %Y")

# Make sure that all the dates are date objects, re-format them to month and
# year only and make them a factor.
manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)] <- lapply(manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)], as.Date, format = "%Y-%m-%d")
manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)] <- lapply(manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)], format, format = "%b %Y")
manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)] <- lapply(manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)], factor, levels = months)


###############################################################################
# Construct a data frame containing summary statistics for initial submissions.
initial_summary_data  <- data.frame(date = months,
                                    total_initial_submissions <- as.vector(by(manuscript_data, manuscript_data$initial_qc_dt, 
                                                                              function(df) nrow(df))),
                                    cumulative_total_initials <- cumsum(total_initial_submissions),
                                    total_initial_decisions <- as.vector(by(manuscript_data, manuscript_data$initial_decision_dt,
                                                                           function(df) nrow(df[df$initial_decision != "Simple Withdraw",]))),
                                    encouraged_initial_submissions <- as.vector(by(manuscript_data, manuscript_data$initial_decision_dt, 
                                                                                   function(df) nrow(df[df$initial_decision == "Encourage Full Submission",]))),
                                    cumulative_encouraged_initials <- cumsum(encouraged_initial_submissions),
                                    percentage_encouraged_initials <- round((encouraged_initial_submissions /total_initial_decisions) * 100, digits = 1),
                                    cumulative_percentage_initials <- round((cumulative_encouraged_initials / cumulative_total_initials) * 100, digits = 1)
)

# Assign column names
colnames(initial_summary_data)  <- c("Date","Initial Subs", "Cumulative Initial Subs","Total Initial Decisions", "Encouraged Initial Subs", "Cumulative Encourage Initial Subs",
                                     "% Encouraged Initial Subs", "Cumulative % Encouraged Initial Subs")


###############################################################################
# Construct a data frame containing summary statistics for full submissions.
full_summary_data  <- data.frame(date = months,
                                 total_full_submissions <- as.vector(by(manuscript_data, manuscript_data$full_qc_dt, 
                                                                        function(df) nrow(df))),
                                 cumulative_full_submissions  <- cumsum(total_full_submissions),
                                 total_decisions <- as.vector(by(manuscript_data, manuscript_data$full_decision_dt,
                                                                 function(df) nrow(df))),
                                 cumulative_total_decisions <- c(0, cumsum(total_decisions[2:length(total_decisions)])),
                                 revise_decisions <- as.vector(by(manuscript_data, manuscript_data$full_decision_dt,
                                                                  function(df) nrow(df[df$full_decision == "Revise Full Submission",]))),
                                 cumulative_revise_decisions <- c(0, cumsum(revise_decisions[2:length(revise_decisions)])),
                                 percentage_revise_decisions <- round((revise_decisions / total_decisions) * 100, digits = 1),
                                 cumulative_percentage_revised <- round((cumulative_revise_decisions / cumulative_total_decisions) * 100, digits = 1)
)
full_summary_data[is.na(full_summary_data)]  <- 0

# Assign column names
colnames(full_summary_data)  <- c("Date","Full Subs", "Cumulative Full Subs", "Total Decisions", "Cumulative Total Decisions", "Revise Decisions", "Cumulative Revise Decisions"
                                  , "% Revised Decisions", "Cumulative % Revised Decisions")


###############################################################################
# Construct a data frame containing summary statistics for published submissions.

publication_summary_data  <- data.frame(date = months,
                                        accepted_submissions <- as.vector(by(manuscript_data, manuscript_data$accept_dt, 
                                                                             function(df) nrow(df))),
                                        number_vor <- as.vector(by(manuscript_data, manuscript_data$vor_dt, 
                                                                   function(df) nrow(df))),                                        
                                        median_acc_to_vor <- as.vector(by(manuscript_data, manuscript_data$accept_dt, 
                                                                          function(df) median(na.exclude(df$accept_to_vor)))),
                                        number_poa  <- as.vector(by(manuscript_data, manuscript_data$poa_dt, 
                                                                    function(df) nrow(df))),
                                        median_acc_to_poa <- as.vector(by(manuscript_data, manuscript_data$accept_dt, 
                                                                          function(df) median(na.exclude(df$accept_to_poa)))),
                                        percentage_poa <- as.vector(by(manuscript_data, manuscript_data$accept_dt, 
                                                                       function(df) round((nrow(df[is.na(df$poa_dt) == FALSE,]) / nrow(df) * 100), digits = 1))),
                                        unique_pubs <- as.vector(by(manuscript_data, manuscript_data$publish_dt,
                                                                    function(df) nrow(df)))
                                        
)
publication_summary_data[is.na(publication_summary_data)] <- 0

# Assign column names
colnames(publication_summary_data) <- c("Date", "Accepted Submissions", "Published VoRs", "Median Days Acceptance to VoR", "Published PoAs", 
                                        "Median Days Acceptance to PoA", "% Accepted Subs PoAd", "Unique Publications")


###############################################################################
# Output the three summary data frames as .csv files in a new directory
dir.create(paste(root_dir, "/summaries", sep= ""))
write.csv(initial_summary_data, file = paste(root_dir, "/summaries/initial_summary_", Sys.Date(), ".csv", sep = ""), row.names = FALSE)
write.csv(full_summary_data, file = paste(root_dir, "/summaries/full_summary_", Sys.Date(), ".csv", sep = ""), row.names = FALSE)
write.csv(publication_summary_data, file = paste(root_dir, "/summaries/publication_summary_", Sys.Date(), ".csv", sep = ""), row.names = FALSE)