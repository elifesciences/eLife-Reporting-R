library(ggplot2)

sample_dates  <- format(seq(as.Date("2014-05-01"), as.Date("2015-04-30"), by = "month"), format = "%b %Y")
poa_sample_dates <- format(seq(as.Date("2014-06-01"), as.Date("2015-04-30"), by = "month"), format = "%b %Y")

# Define directort to import output of manuscript_data.R from
root_dir  <- paste("~/Dropbox/R Projects/elife-monthly-stats/stats_", Sys.Date(), sep = "")

# Import the data - manuscript_data.R and summary_data.R must be run beforehand.
manuscript_data  <- read.csv(paste(root_dir, "/manuscript_data_", Sys.Date(), ".csv", sep = ""))
initial_summary_data  <- read.csv(paste(root_dir, "/summaries/initial_summary_", Sys.Date(), ".csv", sep = ""))
full_summary_data  <- read.csv(paste(root_dir, "/summaries/full_summary_", Sys.Date(), ".csv", sep = ""))
publication_summary_data  <- read.csv(paste(root_dir,"/summaries/publication_summary_", Sys.Date(), ".csv", sep = ""))

manuscript_data <- subset(manuscript_data, manuscript_data$initial_decision != "Simple Withdraw" &
                            manuscript_data$full_decision != "Simple Withdraw" &
                            manuscript_data$rev1_decision != "Simple Withdraw")

# Define the ranges for months that we're working with
months <- format(seq(as.Date("2012-05-01"), Sys.Date(), by = "month"), format = "%b %Y")

# Make sure that all the dates are date objects, re-format them to month and
# year only and make them a factor.
manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)] <- 
  lapply(manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)], as.Date, format = "%Y-%m-%d")
manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)] <- 
  lapply(manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)], format, format = "%b %Y")
manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)] <- 
  lapply(manuscript_data[c(5,7,9,11,12,14,15,17,18,20,21,23,24,25,26,28)], factor, levels = months)


###############################################################################
# Generate the data for the boxplots: Graphs A - F and the Acceptance > PoA chart

# Produce data for Graph A - Initial Submission Received > Initial Submission Decision, excludes submissions with no initial submission
graphA_data  <- data.frame(ms = manuscript_data[manuscript_data$initial_decision_dt %in% sample_dates & is.na(manuscript_data$initial_qc_dt) == FALSE,]$ms,
                           date = factor(manuscript_data[manuscript_data$initial_decision_dt %in% sample_dates & is.na(manuscript_data$initial_decision_dt) == FALSE,]$initial_decision_dt, levels = sample_dates),
                           days = manuscript_data[manuscript_data$initial_decision_dt %in% sample_dates & is.na(manuscript_data$initial_qc_dt) == FALSE,]$isub_rcvd_to_isub_dec
)

# Produce data for Graph B - Full Submission Received > Full Submission First Decision
graphB_data  <- data.frame(ms = manuscript_data[manuscript_data$full_decision_dt %in% sample_dates,]$ms,
                           date = factor(manuscript_data[manuscript_data$full_decision_dt %in% sample_dates,]$full_decision_dt, levels = sample_dates),
                           days = manuscript_data[manuscript_data$full_decision_dt %in% sample_dates,]$fsub_rcvd_to_first_dec
)

# Produce data for Graph C - Accept > Publication of Version of Record
graphC_data <- data.frame(ms = manuscript_data[manuscript_data$vor_dt %in% sample_dates,]$ms,
                          date = factor(manuscript_data[manuscript_data$vor_dt %in% sample_dates,]$vor_dt, levels = sample_dates),
                          days = manuscript_data[manuscript_data$vor_dt %in% sample_dates,]$accept_to_vor
)

# Produce data for Graph C - Acceptance > poa boxplot
graphD_data <- data.frame(ms = manuscript_data[manuscript_data$poa_dt %in% poa_sample_dates,]$ms,
                          date = factor(manuscript_data[manuscript_data$poa_dt %in% poa_sample_dates,]$poa_dt, levels = poa_sample_dates),
                          days = manuscript_data[manuscript_data$poa_dt %in% poa_sample_dates,]$accept_to_poa)


# Produce data for Graph E - Initial Submission > Acceptance, excludes submissions with no initial submission
graphE_data  <- data.frame(ms = manuscript_data[manuscript_data$accept_dt %in% sample_dates & is.na(manuscript_data$initial_qc_dt) == FALSE,]$ms,
                           date = factor(manuscript_data[manuscript_data$accept_dt %in% sample_dates & is.na(manuscript_data$initial_qc_dt) == FALSE,]$accept_dt, levels = sample_dates),
                           days = manuscript_data[manuscript_data$accept_dt %in% sample_dates & is.na(manuscript_data$initial_qc_dt) == FALSE,]$isub_rcvd_to_accept
)

# Produce data for Graph F - Time Spent Revising Article
graphF_data  <- data.frame(ms = manuscript_data[manuscript_data$accept_dt %in% sample_dates,]$ms,
                           date = factor(manuscript_data[manuscript_data$accept_dt %in% sample_dates,]$accept_dt, levels = sample_dates),
                           days = manuscript_data[manuscript_data$accept_dt %in% sample_dates,]$revise_time
)
graphF_data[is.na(graphF_data)] <- 0



###############################################################################
# Generate the data for the line graphs: Graphs G & H

# Produce data for Graph G - Submission Volume
graphG_data  <- data.frame(date = rep(factor(sample_dates, levels = sample_dates), time = 2),
                           subs = c(initial_summary_data[initial_summary_data$Date %in% sample_dates,]$Initial.Subs,
                                    full_summary_data[full_summary_data$Date %in% sample_dates,]$Full.Subs),
                           Type = c(rep("Initial Submissions", times = length(sample_dates)), rep("Full Submissions", times = length(sample_dates)))
)

# Produce data for Graph H - Volume of Acceptances and Publications
graphH_data  <- data.frame(date = rep(factor(sample_dates, levels = sample_dates), time = 2),
                           subs = c(publication_summary_data[publication_summary_data$Date %in% sample_dates,]$Accepted.Submissions,
                                    publication_summary_data[publication_summary_data$Date %in% sample_dates,]$Unique.Publications),
                           Type = c(rep("Accepted", times = length(sample_dates)), rep("Published", times = length(sample_dates)))
)


###############################################################################
# Generate the data for the line graphs: Graphs I & J

# Produce data for Graph I - Initial Submission Encouragement Rate 
graphI_data <- data.frame(date = factor(sample_dates, levels = sample_dates),
                          monthly = initial_summary_data[initial_summary_data$Date %in% sample_dates,]$X..Encouraged.Initial.Subs,
                          cumulative = initial_summary_data[initial_summary_data$Date %in% sample_dates,]$Cumulative...Encouraged.Initial.Subs,
                          n = initial_summary_data[initial_summary_data$Date %in% sample_dates,]$Total.Initial.Decisions
                          
)

# Produce data for Graph J - Full Submissions Sent for Revision
graphJ_data <- data.frame(date = factor(sample_dates, levels = sample_dates),
                          monthly = full_summary_data[full_summary_data$Date %in% sample_dates,]$X..Revised.Decisions,
                          cumulative = full_summary_data[full_summary_data$Date %in% sample_dates,]$Cumulative...Revised.Decisions,
                          n = full_summary_data[full_summary_data$Date %in% sample_dates,]$Total.Decisions
)


###############################################################################
# Graphing functions

# A function to generate the boxplots Graphs A - F
boxplot <- function(plot_data, title, x_label, y_label, n_labels, y_ticks, y_limit) {
  plot <- ggplot(plot_data, aes_string(x = "date", y = "days")) +
    geom_boxplot() +
    theme_bw() +
    scale_y_continuous(limits=c(0, as.numeric(y_limit + y_ticks)), breaks=seq(0, as.numeric(y_limit + y_ticks), by = y_ticks)) +
    labs(x = x_label, y = y_label) +
    ggtitle(title) +
    theme(axis.title.x = element_text(size = 20, vjust = -0.5, face = "bold"), 
          axis.title.y = element_text(size = 20, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 14, angle = 90), 
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 24, face = "bold"))
  
  if (n_labels == TRUE) {
    labels <- as.vector(by(plot_data, plot_data$date, function(df) nrow(df)))
    
    for (i in 1:length(labels)) {
      plot <- plot + annotate("text", 
                              x = i, 
                              y = y_limit + y_ticks,
                              size = 5,
                              label= paste("n =", labels[i]))
    }
  } 
  
  return(plot)
}

# A function to generate the line charts Graphs G & H
line_chart <- function(plot_data, title, y_label, y_ticks) {
  plot <- ggplot(plot_data, aes_string(x = "date", y = "subs", group = "Type", colour = "Type")) + 
    geom_line() + 
    theme_bw() + 
    ggtitle(title) +
    labs(y = y_label) + 
    scale_colour_manual(values=c("#732060", "#0A9DD9")) +
    theme(axis.title.x = element_blank(), 
          legend.position = "bottom", 
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 16), 
          axis.title.y = element_text(size = 20, vjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 14, angle = 90), 
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 24,face = "bold")) +
    scale_y_continuous(limits = c(0, max(plot_data[2]) + y_ticks), breaks = seq(0, round(max(plot_data[2]), digits = -1) + y_ticks, by = y_ticks))
  
  return(plot)
}

# A function to generate the bar charts Graphs I & J
bar_chart  <- function(plot_data, title, x_label, y_label, labels) {
  plot <- ggplot(plot_data, aes_string (x = "date", y = "monthly")) + 
    geom_bar(stat = "identity", fill = "#0A9DD9") +
    geom_line(aes_string(y = "cumulative", group = 1), colour = "#CF0C4E", size = 2) +
    theme_bw() +
    ggtitle(title) +
    labs(x = x_label, y = y_label) +
    theme(axis.title.x = element_text(size = 20, vjust = 0.5, face = "bold"),
          axis.title.y = element_text(size = 20, vjust = 1, face = "bold"),
          axis.text.x = element_text(size = 14, angle = 90), 
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 24,face = "bold")) +
    scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10))
  
  if (labels == TRUE) {
    labels  <- as.vector(plot_data$n)
    
    for (i in 1:length(labels)) {
      plot <- plot + annotate("text", 
                              x = i, 
                              y = 90,
                              size = 5,
                              label= paste("n =", labels[i]))
    }
  }
  
  return(plot)
}

# A function to return a dataframe of summary stats for each boxplot
stats_summary <- function(plot_data) {
  return(data.frame(date = levels(plot_data$date),
                    min = as.vector(by(plot_data, plot_data$date, function(df) min(df$days))),
                    max = as.vector(by(plot_data, plot_data$date, function(df) max(df$days))),
                    median = as.vector(by(plot_data, plot_data$date, function(df) median(df$days)))
                    )
         )
}

###############################################################################
# Generate graphs

# Create a director to store the charts in
dir.create(paste(root_dir, "/charts", sep = ""))

out_width <- 800
out_height <- 500

graphA <- boxplot(plot_data = graphA_data, 
                  title = "Initial Submission Received > Initial Submission Decision", 
                  x_label = "Month of Decision", 
                  y_label = "Days",
                  n_labels = TRUE,
                  y_ticks = 2,
                  y_limit = max(graphA_data$days))
png(paste(root_dir, "/charts/graphA-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphA
dev.off()


graphB <- boxplot(plot_data = graphB_data, 
                  title = "Full Submission Received > Full Submission First Decision", 
                  x_label = "Month of Decision", 
                  y_label = "Days",
                  n_labels = TRUE,
                  y_ticks = 5,
                  y_limit = max(graphB_data$days))
png(paste(root_dir, "/charts/graphB-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphB
dev.off()


graphC <- boxplot(plot_data = graphC_data, 
                  title = "Acceptance > Publication of Version of Record", 
                  x_label = "Month of Publication", 
                  y_label = "Days",
                  n_labels = TRUE,
                  y_ticks = 5,
                  y_limit = max(graphC_data$days))
png(paste(root_dir, "/charts/graphC-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphC
dev.off()

graphD <- boxplot(plot_data = graphD_data, 
                  title = "Acceptance to PoA Time", 
                  x_label = "Month of Publication", 
                  y_label = "Days",
                  n_labels = TRUE,
                  y_ticks = 2,
                  y_limit = max(graphD_data$days))
png(paste(root_dir, "/charts/graphD-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphD
dev.off()

graphE <- boxplot(plot_data = graphE_data, 
                  title = "Initial Submission > Acceptance", 
                  x_label = "Month of Acceptance", 
                  y_label = "Days",
                  n_labels = TRUE,
                  y_ticks = 20,
                  y_limit = 360)
png(paste(root_dir, "/charts/graphE-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphE
dev.off()

graphF <- boxplot(plot_data = graphF_data, 
                  title = "Time Spent Revising Article", 
                  x_label = "Month of Acceptance", 
                  y_label = "Days",
                  n_labels = TRUE,
                  y_ticks = 20,
                  y_limit = 360)
png(paste(root_dir, "/charts/graphF-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphF
dev.off()

graphG <- line_chart(plot_data = graphG_data,
                     title = "Submission Volume",
                     y_label = "Number of Articles Received",
                     y_ticks = 20)
png(paste(root_dir, "/charts/graphG-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphG
dev.off()

graphH <- line_chart(plot_data = graphH_data,
                     title = "Volume of Acceptances and Publications",
                     y_label = "Number of Articles",
                     y_ticks = 5)
png(paste(root_dir, "/charts/graphH-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphH
dev.off()

graphI <- bar_chart(plot_data = graphI_data,
                    title = "Initial Submission Encouragement Rate",
                    x_label = "Month of Decision",
                    y_label = "Percentage Encouraged",
                    labels = TRUE)
png(paste(root_dir, "/charts/graphI-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphI
dev.off()

graphJ <- bar_chart(plot_data = graphJ_data,
                    title = "Full Submissions Sent for Revision",
                    x_label = "Month of Decision",
                    y_label = "Percentage Sent for Revision",
                    labels = TRUE)
png(paste(root_dir, "/charts/graphJ-", Sys.Date(), ".png", sep = ""), width = out_width, height = out_height)
graphJ
dev.off()



###############################################################################
# Generate boxplot data summaries

write.csv(stats_summary(graphA_data), file = paste(root_dir, "/summaries/graphA_summary.csv", sep = ""))
write.csv(stats_summary(graphB_data), file = paste(root_dir, "/summaries/graphB_summary.csv", sep = ""))
write.csv(stats_summary(graphC_data), file = paste(root_dir, "/summaries/graphC_summary.csv", sep = ""))
write.csv(stats_summary(graphD_data), file = paste(root_dir, "/summaries/graphD_summary.csv", sep = ""))
write.csv(stats_summary(graphE_data), file = paste(root_dir, "/summaries/graphE_summary.csv", sep = ""))
write.csv(stats_summary(graphF_data), file = paste(root_dir, "/summaries/graphF_summary.csv", sep = ""))