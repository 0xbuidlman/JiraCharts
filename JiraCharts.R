setwd("C:/Users/jhabib/Documents/R/JiraCharts")
library(httr)
library(jsonlite)
library(plyr)
library(reshape2)

rv <- "https://aunr-jira-01.ali.local/rest/greenhopper/latest/rapidviews/list"
rapid_views <- GET(rv, authenticate("jhabib", "November@2015"))

sb <- "https://aunr-jira-01.ali.local/rest/greenhopper/latest/sprintquery/113/?includeHistoricSprints=true&includeFutureSprints=true"
sprints <- GET(sb)

board_sprints <- content(sprints, as = "text")
sp_list <- fromJSON(board_sprints)
sp_df <- as.data.frame(sp_list[1])

write.csv(sp_df, file = "ngp_sprints.csv")

sp <- "https://aunr-jira-01.ali.local/rest/greenhopper/latest/rapid/charts/sprintreport?rapidViewId=113&sprintId=1326"

sprint_doc <- GET(sp)
sprint_details <- content(sprint_doc, as = "text")
sprint_details_list <- fromJSON(sprint_details)

json_list <- function(json_url){
  jd <- GET(json_url)
  jd_content <- content(jd, as = "text")
  return(fromJSON(jd_content))
}

completed_issues <- list()
incompleted_issues <- list()
punted_issues <- list()
for(sprint_id in sp_df$sprints.id){
  sp_url <- paste("https://aunr-jira-01.ali.local/rest/greenhopper/latest/rapid/charts/sprintreport?rapidViewId=113&sprintId=", sprint_id, sep="")
  sp_list <- json_list(sp_url)
  for(ci in sp_list$contents$completedIssues$key){
    temp <- list(sprint_id, ci)
    completed_issues <- rbind(completed_issues, temp)
  }
  for(ii in sp_list$contents$incompletedIssues$key){
    temp <- list(sprint_id, ii)
    incompleted_issues <- rbind(incompleted_issues, temp)
  }
  for(pi in sp_list$contents$puntedIssues$key){
    temp <- list(sprint_id, pi)
    punted_issues <- rbind(punted_issues, temp)
  }
}


# ti_url <- "https://aunr-jira-01.ali.local/rest/api/latest/issue/CTOB-97"
# ti_doc <- GET(ti_url)
# ti_details <- content(ti_doc, as = "text")
# ti_details_list <- fromJSON(ti_details)

# df_list <- list(completed_issues, incompleted_issues, punted_issues)
# df_list <- lapply(df_list, function(x){ as.data.frame(x); x})
# df_list <- lapply(df_list, 'rownames<-', NULL)

ci_df <- as.data.frame(completed_issues)
row.names(ci_df) <- NULL
names(ci_df) <- c("sprint_id", "completed_key")
ci_df <- data.frame(lapply(ci_df, as.character), stringsAsFactors=FALSE)
write.csv(ci_df, file = "completed_issues_df.csv", row.names = FALSE)

ii_df <- as.data.frame(incompleted_issues)
row.names(ii_df) <- NULL
names(ii_df) <- c("sprint_id", "incompleted_key")
ii_df <- data.frame(lapply(ii_df, as.character), stringsAsFactors=FALSE)
write.csv(ii_df, file = "incompleted_issues_df.csv", row.names = FALSE)

pi_df <- as.data.frame(punted_issues)
row.names(pi_df) <- NULL
names(pi_df) <- c("sprint_id", "punted_key")
pi_df <- data.frame(lapply(pi_df, as.character), stringsAsFactors=FALSE)
write.csv(pi_df, file = "punted_issues_df.csv", row.names = FALSE)

df_list <- list(ci_df, ii_df, pi_df)
df_merged <- Reduce(function(...) merge(..., all=T), df_list)
df_merged_new <- melt(df_merged, id.vars = "sprint_id")
df_merged_new[duplicated(df_merged_new[setdiff(names(df_merged_new), "variable")]), "value"] <- NA
df_merged_new <- na.omit(df_merged_new)

for(issue in ci_df$key){
  
}