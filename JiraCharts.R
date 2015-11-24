# Libraries ---------------------------------------------------------------


setwd("C:/Users/jhabib/Documents/R/JiraCharts")
library(httr)
library(jsonlite)
library(plyr)
library(reshape2)
library(data.table)
library(googleVis)

# Helpers -----------------------------------------------------------------


json_list_auth <- function(json_url, username, password){
  jd <- GET(json_url, authenticate(username, password))
  jd_content <- content(jd, as = "text")
  return(fromJSON(jd_content))
}


json_list <- function(json_url){
  jd <- GET(json_url)
  jd_content <- content(jd, as = "text")
  return(fromJSON(jd_content))
}


# Get Sprints from Board --------------------------------------------------


board_url <- "https://aunr-jira-01.ali.local/rest/greenhopper/latest/sprintquery/113/?includeHistoricSprints=true&includeFutureSprints=true"
board <- json_list_auth(board_url, "jhabib", "November@2015")
board_sprints <- board$sprints$id
# write.csv(board_sprints, file = "ngp_sprints.csv", row.names = FALSE)


# Get Issues from Sprints -------------------------------------------------


sprint_issues <- data.table()
completed <- data.table()
incompleted <- data.table()
punted <- data.table()
for(sprint_id in board_sprints){
  sp_url <- paste("https://aunr-jira-01.ali.local/rest/greenhopper/latest/rapid/charts/sprintreport?rapidViewId=113&sprintId=", 
                  sprint_id, 
                  sep="")
  sp_list <- json_list(sp_url)
  
  if(!(class(sp_list$contents$completedIssues$key)=="NULL"))
    completed <- data.table(sprint_id, "completed", sp_list$contents$completedIssues$key)
  
  if(!(class(sp_list$contents$incompletedIssues$key)=="NULL"))
    incompleted <- data.table(sprint_id, "incompleted", sp_list$contents$incompletedIssues$key)
  
  if(!(class(sp_list$contents$puntedIssues$key)=="NULL"))
    punted <- data.table(sprint_id, "punted", sp_list$contents$puntedIssues$key)
  
  this_list <- list(sprint_issues, completed, incompleted, punted)
  sprint_issues <- rbindlist(this_list)
}
setnames(sprint_issues, c("sprintid", "issuestatus", "issuekey"))
write.csv(sprint_issues, file = "sprint_issues.csv", row.names = FALSE)


# Get Issue Details ------------------------------------------------------

# JIRA current allows maxResults=1000
# However, we break our issue list into chunks of 200 issues each
issue_work_estimates <- data.table()

issue_keys <- unique(sprint_issues$issuekey)
issue_key_chunks <- split(issue_keys, ceiling(seq_along(issue_keys)/200))

for(i in seq(length(issue_key_chunks))){
  
  key_vec <- do.call("paste", c(issue_key_chunks[i], collapse=","))
  query_url <- paste("https://aunr-jira-01.ali.local/rest/api/latest/search?jql=issuekey%20in%20(", key_vec, ")&maxResults=200&fields%20in%20(id,key,aggregatetimeoriginalestimate,aggregatetimespent,aggregatetimeestimate)", sep = "")
  issue_details <- json_list(query_url)
  
  Sys.sleep(2)
  
  this_chunk_data <- data.table(issue_details$issues$key, 
                                issue_details$issues$fields$aggregatetimeoriginalestimate, 
                                issue_details$issues$fields$aggregatetimespent, 
                                issue_details$issues$fields$aggregatetimeestimate)
  
  # write.csv(this_chunk_data, paste("chunk", i, ".csv", sep = ""), row.names = FALSE, col.names = FALSE)
  
  temp_list <- list(issue_work_estimates, this_chunk_data)
  issue_work_estimates <- rbindlist(temp_list)
}
setnames(issue_work_estimates, c("issuekey", "originalestimate", "timespent", "currentestimate"))
write.csv(issue_work_estimates, file = "issue_work_estimates.csv", row.names = FALSE)

# Combine Sprint Issues and Issue Work Data -------------------------------

setkey(sprint_issues)
setkey(issue_work_estimates)

sprint_issue_combined <- merge(sprint_issues, issue_work_estimates, all = TRUE, by = "issuekey")
write.csv(sprint_issue_combined, file = "sprint_issue_combined.csv", row.names = FALSE)


# Create Sprint Work Column Chart -----------------------------------------

sprint_work_agr <- aggregate(cbind(originalestimate, timespent, currentestimate) ~ sprintid, data = sprint_issue_combined, FUN = function(x){sum(x)/(8*60*60)})
sprint_chart <- gvisColumnChart(sprint_work_agr, xvar = "sprintid", yvar = c("originalestimate", "timespent", "currentestimate"))
plot(sprint_chart)

# Tests -------------------------------------------------------------------

# test_sp_url <- "https://aunr-jira-01.ali.local/rest/greenhopper/latest/rapid/charts/sprintreport?rapidViewId=113&sprintId=1326"
# test_sp_res <- GET(test_sp_url)
# test_sp_text <- content(test_sp_res, as = "text")
# test_sp_list <- fromJSON(test_sp_text)
