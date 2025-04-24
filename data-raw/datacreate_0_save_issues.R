

# a way to save open issues in a data.frame - source this whole file
# install/load the githubr pkg and gh pkg
# but we are avoiding making EJAM depend on the githubr or gh package so this is not in the /R/ folder


############################################## #
## helper function to just look at the structure of data on issues:

print1field = function(fieldname, issues_details, egn = 29, justchecking = TRUE) {
  
  fields = names(issues_details)
  i = which(fields == fieldname)
  
  cat('\n---------------------------------- ', i,'\n')
  cat(fields[i], '   ')
  cat('(', class(issues_details[[fields[i]]]), 'of', 
      length(issues_details[[fields[i]]]), 'issues) \n')
  
  islist =   ('list' %in% unique(sapply(issues_details[[fields[i]]], class)))
  if (islist) {
    cat(fields[i], 'info can be a list for each issue: \n\n')
    # browser()
    if (length(issues_details[[fields[i]]]) < egn) {egn <- 1}
    
    
    if ((length(issues_details[[fields[i]]]) > 1 ) &&
        'list' %in% class( (issues_details[[fields[i]]][[egn  ]])[[1]]) ) {
      cat(' and 1 issue can have multiple', fields[i],' so the issue has a list of lists, \n e.g., 1 issue has multiple labels but each label has a few fields like name, description, color, etc. \n')
      
      x = do.call(cbind, issues_details[[fields[i]]][[egn]] )
      
      ## see example for just 1  issue, multiple assignees or whatever 
      colnames(x) <- paste0(
        paste0('issue_', issues_details[[fields[which(fields  %in% 'number')]]][[egn  ]]),
        '_', fields[i], '_', 1:NCOL(x)
      )
      # return all?
      y <- issues_details[[fields[i]]]
      
    } else {
      
      x = cbind(issues_details[[fields[i]]][[egn  ]])
      ## see example for just 1 issue
      if (NCOL(x) == 1) { # should be the case
        colnames(x) =  paste0('issue_', issues_details[[fields[which(fields  %in% 'number')]]][[egn  ]])
      }
      # return all even though printed only some
      y = cbind(issues_details[[fields[i]]])
      # if (NCOL(x) == 1) { # should be the case
      #   colnames(x) =  paste0('issue_', issues_details[[fields[which(fields  %in% 'number')]]] )
      # } 
    }
    
    print(x)
    x <- y
    
  } else {
    
    cat("1st 2 issues: \n")
    x = data.frame(unlist( issues_details[[fields[i]]]  ))
    if (NCOL(x) == 1) {
      colnames(x) = fields[i]
    }
    print(head(x, 2))
  }
  if (!justchecking) {return(x)} else {return(NULL)}
  
}
#################### # 
# helper function to print structure info 

printfieldinfo = function(repo, issues_details, fieldnames = NULL, justchecking = TRUE) {
  
  if (!("githubr" %in% loadedNamespaces())) {
    stop("for the get_issues() function to work, you have to install and then load the githubr package using library()")
  }
  if (!("gh" %in% loadedNamespaces())) {
    stop("for the gh() function to work, you have to install and then load the gh package using library()")
  }
  warning('work in progress may not work for justchecking=FALSE')

  if (missing(issues_details)) {
    if (!missing(repo)) {
      issues_details <-  get_issues(repo)
    } else {
      stop('must specify repo or provide output of get_issues()')
    }
  } else {
    if (!missing(repo)) {
      warning('should specify only repo or issues_details not both - ignoring repo and using issues_details')
    }
  }
  
  if (is.null(fieldnames) | missing(fieldnames)) {
    fieldnames = names(issues_details)
  } else {
    if (!all(fieldnames %in% names(issues_details))) {stop('all fieldnames must be in names(issues_details')}
  }
  
  out = list()
  for (fieldname in (fieldnames)) {
    n = which(fieldnames == fieldname)
    out[[n]] <- print1field(fieldname = fieldname, issues_details = issues_details, egn = 29, justchecking = justchecking)
  }
  
  if (!justchecking) {
    names(out) <- fieldnames
    return(out)
  } else {
    return(NULL)
  }
  cat("\n\n")
}
#################### # 

################################################ #

#' download/save issues list from a github repo
#'
#' @param repo owner/reponame
#' 
#' @param savedetails must set TRUE to retain all the details (they are not returned) - 
#'   this will save .rda with all info provided by githubr pkg function get_issues()
#' @param savebasics if TRUE, save data.frame of just the key info as .rda and .csv, instead of just returning it invisibly
#' @param folder folder without filename, for .rda or .csv files to be saved in
#' @param printhigh set FALSE if you want to print most recent ones. TRUE means only if "high" found within labels
#' 
#' @param justchecking passed to [printfieldinfo()] not yet working?
#' 
#' @return invisibly returns data.frame that would be saved by savebasics = TRUE
#'
#' @export
#' @keywords internal
#'
#' @seealso [repolist()]
#'
repoissues <- function(repo, savedetails = FALSE, savebasics = FALSE, folder = ".", printhigh=T, justchecking =TRUE) {
  
  if (missing(repo)) {stop('must specify a repo as "OWNERNAME/REPONAME"')}
  if (!("githubr" %in% loadedNamespaces())) {
    stop("for the get_issues() function to work, you have to install and then load the githubr package using library()")
  }
  if (!("gh" %in% loadedNamespaces())) {
    stop("for the gh() function to work, you have to install and then load the gh package using library()")
  }
  issues_details <- issues <-  get_issues(repo)
  
  # show info about each field returned by get issues
  x = printfieldinfo(repo = repo, issues_details = issues_details, justchecking = justchecking)
  if (justchecking) {warning('not using justchecking here')}
  ############################################## #
  
  # start saving the info in a simpler format and just key info
  
  # > names(issues)
  #
  # [1] "url"                      "repository_url"           "labels_url"               "comments_url"            
  # [5] "events_url"               "html_url"              "id"                       "node_id"                 
  # [9] "number"                   "title"                    "user"                     "labels"                  
  # [13] "state"                    "locked"                   "assignee"                 "assignees"               
  # [17] "milestone"                "comments"                 "created_at"               "updated_at"              
  # [21] "closed_at"                "author_association"       "sub_issues_summary"       "active_lock_reason"      
  # [25] "draft"                    "pull_request"             "body"                     "closed_by"               
  # [29] "reactions"                "timeline_url"             "performed_via_github_app" "state_reason"       
  
  assignee1 = rep(NA, length(issues$id))
  for (i in 1:length(issues$id)) {
    if (0 < length(issues$assignees[[i]])) {
      assignee1[i] =  (issues$assignees[[i]][[1]]$login ) 
    } else {
      assignee1[i] =  ("none")
    } 
  }
  # browser()
  body = rep("", length(issues$id))
  # this would work but saves reactions url when there is no actual body text:
  # body[1 == sapply(sapply(issues$body, function(x) x[[1]]), length)] <- unlist(sapply(issues$body, function(x) x[[1]]))
  #  this works and usually saves empty char when no actual body text:
  for (i in 1:length(issues$id)) {
    if (class(issues_details$body[[i]]) == 'character') {
      body[i] <- issues_details$body[[i]]
    } else {
      NA
    }
  }
  
  issues <- data.frame(
    
    rowid =   1:length(issues_details$id),
    number = sapply(issues_details$number, function(x) x[[1]]),
    state  = sapply(issues_details$state,  function(x) x[[1]]), # only gets open ones 
    title  = sapply(issues_details$title,  function(x) x[[1]]),
    
    ############################ #
    # labels_as_list = issues_details$labels, # a list that is a bit complicated
    
    labels_name = sapply(seq_along(issues_details$labels), function(x) {
      paste0(sapply(issues_details$labels[x][[1]], function(y) y$name), collapse = "|")
    }),
    labels_id = sapply(seq_along(issues_details$labels), function(x) {
      paste0(sapply(issues_details$labels[x][[1]], function(y) y$id), collapse = "|")
    }),
    labels_color = sapply(seq_along(issues_details$labels), function(x) {
      paste0(sapply(issues_details$labels[x][[1]], function(y) y$color), collapse = "|")
    }),
    labels_description  = sapply(seq_along(issues_details$labels), function(x) {
      paste0(sapply(issues_details$labels[x][[1]], function(y) y$description), collapse = "|")
    })
    ############################ #
    
    
  )  
  
  comments_txt = rep("", length(issues_details$id))
  for (i in 1:length(issues_details$id)) {
    issuenum = unlist(issues_details$number)[i]
    
    # VERY SLOW WAY TO DO THIS:
    
    x = gh(paste0("https://api.github.com/repos/", repo, "/issues/", issuenum, "/comments") )
    
    comments_txt[i] <- paste0(sapply(x, function(z) z$body), collapse = "|")
    
  }
  
  
  reform = function(z) {
    sapply(z, function(x) {
      if (length(x[[1]]) > 0 ) {
        
        paste0(sapply(x[[1]], function(z) x[[1]]  ), collapse = " | ") 
        
        # x[[1]]
        
      } else {
        ""
      }
    })
  }
  
  more = data.frame( 
    
    commentcount = reform(issues_details$comments),
    body      = body,
    comments_txt = comments_txt,
    draft = reform(issues_details$draft),
    comments_url = reform(issues_details$comments_url),
    
    created_at     = reform(issues_details$created_at),
    updated_at     = reform(issues_details$updated_at),
    # closed_at      = reform(issues_details$closed_at), # never
    
    user = sapply(issues_details$user, function(x) x[['login']]),
    assignee1 = assignee1
  )
  more$created_at = substr(more$created_at, 1, 10)
  more$updated_at = substr(more$updated_at, 1, 10)
  
  
  issues = data.frame(issues, more)
  
  issues <- issues[order(issues$number, decreasing = TRUE),]
  
  when <- Sys.Date()
  
  if (savedetails) {
    cat("\n")
    fname <- paste0(when, "_issues_details.rda")
    fname <- file.path(folder, fname)
    save(issues_details, file = fname)
    cat('saved detailed info about open issues as a list in', normalizePath(fname), '\n')
  }
  
  if (savebasics) {
    cat("\n")
    fname <- paste0(when, "_issues.rda")
    fname <- file.path(folder, fname)
    save(issues, file = fname)
    cat('saved basic info about open issues as a data.frame in', normalizePath(fname), '\n')
    
    fname <- paste0(when, "_issues.csv")
    fname <- file.path(folder, fname)
    write.csv(issues, row.names = FALSE, file = fname)
    cat('saved basic info about open issues as a data.frame in', normalizePath(fname), '\n')
    cat("\n")
  }
  
  if (printhigh) {
    
    print((issues[grepl("high", issues$labels_name, ignore.case = T), 
                  # setdiff(names(issues), c('body', 'draft', 'comments_url','labels_description', 'comments_txt'))
                  c( 'number' ,'state' ,'title', 'commentcount', 'updated_at')
    ]))
    
  } else {
    print(head(issues))
  }
  
  invisible(issues)
}
################################################ #
