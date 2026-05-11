# Election simulation with removed precincts 

# rcv sim function, council_d1 can be changed to data table for any of the districts or for mayor (for mayor, change to seats = 1) 

rcv_sim <- function(council_d1, precinct_col = "precinct", exclude_precincts = NULL, 
                    seats = 3) {
  
  # remove excluded precincts, if any 
  if (!is.null(exclude_precincts)) {
    council_d1 <- council_d1 |>
      filter(!(!!sym(precinct_col) %in% exclude_precints))
  } 
  
  candidates <- setdiff(names(council_d1), precinct_col)
  eliminated <- c()
  winners <- c()
  
  
  # run the rounds 
  while(length(winners) < seats && length(eliminated) < length(candidates)) {
    top_choices <- apply(council_d1[candidates], 1, function(ballot) {
      valid <- ballot[!is.na(ballot) & !(names(ballot) %in% c(eliminated, winners))]
      
      if (length(valid) == 0) return(NA)
      return(names(valid)[which.min(valid)])
    })
    
    #count votes 
    
    votes <- table(factor(top_choices, levels = candidates))
    total <- sum(votes)
    threshold <- floor(total / (seats + 1)) + 1
    
    # check 
    
    new_winners <- names(votes[votes >= threshold & !names(votes) %in% winners])
    winners <- c(winners, new_winners)
    
    # get rid of lowest 
    if (length(new_winners) == 0 && length(winners) < seats) {
      active <- votes[!names(votes) %in% c(eliminated, winners)]
      if (length(active) > 0) {
        eliminated <- c(eliminated, names(active)[which.min(active)])
      } else {
        break 
      } } }
  
  # return winners 
  return(winners[1:seats])
} 
