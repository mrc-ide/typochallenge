########################################
# function to make a title look nice in the output #
########################################
make_title <- function(txt)
{
  ntot <- 36
  nspaces <- (ntot - nchar(txt))/2
  return(paste0( makeNstr("-",ntot),"\n",
                 makeNstr(" ", nspaces),
                 txt,
                 makeNstr(" ", nspaces),"\n",
                 makeNstr("-",ntot),"\n"))
}

generate_personal_stats <- function(values){
  
  # display as new output text the number of dates typed in until now
  txt2 <- paste0(make_title("YOUR STATS"),
                 "You have entered a total of ", 
                 values$Ntyp, 
                 " dates (", sum(values$tabEntries[, 5] == FALSE), 
                 " mistake(s) so far)")
  
  # and if an error was made, display it
  if (values$tabEntries[nrow(values$tabEntries), 5]) {
    txt1 <- "" 
  } else {
    txt1 <- paste0(make_title("TYPO AT LAST ENTRY!"),
                   "The date was '",
                   values$tabEntries[nrow(values$tabEntries), 2],
                   "' and you typed in '",
                   values$tabEntries[nrow(values$tabEntries), 1],
                   "'.\n\n\n")
  }
  
  if (sum(values$tabEntries[, 5] == TRUE)) {
    mean_time <- round(mean(as.double(values$tabEntries[values$tabEntries[, 5] 
                                                        == TRUE, 4])), 
                       digit=2)
    txt3 <- paste("\nYou are taking an average of",
                  mean_time,
                  "s per correct entry.\nYour personal record for a (correct) entry is",
                  values$shortestEntry,"seconds.")
  } else {
    txt3 <- paste("\nYou have not typed any correct date yet.")
  }
  
  output <- list(text = renderText(txt1), 
                 text_stats = renderText(paste0(txt2,txt3)))
  return(output)
}