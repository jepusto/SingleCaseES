# Load data
dat <- 
  read.table("{user_path}", header = {user_header}, sep = "{user_sep}", 
             quote = '{user_quote}', stringsAsFactors = FALSE, check.names = FALSE) 
