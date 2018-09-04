#' text mining function
#' The first function will extract unique text values in a variable with concatenated text values.
#' This returns a vector of unique text fields.
#' The second function will create columns for unique text value and return a data frame. 
#' The third function will create document term matrix for terms specified in term_df. 
#' term_df is a dataframe of concepts with columnnames same as concepts name and each concept 
#' containing text value to text mine.



extract_unique <- function(dt,id_var,txt_var,split_char){
  
  collapse_all <- paste0(dt[,txt_var],collapse = split_char)
  all_txt<-strsplit(as.character(collapse_all),split_char)[[1]]
  
  unique_txt <- unique(all_txt)
  unique_txt
  
}


create_binary_dtm <- function(dt, id_var, txt_var, unique_txt,split_char){
  m_colnames <- unique_txt
  
  m_colnames <-  gsub(" ","",m_colnames)
  m_colnames <-  gsub("-","_",m_colnames)
  m_colnames <-  gsub("\\(","_",m_colnames)
  m_colnames <-  gsub(")","_",m_colnames)
  m_colnames <-  gsub("/","_",m_colnames)
  
  m_dtm <- data.frame(matrix(ncol = length(m_colnames),nrow = nrow(dt)))
  colnames(m_dtm)<-unique_txt
  
  pb <- txtProgressBar(min = 0, max = nrow(dt), style = 3)
  for(i in 1:nrow(dt))
  {
   
    each_txt <- strsplit(as.character(dt[i,txt_var]),split_char)[[1]]

    if(sum(is.na(each_txt))==0)
      m_dtm[i,each_txt] <- 1
    
    Sys.sleep(0.001)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  
  m_dtm[is.na(m_dtm)]<-0
  colnames(m_dtm) <- m_colnames
  m_dtm <- cbind(dt[,id_var],m_dtm)
  colnames(m_dtm)[1] <- id_var
  m_dtm
}

dtm_notes <- function(dt,term_df,id_var,note_var){
  dtm <-  data.frame(matrix(ncol = ncol(term_df),nrow = nrow(dt)))
  colnames(dtm) <- colnames(term_df)
  pb <- txtProgressBar(min = 0, max = ncol(term_df), style = 3)
  for(i in 1:ncol(term_df))
  {
    x <- as.character(term_df[,i])
    x<-x[!(is.na(x) | x=="") ]
    
    dtm[,i] <- grepl(paste(x, collapse = "|"), dt[,note_var],ignore.case = TRUE)  * 1
    Sys.sleep(0.001)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  
  dtm[,id_var] <- dt[,id_var]
  dtm
}

# Example:
# 
# unique_txt <- extract_unique(dt,"unique_id","concatenated_column_name",",")
# aa <- create_binary_dtm(dt,"unique_id","concatenated_column_name",unique_txt,",")
# 
# bb <- dtm_notes(dt,concepts,"unique_id","text_column_name")


