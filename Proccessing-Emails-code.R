#141B Project 2 Text Processing 
#"I completed this assignment myself without consulting books or tutorials that specifically addressed this data set."

#----------------------------------------------------------------------------
#In this first section, I am investigating the files and testing possible
#procedures to include in the read function:
#----> The read email function and analysis is below this first section
#----------------------------------------------------------------------------
#Libraries:
#install.packages('rlist')
#install.packages('stringr')
#install.packages('ggpubr')
library(rlist)
library(stringr)
library(ggplot2)
library(ggpubr)
library(dplyr)

#Make the files easily attainable:
folder_names = list.files(getwd(), full.names = TRUE)
all_files = unlist(lapply(folder_names, list.files, full.names = TRUE))

#Check to see the format of each first entry in the headers:
first_lines = sapply(all_files, readLines, n = 1)
test_first_lines = grepl("^[A-Z]|^[A-Z].*: ", first_lines)
all(test_first_lines == TRUE)
which(test_first_lines == FALSE) 

#It appears as though the 4865 email does not have the same format as the 
#rest of the first lines.
#To check it
readLines(all_files[4865])
#The 4865 file is not of the same format as the rest of the files:
#When writing the function we need a check for this file. Either remove or treat
#the entire email as the body.

#I will do a few more checks for the files
all_files[4865] #The file name doesn't indicate the file as a special file
readLines(all_files[4865]) 
#If we read the lines again in the 4865th file, we see that the each line 
#starts with mv and the last line also has cmds.
#I will see if any other files have this pattern:
junk_file_ind = sapply(all_files, function(x) any(grepl("^(mv) ", readLines(x, warn = FALSE), useBytes = TRUE)))
#I got warnings about strings not in the locale, I turned  useBytes to TRUE to avoid the warning
#I also received warnings about incomplete final lines. It doesn't seem like this is a 
#major issue.
any(junk_file_ind == TRUE)
which(junk_file_ind == TRUE)

#Now, I will check for headers missing "from": Date, From, and To (or In-Reply-To, or Bcc)
junk_file_ind1 = sapply(all_files, function(x) any(grepl("^From |^From: ", readLines(x, warn = FALSE, n=50), useBytes = TRUE)) == TRUE)
junk_file_ind2 = sapply(all_files, function(x) any(grepl("^Date: ", readLines(x, warn = FALSE), useBytes = TRUE)) == TRUE)
junk_file_ind3 = sapply(all_files, function(x) any(grepl("^To: |^In-Reply-To: |^Bcc: |^Delivered-To: |^[Cc]{2}: ", readLines(x, warn = FALSE), useBytes = TRUE)) == TRUE)
which(junk_file_ind1 == FALSE)
which(junk_file_ind2 == FALSE)
which(junk_file_ind3 == FALSE)
omit_files=which(junk_file_ind3 == FALSE)
#From the prompt, the To, In-Reply-To, or Bcc, keys are mandatory
#There are a few files that are missing these mandatory keys
#Since they are mandatory, I will not read these files. 

#Lastly, I want to confirm the header format:
header_check = sapply(all_files, function(x) any(grepl("^[a-z].*", readLines(x, warn = FALSE, n = 6), useBytes = TRUE) == TRUE))
any(header_check == TRUE)
which(header_check == TRUE) #Doesn't appear to be any header keys in non-capitals except for 4865

#Checking for files that don't have a blank line:
header_check = sapply(all_files, function(x) any(grepl("^$", readLines(x, warn = FALSE), useBytes = TRUE) == TRUE))
any(header_check == FALSE)
which(header_check == FALSE) #4865

header_check = sapply(all_files, function(x) any(which(readLines(x, warn = FALSE) == "") > 0))
any(header_check == FALSE)
which(header_check == FALSE)

#Check for non-alphabetic/numeric characters that start a header key:
header_check = sapply(all_files, function(x) any(grepl("^>[A-Z][a-z]+: ", readLines(x, warn = FALSE, n=40), useBytes = TRUE) == TRUE))
any(header_check == TRUE)
which(header_check == TRUE)
#783, 784, 2510  have a ">" character at the beginning of a key
#Since there are only 3 and I will not be using the Received: key for my
#Analysis, I will not change them.



#Now, I will test how I will break up the emails:
email_contents = readLines(all_files[4666])  #4666 for attachments

#According to the description and from observation, The first blank line splits 
#the header from the rest of the body. So, I think the best way to go about this
#is to split the email into two parts and read the patterns separately.
header_body_split_pos = which(email_contents == "")[1]
header_split = email_contents[1:(header_body_split_pos-1)]
body_split = email_contents[(header_body_split_pos+1):length(email_contents)]

#Now, I want to split the header into a character vector as name:value pairs:
(header_pattern = grepl("^[A-Z]|^[A-Z].*: ", header_split)) #FIX THIS
#(header_pattern2 = grepl("^From |^[A-Z].*: ", header_split)) 
#I used grepl() to find each single header entry. Now, I will use cumsum() to
#group the entries and I will split the header by these groupings
header_groups = cumsum(header_pattern)
separated_header_entries = split(header_split, header_groups) #character vector
separated_header_entries = sapply(separated_header_entries, paste, collapse = " ") #Char Vector


#The next step would be to separate each entry title from the values
#To help facilitate the process, I will check if the first entry title is in the
#same format as the other titles and, if not, I will make it so. This will hopefully
#be useful when I want to split the entries:

title_pattern = regexpr("^[A-Z][^:]*:|^[A-Z][a-z]*[^[:space:]]", separated_header_entries)
header_titles_values = regmatches(separated_header_entries, title_pattern, invert = TRUE)
header_titles_values = matrix(unlist(header_titles_values), ncol = 2, byrow = 2)
headers_titles = regmatches(separated_header_entries, title_pattern, invert = FALSE)
headers_titles = gsub("^([A-Z][^[:space:]]*):", "\\1", headers_titles)
headers_values_matrix = cbind(headers_titles, header_titles_values[,2])
colnames(headers_values_matrix) = c("headers", "values")       
#Create a vector:
header_values_vector = headers_values_matrix[,2]
names(header_values_vector) = headers_values_matrix[,1]

#Now, we will read the body:
#I think the best method here would to have a check on whether the email has
#attachments or not. I will first start with code that looks for the "--" at the
#beginning of each attachment to 

email_attachments = list()
message_body = list()
if("Content-Type" %in% names(header_values_vector) &
  grepl(".*boundary=.+", header_values_vector["Content-Type"])){
    attachment_boundary = gsub('.*\\"(.+)\\"$', "\\1",  header_values_vector["Content-Type"]) #if issues, could also use strlocate
    #Use the boundary to find the attachments
    attachment_ind = grepl(paste0("^(--)", attachment_boundary, "-*"), body_split, useBytes = TRUE)
    #Get any text before attachments start
    #if(attachment_ind[1] != TRUE){
    # before_attachments=which(attachment_ind==TRUE)[1]
    #  message_body = append(message_body,body_split[1: before_attachments-1])
    #}
    #if(attachment_ind[length(attachment_ind)]!=TRUE){
    # end_attach_pos = which(attachment_ind==TRUE)[length(which(attachment_ind==TRUE))]
    # message_body = append(message_body, body_split[(end_attach_pos+1):length(attachment_ind)])
    #}
    #Group the attachments:
    attach_groups_nums = cumsum(attachment_ind)
    attachment_split = split(body_split, attach_groups_nums) 
    #attachment_split = sapply(attachment_split, paste, collapse = " ") MAY NEED TO PAST TOGETHER
    remove_body_locate = sapply(1:length(attachment_split),
                                function(x) any(grepl('^Content-Type:', attachment_split[[x]], useBytes = TRUE)))
    #HOW TO SEARCH THROUGH LIST^^
    remove_body_attach = sapply(1:length(attachment_split), 
                                function(x) any(grepl("Content-Type: text/plain", attachment_split[[x]], ignore.case = TRUE, useBytes = TRUE)))
    attach_locate = which(remove_body_locate == TRUE & remove_body_attach == FALSE)
    find_body = which(remove_body_attach == FALSE & remove_body_locate==FALSE)  
    body_text_attach=which(remove_body_attach == TRUE & remove_body_locate == TRUE)  
    message_body = append(message_body,sapply(c(find_body, body_text_attach), function(x) attachment_split[[x]]))
    email_attachments = list.append(email_attachments, sapply(attach_locate, function(x) attachment_split[[x]]))
}else{
  email_attachments=NA
  message_body=body_split 
}

#Now we list the elements together
#May want to unlist attachments*****************
return_value = list(header_values_vector, email_attachments , unlist(message_body)) 
rm(list = ls()[!(ls() %in% c('folder_names','all_files', 'omit_files'))])


##----------------------------------------------------------------------------------------
#FUNCTION TO READ TEXT AND ANALYSIS
##---------------------------------------------------------------------------------------
#Watch for line 4865!!!!!

#Omit these files that are missing mandatory fields
real_emails = all_files[-omit_files] 

#Function to get the header:
get_header = function(header_split){
  #Split the header into a character vector as name:value pairs
  header_pattern = grepl("^[A-Z][^[:space:]]* ", header_split, useBytes = TRUE) #"^[A-Z]|^[A-Z].*: "
  #grepl("^From |^[A-Z].*: ", header_split)) 
  #Group the entries and I will split the header by these groupings:
  header_groups = cumsum(header_pattern)
  separated_header_entries = split(header_split, header_groups) #character vector
  separated_header_entries = sapply(separated_header_entries, paste, collapse = " ") 
  #The next step would be to separate each entry title from the values
  #To help facilitate the process, I will check if the first entry title is in the
  #same format as the other titles and, if not, I will make it so. This will hopefully
  #be useful when I want to split the entries:
  title_pattern = regexpr("^[A-Z][^[:space:]]* ", separated_header_entries, useBytes = TRUE)
  header_titles_values = regmatches(separated_header_entries, title_pattern, invert = TRUE)
  header_titles_values = matrix(unlist(header_titles_values), ncol = 2, byrow = 2)
  headers_titles = regmatches(separated_header_entries, title_pattern, invert = FALSE)
  headers_titles = gsub("^([A-Z][^[:space:]]*):", "\\1", headers_titles, useBytes = TRUE) #"^([A-Z][^[:space:]]*): " MAY WANT TO FIX
  headers_values_matrix=cbind(headers_titles, header_titles_values[,2])
  colnames(headers_values_matrix)=c("headers", "values")       
  #Create a vector:
  header_values_vector_f = headers_values_matrix[,2]
  names(header_values_vector_f) = substring(headers_values_matrix[,1], 1, nchar(headers_values_matrix[,1])-1)
  return(header_values_vector_f)
}

body_attachment_split= function(attach_groups_nums, body_split){
  temp_body=list()
  temp_attach=list()
  attachment_split = split(body_split, attach_groups_nums) 
  remove_body_locate = sapply(1:length(attachment_split), 
                              function(x) any(grepl('^Content-Type:', attachment_split[[x]], useBytes = TRUE)))
  remove_body_attach = sapply(1:length(attachment_split), 
                              function(x) any(grepl("Content-Type: text/plain", attachment_split[[x]], ignore.case = TRUE, useBytes = TRUE)))
  attach_locate=which(remove_body_locate == TRUE & remove_body_attach == FALSE)
  find_body = which(remove_body_attach == FALSE & remove_body_locate == FALSE)  
  body_text_attach = which(remove_body_attach == TRUE & remove_body_locate == TRUE)  
  temp_body = append(temp_body,sapply(c(find_body, body_text_attach), function(x) attachment_split[[x]]))
  if(length(attach_locate)==0){
    temp_attach=list.append(temp_attach,NA)
  }else{
  temp_attach = list.append(temp_attach, sapply(attach_locate, function(x) attachment_split[[x]]))
        }
  return(list(temp_body,temp_attach))
}



get_body = function(body_split, header_items){
    email_attachments = list()
    message_body = list()
    if("Content-Type" %in% names(header_items) &
              grepl(".*boundary=.+", header_items["Content-Type"], useBytes = TRUE)){
    attachment_boundary = gsub('.*boundary=[ \\"]*([^\\"]*).*', "\\1",  header_items["Content-Type"], useBytes = TRUE) #if issues, could also use strlocate or add boundary '.*boundary=\\"*(.+)\\"*', "\\1"
    #Use the boundary to find the attachments
    attachment_ind = grepl(paste0("^(--)", attachment_boundary, "-*"), body_split, useBytes = TRUE)
    #Group the attachments:
    attach_groups_nums = cumsum(attachment_ind)
    if(all(attach_groups_nums == 0))
      { #This is for files that have different boundaries then what was indicated in the header
       attachment_ind=agrepl(paste0("^(--)", attachment_boundary, "-*"), body_split, max = 4, useBytes = TRUE) # most I see have two different but I put at most 4 for precautions
       attach_groups_nums = cumsum(attachment_ind)
       email_items=body_attachment_split(attach_groups_nums, body_split)
       message_body=email_items[[1]]
       email_attachments = email_items[[2]]
      }else{
        #attachment_split = split(body_split, attach_groups_nums) 
       # remove_body_locate = sapply(1:length(attachment_split), 
                             # function(x) any(grepl('^Content-Type:', attachment_split[[x]])))
        #HOW TO SEARCH THROUGH LIST^^
        #remove_body_attach = sapply(1:length(attachment_split), 
                                #function(x) any(grepl("Content-Type: text/plain", attachment_split[[x]], ignore.case = TRUE)))
        #attach_locate=which(remove_body_locate == TRUE & remove_body_attach == FALSE)
       # find_body = which(remove_body_attach == FALSE & remove_body_locate == FALSE)  
       # body_text_attach = which(remove_body_attach == TRUE & remove_body_locate == TRUE)  
       # message_body = append(message_body,sapply(c(find_body, body_text_attach), function(x) attachment_split[[x]]))
       # email_attachments = list.append(email_attachments, sapply(attach_locate, function(x) attachment_split[[x]]))
        email_items=body_attachment_split(attach_groups_nums, body_split)
        message_body=email_items[[1]]
        email_attachments = email_items[[2]]
      }
    }else if(any(grepl("^(--).*[multi|next]part.*-*", body_split, ignore.case = TRUE, useBytes = TRUE)==TRUE) |
             any(grepl("^(--).*[=]+.*Exmh.*-*", body_split, ignore.case = TRUE, useBytes = TRUE)==TRUE)) #If the boundary is not present in the header but there are attachments
        {
      attachment_ind = grepl("^(--).*[multi|next]part.*-*", body_split, useBytes = TRUE) # most I see have two different but I put at most 4 for precautions
      attach_groups_nums = cumsum(attachment_ind)
      email_items=body_attachment_split(attach_groups_nums, body_split)
      message_body=email_items[[1]]
      email_attachments = email_items[[2]]
        }else{
    email_attachments = NA
    message_body = body_split 
  }
  return(list(email_attachments, message_body))
}
#email_file=real_emails[5145]
read_emails= function(email_file){
  email_contents = readLines(email_file, warn=FALSE) 
  #Split the email into two parts: header and body
  header_body_split_pos = which(email_contents == "")[1]
  header_split = email_contents[1:(header_body_split_pos-1)]
  body_split = email_contents[(header_body_split_pos+1):length(email_contents)]
  #Read in the Header:
  header_values_vector = get_header(header_split)
  email_body = get_body(body_split, header_values_vector) ###############################
  email_attachments = email_body[[1]]
  if(length(email_body[[2]])==0){
  email_message = ""
  }else{
    email_message = email_body[[2]]
  }
  return(list(header_values_vector,  email_attachments, unlist(email_message)))
}


all_emails_list=lapply(real_emails, read_emails) 

##------------------------------------------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------------------------------------------
#VARIABLES CODE
##-----------------------------------------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------------------------------------------

#Emails marked as spam or ham
test_ham = grepl("_ham", all_files[-omit_files])
head(test_ham)
any(test_ham == FALSE)
isSpam= ifelse(!test_ham==TRUE, "spam", "ham")

#To confirm that ham and spam were marked correctly
length(which(isSpam == 'ham')) #Find the number of Ham emails(4838)

#First variable: find the subject(if it exists) and get if Re is in the beginning:-------------------------------------------------------------------
isRe_funct = function(file_num){
  if("Subject" %in% names(all_emails_list[[file_num]][[1]])
     & grepl("^Re:|^\\[.*\\][ ]*Re:", all_emails_list[[file_num]][[1]]["Subject"], ignore.case = TRUE)){
    return(TRUE)
  }
     return(FALSE)
}
isRe_vect=sapply(1:length(all_emails_list), isRe_funct)


#Second Variable: Number of lines in Body-------------------------------------------------------------------
#To get an accurate count, I will have to remove attachment separations if they are present in body
numLinesInBody_funct = function(file_num){
  if("Content-Type" %in% names(all_emails_list[[file_num]][[1]]) &
     grepl(".*boundary=.+", all_emails_list[[file_num]][[1]]["Content-Type"], useBytes = TRUE)){
     added_boundary = gsub('.*boundary=[\\"]*([^\\"]*).*', "\\1",  
                             all_emails_list[[file_num]][[1]]["Content-Type"]) 
     added_boundary = paste0("^(--)",  added_boundary, "-*")
     num_lines= sum(!grepl(added_boundary, all_emails_list[[file_num]][[3]], useBytes = TRUE))
     return(num_lines)
     }
  return(length(all_emails_list[[file_num]][[3]]))
}
numLinesInBody = sapply(1:length(all_emails_list), numLinesInBody_funct)

#Third Variable: body character count-------------------------------------------------------------------
bodyCharacterCount_funct= function(file_num){
  if("Content-Type" %in% names(all_emails_list[[file_num]][[1]]) &
     grepl(".*boundary=.+", all_emails_list[[file_num]][[1]]["Content-Type"], useBytes = TRUE) &
           length(all_emails_list[[file_num]][[3]])>1)
    {
    added_boundary = gsub('.*boundary=[\\"]*([^\\"]*).*', "\\1", 
                          all_emails_list[[file_num]][[1]]["Content-Type"]) 
    added_boundary = paste0("^(--)",  added_boundary, "-*")
    lines_to_count = !grepl(added_boundary, all_emails_list[[file_num]][[3]], useBytes = TRUE)
    num_char = sum(sapply(all_emails_list[[file_num]][[3]][lines_to_count], nchar, type = "bytes"))
    return(num_char)
    }else if("Content-Type" %in% names(all_emails_list[[file_num]][[1]]) &
             grepl(".*boundary=.+", all_emails_list[[file_num]][[1]]["Content-Type"], useBytes = TRUE) &
                   length(all_emails_list[[file_num]][[3]])==1)
                   {
                     return(1)
                   }
  return(sum(sapply(all_emails_list[[file_num]][[3]], nchar, type = "bytes")))
}
bodyCharacterCount = sapply(1:length(all_emails_list), bodyCharacterCount_funct) 


#Fourth Variable: subject exclamation point count-------------------------------------------------------------------
subjectExclamationCount_funct = function(file_num){
  if("Subject" %in% names(all_emails_list[[file_num]][[1]]) &
     gregexpr("!", all_emails_list[[file_num]][[1]]["Subject"])[[1]][1] != -1) #Can also use string count
    {
    num_excla = length(gregexpr("!", all_emails_list[[file_num]][[1]]["Subject"])[[1]])
    return(num_excla)
     }
  return(0)
}
subjectExclamationCount=sapply(1:length(all_emails_list), subjectExclamationCount_funct)


#Fifth Variable: Number of attachments-------------------------------------------------------------------
numAttachments_funct = function(file_num){
  if(any(!is.na(all_emails_list[[file_num]][[2]])==TRUE)){
    num_attachments = length(all_emails_list[[file_num]][[2]])
    return(num_attachments)
  }
  return(0)
}

numAttachments=sapply(1:length(all_emails_list), numAttachments_funct)

#Sixth Variable: Number of question marks in the subject-------------------------------------------------------------------
subjectQuestCount_funct = function(file_num){
  if("Subject" %in% names(all_emails_list[[file_num]][[1]]) &
     gregexpr("\\?", all_emails_list[[file_num]][[1]]["Subject"])[[1]][1] != -1) #Can also use string count
    {
    num_excla = length(gregexpr("\\?", all_emails_list[[file_num]][[1]]["Subject"])[[1]])
    return(num_excla)
     }
  return(0)
}
subjectQuestCount=sapply(1:length(all_emails_list), subjectQuestCount_funct)


#Seventh Variable: Determining Priority-------------------------------------------------------------------
#Pre-specification:
#priority_vals=table(sapply(1:600, function(x) list_test[[x]][[1]]["X-Priority"])) 
#priority_vals=table(sapply(1:600, function(x) list_test[[x]][[1]]["X-Smell-Priority"]))
# 3 seems to indicate a normal priority
# 2 seems to indicate high
# 1 seems to indicate a highest priority

priority_funct = function(file_num){
  if("X-Priority" %in% names(all_emails_list[[file_num]][[1]])){
    priority_high=grepl("high|1|2", all_emails_list[[file_num]][[1]]["X-Priority"])
    return(priority_high)
  }else if("X-Smell-Priority" %in% names(all_emails_list[[file_num]][[1]])){
    priority_high=grepl("high|1|2", all_emails_list[[file_num]][[1]]["X-Smell-Priority"])
    return(priority_high)
  }
  return(FALSE)
}

priority=sapply(1:length(all_emails_list), priority_funct)

#Eighth Variable: is "In-reply-to" in the header?-------------------------------------------------------------------

isInReplyTo_funct = function(file_num){
  if("In-Reply-To" %in% names(all_emails_list[[file_num]][[1]])){
    return(TRUE)
  }
  return(FALSE)
}
isInReplyTo=sapply(1:length(all_emails_list), isInReplyTo_funct)


#Ninth Variable: Multipart text-------------------------------------------------------------------
#I look for multipart because Multipart/text did not capture a hit
multipartText_funct= function(file_num){
  if("Content-Type" %in% names(all_emails_list[[file_num]][[1]]) &
     grepl("multipart", all_emails_list[[file_num]][[1]]["Content-Type"], ignore.case = TRUE)) 
  {
  return(TRUE)
  }
  return(FALSE)
}

multipartText = sapply(1:length(all_emails_list), multipartText_funct)


#Tenth Variable: Subject Spam Words text---------------------------------------------------------------------------------------
subjectSpamWords_funct = function(file_num){
  if("Subject" %in% names(all_emails_list[[file_num]][[1]]))
  {
    spam_words = "\\b(viagra|pounds|free|weight|guarantee|millions|dollars|credit|risk|prescription|generic|drug|money back|money-back|credit card|credit-card)\\b"
    spam_word_found=grepl(spam_words, all_emails_list[[file_num]][[1]]["Subject"])
    return(spam_word_found)
  }
  return(FALSE)
}
subjectSpamWords=sapply(1:length(all_emails_list), subjectSpamWords_funct)

which(subjectSpamWords==TRUE)

#Eleventh Variable: percent of blanks in the subject----------------------------------------------------------------------

percentSubjectBlanks_funct = function(file_num){
  if("Subject" %in% names(all_emails_list[[file_num]][[1]]) &
     gregexpr("[[:space:]]", all_emails_list[[file_num]][[1]]["Subject"])[[1]][1] != -1) #Can also use string count
  {
    num_spaces = length(gregexpr("[[:space:]]", all_emails_list[[file_num]][[1]]["Subject"])[[1]])  
    total_characters = nchar(all_emails_list[[file_num]][[1]]["Subject"], type = "bytes")
    return(num_spaces/total_characters)
  }
  return(0)
}
percentSubjectBlanks=sapply(1:length(all_emails_list), percentSubjectBlanks_funct)


#Twelfth Variable: No hostname----------------------------------------------------------------------
messageIdHasNoHostname_funct = function(file_num){
  if("Message-Id" %in% names(all_emails_list[[file_num]][[1]]) &
     grepl("[^@#]+[^-.@]@[^@#]+.[^@#]+$", all_emails_list[[file_num]][[1]]["Message-Id"])==TRUE) #Or use discussion code
  {
    return(FALSE)
  }else if("Message-ID" %in% names(all_emails_list[[file_num]][[1]]) &
           grepl("[^@#]+[^-.@]@[^@#]+.[^@#]+$", all_emails_list[[file_num]][[1]]["Message-ID"])==TRUE)
    {
    return(FALSE)
    }
  return(TRUE)
}
messageIdHasNoHostname=sapply(1:length(all_emails_list), messageIdHasNoHostname_funct)


#Thirteenth Variable: Is the subject in all capital Letters----------------------------------------------------------------------
#For this variable, I will say that a subject that has 75% or more caps is true
#I count ! and ? because they are related to yelling
isYelling_funct = function(file_num){
  if("Subject" %in% names(all_emails_list[[file_num]][[1]]))
  {
    num_spaces=length(gregexpr("[^][a-z 0-9@#]", all_emails_list[[file_num]][[1]]["Subject"])[[1]])  #"[^a-z()@# ]"
    total_characters = nchar(all_emails_list[[file_num]][[1]]["Subject"], type = "bytes")
    if((num_spaces/total_characters)>.75){
      return(TRUE)
    }
  }
  return(FALSE)
}
isYelling=sapply(1:length(all_emails_list), isYelling_funct)

#Fourteenth Variable: when the user login ends in numbers----------------------------------------------------------------------
fromNumericEnd_funct = function(file_num){
  if("From" %in% names(all_emails_list[[file_num]][[1]]) &
     grepl("^[^@]*[0-9]+@[^@]+", all_emails_list[[file_num]][[1]]["From"]) == TRUE)
  {
    return(TRUE)
  }
  return(FALSE)
}
fromNumericEnd=sapply(1:length(all_emails_list), fromNumericEnd_funct)


#Fifteenth Variable: number of dollar signs in the body of the message----------------------------------------------------------------------
numDollarSigns_funct = function(file_num){
    if(any(str_count(all_emails_list[[file_num]][[3]], "\\$")!=0))
    {
      num_dollar_signs = sum(str_count(all_emails_list[[file_num]][[3]], "\\$"))
      return(num_dollar_signs)
    }
    return(0)
  }
numDollarSigns = sapply(1:length(all_emails_list), numDollarSigns_funct)


#Put all variables in a data frame:
spam_data=data.frame(isSpam, isRe_vect, numLinesInBody, 
           bodyCharacterCount, subjectExclamationCount,
           numAttachments, subjectQuestCount, priority, isInReplyTo, multipartText,
           subjectSpamWords, percentSubjectBlanks, messageIdHasNoHostname,isYelling,
           fromNumericEnd, numDollarSigns)

##------------------------------------------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------------------------------------------
#Plotting and Forming a model:
##-----------------------------------------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------------------------------------------

#Plots:
#For numeric variables, I will plot side-by-side boxplots
#For categorical variables, I will generate a side-by-side bar graph

isSpam_plot = ggplot(spam_data)+
  geom_bar(mapping = aes(x = isSpam, fill=isSpam)) +
                               xlab("Email Type") + 
                                ylab("Number of Emails")+
                                labs(title = "Spam and Ham Emails") 

isRe_vect_plot = ggplot(spam_data) +
                geom_bar(mapping= aes(x = isRe_vect, fill = isSpam),position = "dodge2") +
                scale_x_discrete(labels=c("No Re", "Has Re")) +
                xlab("Re Status") + 
                ylab("Email Count")+
                labs(title = "Re In The Email Subject") 


priority_plot =  ggplot(spam_data) +
                 geom_bar(mapping = aes(x =  priority, fill = isSpam), position = "dodge2") +
                 scale_x_discrete(labels=c("Low Priority", "High Priority")) +
                 xlab("Priority Type") + 
                 ylab("Email Count") +
                 labs(title = "Priority Emails") 


isInReplyTo_plot = ggplot(spam_data) +
                  geom_bar(aes(x = isInReplyTo, fill = isSpam),position="dodge2") +
                  scale_x_discrete(labels=c("No In-Reply-To", "In-Reply-To")) +
                  xlab("In-Reply-To Status") + 
                  ylab("Email Count") +
                  labs(title = "In-Reply-To In Email Header") 


multipartText_plot =  ggplot(spam_data) +
                      geom_bar(aes(x = multipartText, fill = isSpam), position = "dodge2") +
                      scale_x_discrete(labels=c("Multipart not present", "Multipart Present")) +
                      xlab("Multipart Status") + 
                      ylab("Email Count")+
                      labs(title = "Multipart-Text In Email Header") 

subjectSpamWords_plot = ggplot(spam_data) +
                       geom_bar(aes(x = subjectSpamWords, fill = isSpam), position = "dodge2") +
                      scale_x_discrete(labels=c("No Spam Words", "Spam Words")) +
                      xlab("Spam Words Status") + 
                      ylab("Email Count")+
                      labs(title = "Spam Words In The Email Subject") 

messageIdHasNoHostname_plot = ggplot(spam_data) +
                        geom_bar(aes(x = messageIdHasNoHostname, fill = isSpam), position = "dodge2") +
                        scale_x_discrete(labels=c("No Hostname", "Hostname")) +
                        xlab("Hostname Status") + 
                        ylab("Email Count")+
                        labs(title = "No Hostname In The Message ID") 

isYelling_plot = ggplot(spam_data) +
                geom_bar(aes(x = isYelling, fill = isSpam), position = "dodge2") +
                scale_x_discrete(labels=c("No Yelling", "Yelling")) +
                xlab("Yelling Status") + 
                ylab("Email Count")+
                labs(title = "Yelling In The Email Subject") 

fromNumericEnd_plot = ggplot(spam_data) +
                      geom_bar(aes(x = fromNumericEnd, fill = isSpam), position = "dodge2") +
                      scale_x_discrete(labels=c("No Numeric End", "Numeric End")) +
                      xlab("Numeric End Email Adress Status") + 
                      ylab("Email Count")+
                      labs(title = "Email Addesses Ending With Numbers") 

suppressWarnings(ggarrange(isRe_vect_plot, priority_plot, isInReplyTo_plot, multipartText_plot, ncol = 2, nrow=2, labels = c("A", "B", "C", "D")))

suppressWarnings(ggarrange(subjectSpamWords_plot,messageIdHasNoHostname_plot, isYelling_plot, fromNumericEnd_plot,  ncol = 2, nrow=2, labels = c("E", "F","G", "H")))

#Numerical Data:------------------------------------------------------------------------------------

numLinesInBody_plot = ggplot(spam_data, aes(x = isSpam, y=numLinesInBody, fill=isSpam)) +
                      #geom_histogram(aes(y=..density..), alpha=0.5, 
                      #position="identity") +
                      geom_boxplot(alpha=0.4) +
                      ylim(0, 500)+
                      xlab("Email Type") + 
                      ylab("Number of Lines")+
                      labs(title = "Number of Lines In Body") 


bodyCharacterCount_plot = ggplot(spam_data, aes(x = isSpam, y = bodyCharacterCount, fill=isSpam)) +
                          #geom_histogram(aes(y=..density..), alpha=0.5, 
                          #position="identity") +
                          geom_boxplot(alpha=0.4) +
                          ylim(0, 5000)+
                          xlab("Email Type") + 
                          ylab("Number of Characters")+
                          labs(title = "Number of Characters In Body") 


subjectExclamationCount_plot = ggplot(spam_data, aes(x = isSpam, y= subjectExclamationCount, fill=isSpam)) +
                              #geom_histogram(aes(y=..density..), alpha=0.5, 
                              #position="identity") +
                              geom_boxplot(alpha=0.4) +
                              ylim(0, 9)+
                              xlab("Email Type") + 
                              ylab("Number of Exclamations") +
                              labs(title = "Number of Exclamations") 

numAttachments_plot =  ggplot(spam_data, aes(x = isSpam, y = numAttachments, fill=isSpam)) +
                      #geom_histogram(aes(y=..density..), alpha=0.5, 
                      #position="identity") +
                      geom_boxplot(alpha=0.4) +
                      xlab("Email Type") + 
                      ylab("Number of Attachments")+
                      labs(title = "Number of Email Attachments") 

subjectQuestCount_plot =  ggplot(spam_data, aes(x = isSpam, y = subjectQuestCount, fill=isSpam)) +
                          #geom_histogram(aes(y=..density..), alpha=0.5, 
                          #position="identity") +
                          geom_boxplot(alpha=0.4) + 
                          ylim(0, 4)+
                          xlab("Email Type") + 
                          ylab("Question Mark Count")+
                          labs(title = "Number of Question Marks") 

percentSubjectBlanks_plot =  ggplot(spam_data, aes(x = isSpam, y = percentSubjectBlanks, fill=isSpam)) +
                            #geom_histogram(aes(y=..density..), alpha=0.5, 
                            #position="identity") +
                            geom_boxplot(alpha=0.4) +
                            xlab("Email Type") + 
                            ylab("Blank Percentage")+
                            labs(title = "Percentage of Blanks In The Subject") 
  
numDollarSigns_plot = ggplot(spam_data, aes(x = isSpam, y = numDollarSigns, fill=isSpam)) +
                  #geom_histogram(aes(y=..density..), alpha=0.5, 
                  #position="identity") +
                  geom_boxplot(alpha=0.4) +
                  ylim(0, 25) +
                  xlab("Email Type") + 
                  ylab("Number of Dollar Signs")+
                  labs(title = "Number of Dollar Signs In The Email Body") 


suppressWarnings(ggarrange(numLinesInBody_plot, bodyCharacterCount_plot, 
                           subjectExclamationCount_plot, numAttachments_plot,
                           ncol = 2, nrow=2, labels = c("1", "2", "3", "4"))) #Some plots have extreme values

suppressWarnings(ggarrange(subjectQuestCount_plot, percentSubjectBlanks_plot , numDollarSigns_plot, 
                    ncol = 2, nrow=2, labels = c("5", "6", "7")))

#----------------------------------------------------------------------------------------------------------------------------------------------
#For my analysis, I will form a classification tree:
#To form the tree, I will use the rpart function:
#Resource for functions: https://www.statmethods.net/advstats/cart.html
library(rpart)
dir_file= getwd()
spam_decision=invisible(rpart(isSpam~., data=spam_data, method="class"))
#View the results:
summary(spam_decision)
printcp(spam_decision)
#plot the cross validation results:
plotcp(spam_decision)
#Plot the decision tree:
plot(spam_decision, uniform=TRUE, 
     main="Classification Tree for Spam Emails")
text(spam_decision, use.n=TRUE, all=TRUE, cex=.65)

#Forming a full grown tree
spam_decision2 = invisible(rpart(isSpam~., data=spam_data, method="class", cp=-1))
printcp(spam_decision2)
#Prune the tree to prevent overfitting
spam_decision2_pruned = invisible(prune(spam_decision2, cp=spam_decision$cptable[which.min(spam_decision$cptable[,"xerror"]),"CP"]))
printcp(spam_decision2_pruned)

