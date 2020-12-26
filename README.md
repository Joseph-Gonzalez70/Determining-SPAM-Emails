# Determining SPAM Emails

The goals for this project are to process the emails and classify the emails.

Most email files are not in tabular form and, therefore, it is difficult to use functions, like read.table, to read the information in a way we can manipulate it. To fix this problem, we use regular expressions to format the emails and generate a data frame with several variables. We use the variables to classify the emails as SPAM or HAM.

This project's main steps are:

• Reading in the email
• Separating the three email components(header, attachment(s), body message)
• Appending the components to a list 
• Repeating process for all files

