library(stringr)
library(igraph)

folders<-list.files("maildir/")[1:13]



email_df=data.frame(matrix(ncol=4,nrow=0))


matchBetween <- function(text, pat1, pat2){
  regex <- regexpr(pattern = paste(pat1, "(.*?)", pat2), text = text, perl = T)
  s <- attr(regex, "capture.start")
  e <- s + attr(regex, "capture.length") - 1
  substr(text,  s, e)
}

index=0;
for(folder in folders)
{
  
  email_path<-list.files(paste("maildir/",folder,sep=""),full.names = T,recursive = T);
  
  for(path in email_path )
  {
    email<-read_lines(path);
    # print(email[4])
    # email <- str_replace_all(email, "[\r\n\t]" , " ")
    from <- email[grepl("^From: .*([a-zA-Z0-9.'_-]+@[a-zA-Z0-9.'_-]+).*$", email)];
    from<-substring(from,6);
    # from = matchBetween(email, "From:", "To:")
    # from = trimws(from)
    # print(from)
    # print("ok")
    # to = matchBetween(email, "To:", "Subject:")
    # to = strsplit(to, split = ",")
    # to = lapply(to, trimws)
    # print("ok")
    
    # Fetch 'Subject' field:
    # subject = matchBetween(email, "Subject:", "Cc:")
    # if(nchar(subject) < 2 || grepl("From", subject, fixed = T)){
    #   subject = matchBetween(email, "Subject:", "Mime-Version:")
    # }
    #subject = trimws(subject)
    subject <- email[grepl("^Subject: ", email)];
    subject<-substring(subject,9);
    to <- email[4][grepl("^To: .*([a-zA-Z0-9.'_-]+@[a-zA-Z0-9.'_-]+).*$", email[4])] %>% gsub("To: ", "", .) %>% paste(collapse="")
    #print("Ok")
    #print(to)
    #to<-substring(to,4);
    to<-trimws(to);
    receivers<-unlist(str_split(to,", "));
    print(receivers)
    #print(receivers)
    
    # print("Ok")
    for(receiver in receivers)
    {
      # print(receiver);
      # print("Ok")
      # print(from)
      # print(receiver)
      #print(subject)
      #print(path)
      df<-data.frame(from[1],receiver,subject[1],path);
      # print(df)
      #print("Ok for")
      
    }
    email_df<-rbind(email_df,df);
  }
}

#print(email_df)
colnames(email_df)<-c("From","To","Subject","Path")
write.csv(email_df,'/Users/karimli.reyhana/Documents/BD/email_data8.csv');
print("Okay");


