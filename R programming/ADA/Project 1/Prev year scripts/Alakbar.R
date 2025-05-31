counter = 1
final_data = vector(mode="list", length = 0)

for (i in data) {
  final_data[counter] = vector(mode="list", length = 3)
  
  found1 = FALSE
  found2 = FALSE
  found3 = FALSE
  
  for (j in i) {
    tempString = toString(j)
    lengthString = nchar(tempString)
    isXFrom = grepl(pattern = "From:", x = tempString) && !grepl(pattern = "X-From:", x = tempString)
    
    if (isXFrom) {
      found1 = TRUE
      final_data[[counter]][1] = substr(x = j, start = 7, stop = lengthString)
    }
    
    isXTo = grepl(pattern = "To:", x = tempString) && !grepl(pattern = "X-To:", x = tempString)
    
    if (isXTo) {
      found2 = TRUE
      final_data[[counter]][2] = substr(x = j, start = 5, stop = lengthString)
    }
    
    isXSubject = grepl(pattern = "Subject:", x = tempString)
    
    if (isXSubject) {
      found3 = TRUE
      final_data[[counter]][3] = substr(x = j, start = 10, stop = lengthString)
    }
    
    if (found1 && found2 && found3) {
      break;
    }
  }
  
  
  if (!found1) {
    final_data[[counter]][1] = "NOT_FOUND"
  }
  
  if (!found2) {
    final_data[[counter]][2] = "NOT_FOUND"
  }
  
  if (!found3) {
    final_data[[counter]][3] = "NOT_FOUND"
  }
  
  counter = counter + 1
}