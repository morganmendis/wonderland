# (i) Create a function which calculates the missing values ratio
missing.ratio <-function(x){
  r <- sum(is.na(x))/length(x)
  return(r)
}

# (ii) Derive continuous columns from categorical where possible & meaningful
fn_ConvertCategorical <- function(df){
  #(ii.01) Replace NAs
  df[is.na(df)] <- 0
  
  #(ii.02) Create lists of column names with particular charcteristics
  colsYN = list("COLLECT1", "VETERANS", "BIBLE", "CATLG", "HOMEE", "PETS", "CDPLAY", "STEREO", "PCOWNERS", "PHOTO", "CRAFTS", "FISHER", "GARDENIN", "BOATS", "WALKER", "KIDSTUFF", "CARDS", "PLATES")
  colsGender = list("CHILD03", "CHILD07", "CHILD12", "CHILD18", "GENDER")
  colsBinary = list("HOMEOWNR", "AGEFLAG")
  
  #(i.03) Loop through YN, Gender & Binary indicator columns, encoding into numerical
  for(col in colsYN){
    df[,col] <- ifelse(df[,col]=="y",1,0)
  }
  for(col in colsGender){
    df[df[,col]==" ",col] <- 0    #not provided
    df[df[,col]=="M",col] <- .20    #male
    df[df[,col]=="U",col] <- .40    #unknown
    df[df[,col]=="F",col] <- .60    #female
    df[df[,col]=="J",col] <- .80    #joint account, unknown gender
    df[df[,col]=="B",col] <- 1.0    #both (m & f - applicable to children fields)
  }
  for(col in colsBinary){
    df[(df[,col]=="I") | (df[,col]=="U"),col] <- 0    #Inferred from Date of Birth Field (Age) or Unkown (Homeowner)
    df[df[,col]==" ",col] <- 0    #Inferred from Date of Birth Field (Age) or Unkown (Homeowner)
    df[(df[,col]=="E") | (df[,col]=="H"),col] <- 1    #Exact (Age) or Home Owner
  }
  
  #(ii.04) Generate new ID columnsfrom DOMAIN, PEPSTRFL & Major Donor Matrix columns
  df[substring(df[,"DOMAIN"], 1, 1) == "X", "Nbrhd_Urban"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "U", "Nbrhd_Urban"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "U", "Nbrhd_Urban"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "C", "Nbrhd_City"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "C", "Nbrhd_City"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "S", "Nbrhd_Suburban"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "S", "Nbrhd_Suburban"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "T", "Nbrhd_Town"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "T", "Nbrhd_Town"] <- 0
  df[substring(df[,"DOMAIN"], 1, 1) == "R", "Nbrhd_Rural"] <- 1
  df[substring(df[,"DOMAIN"], 1, 1) != "R", "Nbrhd_Rural"] <- 0
  df[df[,"PEPSTRFL"] == "X","PEPSTRFL"] <- 1  #has PEP star RFA status
  
  df[, "Nbrhd_SocioEconStts"] <- substring(df[,"DOMAIN"], 2, 2)
  
  #Recency of giving for Major Donor Matrix
  df[substring(df[,"MDMAUD"], 1, 1) == "C", "MajDnrMtx_Crrnt"] <- 1
  df[substring(df[,"MDMAUD"], 1, 1) == "L", "MajDnrMtx_Lpsd"] <- 1
  df[substring(df[,"MDMAUD"], 1, 1) == "I", "MajDnrMtx_Inact"] <- 1
  df[substring(df[,"MDMAUD"], 1, 1) == "D", "MajDnrMtx_Drmnt"] <- 1
  df[is.na(df)] <- 0
  #Frequency of giving for Major Donor Matrix
  df[df[,"MDMAUD"], "MajDnrMtx_Freq"] <- substring(df[,"MDMAUD"], 2, 2)
  
  #Clean up memory
  rm(colsYN, colsBinary, colsGender)
  return(df)
}

# (iii) Ensure columns intended to be numeric are actually numeric
fn_correctNumerics  <- function(df, colsChar) {
  forConversion <- c("MajDnrMtx_Crrnt", "MajDnrMtx_Lpsd", "MajDnrMtx_Inact","MajDnrMtx_Drmnt", "Nbrhd_Urban", "Nbrhd_City", "Nbrhd_Suburban", "Nbrhd_Town", "Nbrhd_Rural", "COLLECT1", "VETERANS", "BIBLE", "CATLG", "HOMEE", "PETS", "CDPLAY", "STEREO", "PCOWNERS", "PHOTO", "CRAFTS", "FISHER", "GARDENIN", "BOATS", "WALKER", "KIDSTUFF", "CARDS", "PLATES", "PEPSTRFL", "GENDER", "AGEFLAG", "HOMEOWNR", "CHILD03", "CHILD07", "CHILD12", "CHILD18", "NOEXCH", "ZIP")
  for(col in forConversion){
    df[, col] <- as.numeric(df[, col])
  }
  rm(forConversion, col)
  return(df)
}

# (iv) Identify character & date columns for later processing
fn_DropCharacter <- function(df){
  #(iv.01) FindCreate list of all character columns
  colsChar <- list()
  for(col in colnames(df)){
    if(class(df[,col]) == "character"){
      colsChar <- c(colsChar, col)
    }
  }
  
  #(iv.02) FindCreate list of all date columns
  colsDate <- list()
  for(col in colnames(df)){
    if(class(df[,col]) == "date" | class(df[,col])=="datetime"){
      colsDate <- c(colsDate, col)
    }
  }
  rm(col)
  result <- c(colsChar, colsDate)
  return(result)
}
