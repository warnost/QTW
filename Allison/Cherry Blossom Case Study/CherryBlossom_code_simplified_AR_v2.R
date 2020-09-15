
# Created from Will's code
menFiles = menTables
womenFiles = womenTables

# These are the functions used to process the text files into dataframes
convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

findColLocs = function(spacerRow) {
  
  # starting indexes of columns
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) return( c(NA, NA) )
    index = sum(startPos >= searchLocs)
    #below is the change
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, headerRow = headerRow, searchLocs = searchLocs )
}

extractVariables = function(file, varNames =c("name", "home", "ag", "gun",
                                              "net", "time")) {
  
  # Find the index of the row with =s
  eqIndex = grep("^===", file)
  # Extract the two key rows and the data 
  spacerRow = file[eqIndex] 
  headerRow = tolower(file[ eqIndex - 1 ])
  
  # I added this part to correct some issues happening in the women files, but it shouldn't affect the men files
  headerRow = gsub('hometown',
                   'home    ', headerRow)
  headerRow = gsub('gun tim',
                   'gun    ', headerRow)
  headerRow = gsub('net tim',
                   'net    ', headerRow)

  body = file[ -(1 : eqIndex) ]
  # Remove footnotes and blank rows
  footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
  if ( length(footnotes) > 0 ) body = body[ -footnotes ]
  blanks = grep("^[[:blank:]]*$", body)
  if (length(blanks) > 0 ) body = body[ -blanks ]
  
  
  # Obtain the starting and ending positions of variables   
  searchLocs = findColLocs(spacerRow)
  locCols = selectCols(varNames, headerRow, searchLocs)
  
  Values = mapply(substr, list(body), start = locCols[1, ], 
                  stop = locCols[2, ])
  colnames(Values) = varNames
  
  return(Values)
}

createDF = function(Res, year, sex) {
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}


# 2001 has no headers
file=womenFiles$'2001'
# head(file, 15)
# tail(file, 15)

#         "    1  6002 Elana MEYER           34 Rep Of S.africa      52:15   52:16#"
#         " 2964 10022 Cassandra SPEARS      51 Washington DC      2:23:11 2:23:11"
#         "===== ===== ===================== == ================== ======= ======= "
#         "PLACE TOT   NAME                  AG HOMETOWN           GUN TIM NET TIM "

file[2] = "PLACE TOT   NAME                  AG HOMETOWN           GUN TIM NET TIM "
file[3] = "===== ===== ===================== == ================== ======= ======= "

womenFiles$'2001' = file

# Create dataframes
womenResMat = sapply(womenFiles, extractVariables)
womenDF = mapply(createDF, womenResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)



# Check to see if everything worked
lapply(womenDF, head)

# Look at boxplots for age by year
# It appears that age doesn't decrease for women like it does for men over the years
age = sapply(womenDF, 
             function(x) as.numeric(x[ , 'age']))
boxplot(age, ylab = "Age", xlab = "Year")

# Look at boxplot for time by year
runTime = sapply(womenDF, 
             function(x) as.numeric(x[ , 'runTime']))
boxplot(runTime, ylab = "Run Time", xlab = "Year")
