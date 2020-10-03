# set directory path
library(tm)

setwd("C:/Users/b007224/Documents/masters_in_data_science/quantifying_the_world/QTW/jenna")

# set directory path
spamPath = "./SpamAssassinMessages"

# get list of folders found in ./SpamAssassinMessages/messages
#list.files(path = paste(spamPath, "messages", sep = .Platform$file.sep))

# get the directory names
dirNames = list.files(path = paste(spamPath, "messages", sep = .Platform$file.sep))

# verify number of emails messages
#length(list.files(paste(spamPath, "messages", dirNames, sep = .Platform$file.sep)))

# verify number of email messages in each directory
#sapply(paste(spamPath, "messages", dirNames, sep = .Platform$file.sep), function(dir) length(list.files(dir)) )

# create a list of all spam email file names
fullDirNames = paste(spamPath, "messages", dirNames, sep = .Platform$file.sep)
fileNames = list.files(fullDirNames[1], full.names = TRUE)

# split the message into header/body based on the first blank row in the email
splitMessage = function(msg) {
  # look for the first blank line
  splitPoint = match("", msg)
  # header are rows before the first blank line
  header = msg[1:(splitPoint-1)]
  # body are rows after the first blank line
  body = msg[ -(1:splitPoint) ]
  return(list(header = header, body = body))
}

# get boundary positions for attachment
getBoundary = function(header) {
  boundaryIdx = grep("boundary=", header)
  boundary = gsub('"', "", header[boundaryIdx])
  gsub(".*boundary= *([^;]*);?.*", "\\1", boundary)
}

# search the body for the boundary strings and remove the attachments
dropAttach = function(body, boundary){
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  
  if (length(bStringLocs) <= 1) return(body)
  
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  if (length(eStringLoc) == 0) 
    return(body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)])
  
  n = length(body)
  if (eStringLoc < n) 
    return( body[ c( (bStringLocs[1] + 1) : (bStringLocs[2] - 1), 
                     ( (eStringLoc + 1) : n )) ] )
  
  return( body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1) ])
}

# get list of stop words from tm package
stopWords = stopwords()
# clean the stop words to lower case and remove puncuation
cleanSW = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", stopWords))
SWords = unlist(strsplit(cleanSW, "[[:blank:]]+"))
# remove stop words that are only one character
SWords = SWords[ nchar(SWords) > 1 ]
# remove any duplicate stop words
stopWords = unique(SWords)

# clean up the words in the email messages (remove punctuation, make lower case, etc.)
cleanText =
  function(msg)   {
    tolower(gsub("[[:punct:]0-9[:space:][:blank:]]+", " ", msg))
  }

# get a list of words in the email messages that will be used for classification of spam/not spam
findMsgWords = 
  function(msg, stopWords) {
    if(is.null(msg))
      return(character())
    
    words = unique(unlist(strsplit(cleanText(msg), "[[:blank:]\t]+")))
    
    # drop empty and 1 letter words
    words = words[ nchar(words) > 1]
    # remove stop words
    words = words[ !( words %in% stopWords) ]
    invisible(words)
  }

# get a list of all words in the email messages, removing stop words
processAllWords = function(dirName, stopWords)
{
  # read all files in the directory
  fileNames = list.files(dirName, full.names = TRUE)
  # drop files that are not email, i.e., cmds
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  
  messages = lapply(fileNames, readLines, encoding = "latin1")
  
  # split header and body
  emailSplit = lapply(messages, splitMessage)
  # put body and header in own lists
  bodyList = lapply(emailSplit, function(msg) msg$body)
  headerList = lapply(emailSplit, function(msg) msg$header)
  rm(emailSplit)
  
  # determine which messages have attachments
  # find the Content-Type key to identify the start of an attachment, which should have multipart for content-type
  hasAttach = sapply(headerList, function(header) {
    CTloc = grep("Content-Type", header)
    if (length(CTloc) == 0) return(0)
    multi = grep("multi", tolower(header[CTloc])) 
    if (length(multi) == 0) return(0)
    multi
  })
  
  hasAttach = which(hasAttach > 0)
  
  # find boundary strings for messages with attachments
  boundaries = sapply(headerList[hasAttach], getBoundary)
  
  # drop attachments from message body
  bodyList[hasAttach] = mapply(dropAttach, bodyList[hasAttach], boundaries, SIMPLIFY = FALSE)
  
  # extract words from body
  msgWordsList = lapply(bodyList, findMsgWords, stopWords)
  
  invisible(msgWordsList)
}

msgWordsList = lapply(fullDirNames, processAllWords, stopWords = stopWords) 

# which messages are spam? assign TRUE/FALSE for spam indication
numMsgs = sapply(msgWordsList, length)
isSpam = rep(c(FALSE, FALSE, FALSE, TRUE, TRUE), numMsgs)

# flatten the msgWordsList into one list
msgWordsList = unlist(msgWordsList, recursive = FALSE)




# prep for training/test split
numEmail = length(isSpam)
numSpam = sum(isSpam)
numHam = numEmail - numSpam

# determine indices of test span and not spam messages
set.seed(418910)
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

# use the indices to select the word vectors from msgWordList
testMsgWords = c((msgWordsList[isSpam])[testSpamIdx], (msgWordsList[!isSpam])[testHamIdx] )
trainMsgWords = c((msgWordsList[isSpam])[ - testSpamIdx], (msgWordsList[!isSpam])[ - testHamIdx])

# create test and train datasets
testIsSpam = rep(c(TRUE, FALSE), c(length(testSpamIdx), length(testHamIdx)))
trainIsSpam = rep(c(TRUE, FALSE), c(numSpam - length(testSpamIdx), numHam - length(testHamIdx)))

### probability estimates from training data ###

# create bag of words - complete listing of all words in training dataset
bow = unique(unlist(trainMsgWords))

#length(bow)

# create a vector that holds counts for all words
spamWordCounts = rep(0, length(bow))
names(spamWordCounts) = bow

# retrieve only unique words
tmp = lapply(trainMsgWords[trainIsSpam], unique)
# create a frequency table
tt = table( unlist(tmp) )
spamWordCounts[ names(tt) ] = tt

# estimate probabilities
computeFreqs =
  function(wordsList, spam, bow = unique(unlist(wordsList))) {
    # create a matrix for spam, ham, and log odds
    wordTable = matrix(0.5, nrow = 4, ncol = length(bow), 
                       dimnames = list(c("spam", "ham","presentLogOdds", "absentLogOdds"),  bow))
    
    # For each spam message, add 1 to counts for words in message
    counts.spam = table(unlist(lapply(wordsList[spam], unique)))
    wordTable["spam", names(counts.spam)] = counts.spam + .5
    
    # Similarly for ham messages
    counts.ham = table(unlist(lapply(wordsList[!spam], unique)))  
    wordTable["ham", names(counts.ham)] = counts.ham + .5  
    
    
    # Find the total number of spam and ham
    numSpam = sum(spam)
    numHam = length(spam) - numSpam
    
    # Prob(word|spam) and Prob(word | ham)
    wordTable["spam", ] = wordTable["spam", ]/(numSpam + .5)
    wordTable["ham", ] = wordTable["ham", ]/(numHam + .5)
    
    # log odds
    wordTable["presentLogOdds", ] = log(wordTable["spam",]) - log(wordTable["ham", ])
    wordTable["absentLogOdds", ] = log((1 - wordTable["spam", ])) - log((1 -wordTable["ham", ]))
    
    invisible(wordTable)
  }

# apply computeFreqs function to the training dataset
trainTable = computeFreqs(trainMsgWords, trainIsSpam)

# calculate log likelihood ratio
computeMsgLLR = function(words, freqTable) 
{
  # Discards words not in training data.
  words = words[!is.na(match(words, colnames(freqTable)))]
  
  # Find which words are present
  present = colnames(freqTable) %in% words
  
  # compute the log of the ratio of the probability a message is spam/not spam
  sum(freqTable["presentLogOdds", present]) + sum(freqTable["absentLogOdds", !present])
}

# apply the function to all messages to determine if spam/not spam (positive indicates spam, negative indicates not spam more likely)
testLLR = sapply(testMsgWords, computeMsgLLR, trainTable)

# summary statistics for spam/not spam
#tapply(testLLR, testIsSpam, summary)

# boxplot
#spamLab = c("not spam", "spam")[1 + testIsSpam]
#boxplot(testLLR ~ spamLab, ylab = "Log Likelihood Ratio",ylim=c(-500, 500))

# vectorized method to compute TYpe I errors
typeIErrorRates = function(llrVals, isSpam) {
  o = order(llrVals)
  llrVals =  llrVals[o]
  isSpam = isSpam[o]
  
  idx = which(!isSpam)
  N = length(idx)
  list(error = (N:1)/N, values = llrVals[idx])
  }

# vectorized method to compute TYpe II errors
typeIIErrorRates = function(llrVals, isSpam) {
  o = order(llrVals)
  llrVals =  llrVals[o]
  isSpam = isSpam[o]
  
  idx = which(isSpam)
  N = length(idx)
  list(error = (1:(N))/N, values = llrVals[idx])
  }  

xI = typeIErrorRates(testLLR, testIsSpam)
xII = typeIIErrorRates(testLLR, testIsSpam)
tau01 = round(min(xI$values[xI$error <= 0.01]))
t2 = max(xII$error[ xII$values < tau01 ])

# plot comparing type I and II errors
# with a threshold tau of 42, all messags with an LLR value above -43 are classified as spam
# 1% of not spam is misclassified as spam
# 5% of spam is misclassified as not spam
library(RColorBrewer)
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(xII$error ~ xII$values,  type = "l", col = cols[1], lwd = 3,
     xlim = c(-300, 250), ylim = c(0, 1),
     xlab = "Log Likelihood Ratio Values", ylab="Error Rate")
points(xI$error ~ xI$values, type = "l", col = cols[2], lwd = 3)
legend(x = 50, y = 0.4, fill = c(cols[2], cols[1]),
       legend = c("Classify Ham as Spam", 
                  "Classify Spam as Ham"), cex = 0.8,
       bty = "n")
abline(h=0.01, col ="grey", lwd = 3, lty = 2)
text(-250, 0.05, pos = 4, "Type I Error = 0.01", col = cols[2])

mtext(tau01, side = 1, line = 0.5, at = tau01, col = cols[3])
segments(x0 = tau01, y0 = -.50, x1 = tau01, y1 = t2, 
         lwd = 2, col = "grey")
text(tau01 + 20, 0.05, pos = 4,
     paste("Type II Error = ", round(t2, digits = 2)), 
     col = cols[1])


# 5 fold cross validation
k = 5
numTrain = length(trainMsgWords)
partK = sample(numTrain)
tot = k * floor(numTrain/k)
# separate into 5 folds
partK = matrix(partK[1:tot], ncol = k)

# use 5 fold cross validation to classify spam/not spam
testFoldOdds = NULL
for (i in 1:k) {
  foldIdx = partK[ , i]
  trainTabFold = computeFreqs(trainMsgWords[-foldIdx], trainIsSpam[-foldIdx])
  testFoldOdds = c(testFoldOdds, sapply(trainMsgWords[ foldIdx ], computeMsgLLR, trainTabFold))
}

# identify if the message was spam/not spam
testFoldSpam = NULL
for (i in 1:k) {
  foldIdx = partK[ , i]
  testFoldSpam = c(testFoldSpam, trainIsSpam[foldIdx])
}

# calculate error rates, tau = -51
xFoldI = typeIErrorRates(testFoldOdds, testFoldSpam)
xFoldII = typeIIErrorRates(testFoldOdds, testFoldSpam)
tauFoldI = round(min(xFoldI$values[xFoldI$error <= 0.01]))
tFold2 = xFoldII$error[ xFoldII$values < tauFoldI ]


### data prep for recursive partitioning ###

# process the header into usable arguments
processHeader = function(header) {
  # modify the first line to create a key:value pair
  header[1] = sub("^From", "Top-From:", header[1])
  
  # reads data in the format key: value and accounts for continuation on subsequent lines
  headerMat = read.dcf(textConnection(header), all = TRUE)
  # convert to character vector with key as the name for each value
  headerVec = unlist(headerMat)
  
  dupKeys = sapply(headerMat, function(x) length(unlist(x)))
  names(headerVec) = rep(colnames(headerMat), dupKeys)
  
  return(headerVec)
  }

# process the attachment
processAttach = function(body, contentType){
  
  n = length(body)
  boundary = getBoundary(contentType)
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  
  if (length(eStringLoc) == 0) eStringLoc = n
  if (length(bStringLocs) <= 1) {
    attachLocs = NULL
    msgLastLine = n
    if (length(bStringLocs) == 0) bStringLocs = 0
  } else {
    attachLocs = c(bStringLocs[ -1 ],  eStringLoc)
    msgLastLine = bStringLocs[2] - 1
  }
  
  msg = body[ (bStringLocs[1] + 1) : msgLastLine] 
  if ( eStringLoc < n )
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  if ( !is.null(attachLocs) ) {
    attachLens = diff(attachLocs, lag = 1) 
    attachTypes = mapply(function(begL, endL) {
      CTloc = grep("^[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      if ( length(CTloc) == 0 ) {
        MIMEType = NA
      } else {
        CTval = body[ begL + CTloc[1] ]
        CTval = gsub('"', "", CTval )
        MIMEType = sub(" *[Cc]ontent-[Tt]ype: *([^;]*);?.*", "\\1", CTval)   
      }
      return(MIMEType)
    }, attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  if (is.null(attachLocs)) return(list(body = msg, attachDF = NULL) )
  return(list(body = msg, 
              attachDF = data.frame(aLen = attachLens, 
                                    aType = unlist(attachTypes),
                                    stringsAsFactors = FALSE)))   
  
  }                       

readEmail = function(dirName) {
  # retrieve the names of files in directory
  fileNames = list.files(dirName, full.names = TRUE)
  # drop files that are not email
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  
  # read all files in the directory
  lapply(fileNames, readLines, encoding = "latin1")
}


# process all emails, including headers and attachments
processAllEmail = function(dirName, isSpam = FALSE)
{
  # read all files in the directory
  messages = readEmail(dirName)
  fileNames = names(messages)
  n = length(messages)
  
  # split header from body
  eSplit = lapply(messages, splitMessage)
  rm(messages)
  
  # process header as named character vector
  headerList = lapply(eSplit, function(msg) 
    processHeader(msg$header))
  
  # extract content-type key
  contentTypes = sapply(headerList, function(header) 
    header["Content-Type"])
  
  # extract the body
  bodyList = lapply(eSplit, function(msg) msg$body)
  rm(eSplit)
  
  # which email have attachments
  hasAttach = grep("^ *multi", tolower(contentTypes))
  
  # get summary stats for attachments and the shorter body
  attList = mapply(processAttach, bodyList[hasAttach], 
                   contentTypes[hasAttach], SIMPLIFY = FALSE)
  
  bodyList[hasAttach] = lapply(attList, function(attEl) 
    attEl$body)
  
  attachInfo = vector("list", length = n )
  attachInfo[ hasAttach ] = lapply(attList, 
                                   function(attEl) attEl$attachDF)
  
  # prepare return structure
  emailList = mapply(function(header, body, attach, isSpam) {
    list(isSpam = isSpam, header = header, 
         body = body, attach = attach)
  },
  headerList, bodyList, attachInfo, 
  rep(isSpam, n), SIMPLIFY = FALSE )
  names(emailList) = fileNames
  
  invisible(emailList)
}

# apply the over-arching function to all emails
emailStruct = mapply(processAllEmail, fullDirNames, isSpam = rep( c(FALSE, TRUE), 3:2))      
emailStruct = unlist(emailStruct, recursive = FALSE)

# save the processed email file
save(emailStruct, file="emailXX.rda")

# create a derived function that allows the code to remain virtually unchanged when adding/removing functions
createDerivedDF = function(email = emailStruct, operations = funcList, verbose = FALSE) {
  els = lapply(names(operations), function(id) {
    if(verbose) print(id)
    e = operations[[id]]
    v = if(is.function(e)) 
      sapply(email, e)
    else 
      sapply(email, function(msg) eval(e))
    v
    })
    
  df = as.data.frame(els)
  names(df) = names(operations)
  invisible(df)
  }



# create list of functions that can be applied to emails
funcList = list(
  isSpam =
    expression(msg$isSpam)
  ,
  
  # is the message a response from an earlier message
  isRe =
    function(msg) {
      # Can have a Fwd: Re:  ... but we are not looking for this here.
      # We may want to look at In-Reply-To field.
      "Subject" %in% names(msg$header) && 
        length(grep("^[ \t]*Re:", msg$header[["Subject"]])) > 0
    }
  ,
  numLines =
    function(msg) length(msg$body)
  ,
  bodyCharCt =
    function(msg)
      sum(nchar(msg$body))
  ,
  underscore =
    function(msg) {
      if(!"Reply-To" %in% names(msg$header))
        return(FALSE)
      
      txt <- msg$header[["Reply-To"]]
      length(grep("_", txt)) > 0  && 
        length(grep("[0-9A-Za-z]+", txt)) > 0
    }
  ,
  subExcCt = 
    function(msg) {
      x = msg$header["Subject"]
      if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
        return(NA)
      
      sum(nchar(gsub("[^!]","", x)))
    }
  ,
  subQuesCt =
    function(msg) {
      x = msg$header["Subject"]
      if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
        return(NA)
      
      sum(nchar(gsub("[^?]","", x)))
    }
  ,
  numAtt = 
    function(msg) {
      if (is.null(msg$attach)) return(0)
      else nrow(msg$attach)
    }
  
  ,
  priority =
    function(msg) {
      ans <- FALSE
      # Look for names X-Priority, Priority, X-Msmail-Priority
      # Look for high any where in the value
      ind = grep("priority", tolower(names(msg$header)))
      if (length(ind) > 0)  {
        ans <- length(grep("high", tolower(msg$header[ind]))) >0
      }
      ans
    }
  ,
  numRec =
    function(msg) {
      # unique or not.
      els = getMessageRecipients(msg$header)
      
      if(length(els) == 0)
        return(NA)
      
      # Split each line by ","  and in each of these elements, look for
      # the @ sign. This handles
      tmp = sapply(strsplit(els, ","), function(x) grep("@", x))
      sum(sapply(tmp, length))
    }
  ,
  perCaps =
    function(msg)
    {
      body = paste(msg$body, collapse = "")
      
      # Return NA if the body of the message is "empty"
      if(length(body) == 0 || nchar(body) == 0) return(NA)
      
      # Eliminate non-alpha characters and empty lines 
      body = gsub("[^[:alpha:]]", "", body)
      els = unlist(strsplit(body, ""))
      ctCap = sum(els %in% LETTERS)
      100 * ctCap / length(els)
    }
  ,
  isInReplyTo =
    function(msg)
    {
      "In-Reply-To" %in% names(msg$header)
    }
  ,
  sortedRec =
    function(msg)
    {
      ids = getMessageRecipients(msg$header)
      all(sort(ids) == ids)
    }
  ,
  subPunc =
    function(msg)
    {
      if("Subject" %in% names(msg$header)) {
        el = gsub("['/.:@-]", "", msg$header["Subject"])
        length(grep("[A-Za-z][[:punct:]]+[A-Za-z]", el)) > 0
      }
      else
        FALSE
    },
  hour =
    function(msg)
    {
      date = msg$header["Date"]
      if ( is.null(date) ) return(NA)
      # Need to handle that there may be only one digit in the hour
      locate = regexpr("[0-2]?[0-9]:[0-5][0-9]:[0-5][0-9]", date)
      
      if (locate < 0)
        locate = regexpr("[0-2]?[0-9]:[0-5][0-9]", date)
      if (locate < 0) return(NA)
      
      hour = substring(date, locate, locate+1)
      hour = as.numeric(gsub(":", "", hour))
      
      locate = regexpr("PM", date)
      if (locate > 0) hour = hour + 12
      
      locate = regexpr("[+-][0-2][0-9]00", date)
      if (locate < 0) offset = 0
      else offset = as.numeric(substring(date, locate, locate + 2))
      (hour - offset) %% 24
    }
  ,
  multipartText =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      numAtt = nrow(msg$attach)
      
      types = 
        length(grep("(html|plain|text)", msg$attach$aType)) > (numAtt/2)
    }
  ,
  hasImages =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      
      length(grep("^ *image", tolower(msg$attach$aType))) > 0
    }
  ,
  isPGPsigned =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      
      length(grep("pgp", tolower(msg$attach$aType))) > 0
    },
  perHTML =
    function(msg)
    {
      if(! ("Content-Type" %in% names(msg$header))) return(0)
      
      el = tolower(msg$header["Content-Type"]) 
      if (length(grep("html", el)) == 0) return(0)
      
      els = gsub("[[:space:]]", "", msg$body)
      totchar = sum(nchar(els))
      totplain = sum(nchar(gsub("<[^<]+>", "", els )))
      100 * (totchar - totplain)/totchar
    },
  subSpamWords =
    function(msg)
    {
      if("Subject" %in% names(msg$header))
        length(grep(paste(SpamCheckWords, collapse = "|"), 
                    tolower(msg$header["Subject"]))) > 0
      else
        NA
    }
  ,
  subBlanks =
    function(msg)
    {
      if("Subject" %in% names(msg$header)) {
        x = msg$header["Subject"]
        # should we count blank subject line as 0 or 1 or NA?
        if (nchar(x) == 1) return(0)
        else 100 *(1 - (nchar(gsub("[[:blank:]]", "", x))/nchar(x)))
      } else NA
    }
  ,
  noHost =
    function(msg)
    {
      # Or use partial matching.
      idx = pmatch("Message-", names(msg$header))
      
      if(is.na(idx)) return(NA)
      
      tmp = msg$header[idx]
      return(length(grep(".*@[^[:space:]]+", tmp)) ==  0)
    }
  ,
  numEnd =
    function(msg)
    {
      # If we just do a grep("[0-9]@",  )
      # we get matches on messages that have a From something like
      # " \"marty66@aol.com\" <synjan@ecis.com>"
      # and the marty66 is the "user's name" not the login
      # So we can be more precise if we want.
      x = names(msg$header)
      if ( !( "From" %in% x) ) return(NA)
      login = gsub("^.*<", "", msg$header["From"])
      if ( is.null(login) ) 
        login = gsub("^.*<", "", msg$header["X-From"])
      if ( is.null(login) ) return(NA)
      login = strsplit(login, "@")[[1]][1]
      length(grep("[0-9]+$", login)) > 0
    },
  
  # identifies if all alpha characters in the subject line are upper case
  isYelling =
    function(msg)
    {
      if ( "Subject" %in% names(msg$header) ) {
        el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
        if (nchar(el) > 0) nchar(gsub("[A-Z]", "", el)) < 1
        else FALSE
      }
      else
        NA
    },
  forwards =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0 || sum(nchar(x)) == 0)
        return(NA)
      
      ans = length(grep("^[[:space:]]*>", x))
      100 * ans / length(x)
    },
  isOrigMsg =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("^[^[:alpha:]]*original[^[:alpha:]]+message[^[:alpha:]]*$", 
                  tolower(x) ) ) > 0
    },
  isDear =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("^[[:blank:]]*dear +(sir|madam)\\>", 
                  tolower(x))) > 0
    },
  isWrote =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("(wrote|schrieb|ecrit|escribe):", tolower(x) )) > 0
    },
  avgWordLen =
    function(msg)
    {
      txt = paste(msg$body, collapse = " ")
      if(length(txt) == 0 || sum(nchar(txt)) == 0) return(0)
      
      txt = gsub("[^[:alpha:]]", " ", txt)
      words = unlist(strsplit(txt, "[[:blank:]]+"))
      wordLens = nchar(words)
      mean(wordLens[ wordLens > 0 ])
    }
  ,
  numDlr =
    function(msg)
    {
      x = paste(msg$body, collapse = "")
      if(length(x) == 0 || sum(nchar(x)) == 0)
        return(NA)
      
      nchar(gsub("[^$]","", x))
    }
)

# spam words
SpamCheckWords = c("viagra", "pounds", "free", "weight", "guarantee", "million", "dollars", "credit", "risk", "prescription", "generic", "drug",
                   "financial", "save", "dollar", "erotic", "million", "barrister", "beneficiary", "easy", "money back", "money", "credit card")


getMessageRecipients = function(header) {
  c(if("To" %in% names(header))  header[["To"]] else character(0),
    if("Cc" %in% names(header))  header[["Cc"]] else character(0),
    if("Bcc" %in% names(header)) header[["Bcc"]] else character(0)
    )
  }

# add in additional derived variables
emailDF = createDerivedDF(emailStruct)

### exploring email feature set ###

# boxplot for percent of body that is capitalized
# shows that about 75% of the not spam emails have values below the lower quartile for spam
# this variable may be useful for classification
percent = emailDF$perCaps
isSpamLabs = factor(emailDF$isSpam, labels = c("not spam", "spam"))
#boxplot(log(1 + percent) ~ isSpamLabs, ylab = "Percent Capitals (log)")

# QQ plot for percent of body that is capitalized
# spam/not spam distributions have roughly the same shape
# spam messages have a larger average number of capital letters and a greater spread than non-spam messages
logPerCapsSpam = log(1 + emailDF$perCaps[ emailDF$isSpam ])
logPerCapsHam = log(1 + emailDF$perCaps[ !emailDF$isSpam ])
#qqplot(logPerCapsSpam, logPerCapsHam, xlab = "Regular Email", ylab = "Spam Email", main = "Percentage of Capital Letters (log scale)", pch = 19, cex = 0.3)

# compare the joint distribution of the percentage of capital letters in the email and the total number of characters in the body
# spam is purple, not spam is green
# spam tends to be longer and have more capitalization that not spam
colI = c("#4DAF4A80", "#984EA380")
logBodyCharCt = log(1 + emailDF$bodyCharCt)
logPerCaps = log(1 + emailDF$perCaps)
#plot(logPerCaps ~ logBodyCharCt, xlab = "Total Characters (log)",ylab = "Percent Capitals (log)", col = colI[1 + emailDF$isSpam],xlim = c(2,12), pch = 19, cex = 0.5)

# there is little difference between spam/not spam when looking at the counts of email attachments (most emails have no attachments)
table(emailDF$numAtt, isSpamLabs)

# look at whether there is RE: and if there is a # at the end of the sender's email address
# spam messages are less likely to contain RE: but more likely to have a number at the end of the sender's email address
#oldPar = par(mfrow = c(1, 2), mar = c(1,1,1,1))
#colM = c("#E41A1C80", "#377EB880")
#isRe = factor(emailDF$isRe, labels = c("no Re:", "Re:"))
#mosaicplot(table(isSpamLabs, isRe), main = "",xlab = "", ylab = "", color = colM)
#fromNE = factor(emailDF$numEnd, labels = c("No #", "#"))
#mosaicplot(table(isSpamLabs, fromNE), color = colM, main = "", xlab="", ylab = "")
#par(oldPar)


### recursive partitioning ###
library(rpart)

# variables must all be either factors or numeric
# need to convert logicals to factors
setupRpart = function(data) {
  logicalVars = which(sapply(data, is.logical))
  facVars = lapply(data[ , logicalVars], function(x) {
    x = as.factor(x)
    levels(x) = c("F", "T")
    x
    })
  cbind(facVars, data[ , - logicalVars])
  }

emailDFrp = setupRpart(emailDF)

# training/test split 70%/30%
set.seed(418910)
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

testDF = rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ], emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
trainDF = rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])

# fit the classification tree
rpartFit = rpart(isSpam ~ ., data = trainDF, method = "class")

# plot tree
library(rpart.plot)
prp(rpartFit, extra = 1)

# predict on the test df
predictions = predict(rpartFit, newdata = testDF[, names(testDF) != "isSpam"], type = "class")

# type I error where not spam is missclassified as spam, 0.65
predsForHam = predictions[ testDF$isSpam == "F" ]
summary(predsForHam)
sum(predsForHam == "T") / length(predsForHam)

# type II error, 0.19
predsForSpam = predictions[ testDF$isSpam == "T" ]
sum(predsForSpam == "F") / length(predsForSpam)




# let's explore the complexity parameter
# used as a threshold where any split that does not decrease the overall lack of fit by cp is not considered
complexityVals = c(seq(0.00001, 0.0001, length=19), seq(0.0001, 0.001, length=19), seq(0.001, 0.005, length=9), seq(0.005, 0.01, length=9))

# call rpart() with each of these values for cp
# train and predict
fits = lapply(complexityVals, function(x) {
  rpartObj = rpart(isSpam ~ ., data = trainDF, method="class", control = rpart.control(cp=x) )
  predict(rpartObj, newdata = testDF[ , names(testDF) != "isSpam"], type = "class")
  })

# calculate type I and II errors
spam = testDF$isSpam == "T"
numSpam = sum(spam)
numHam = sum(!spam)
errs = sapply(fits, function(preds) {
  typeI = sum(preds[ !spam ] == "T") / numHam
  typeII = sum(preds[ spam ] == "F") / numSpam
  c(typeI = typeI, typeII = typeII)
  })

# plot errors
library(RColorBrewer)
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(errs[1,] ~ complexityVals, type="l", col=cols[2], lwd = 2, ylim = c(0,0.2), xlim = c(0,0.01), ylab="Error", xlab="complexity parameter values")
points(errs[2,] ~ complexityVals, type="l", col=cols[1], lwd = 2)

text(x =c(0.003, 0.0035), y = c(0.12, 0.05), labels=c("Type II Error", "Type I Error"))

minI = which(errs[1,] == min(errs[1,]))[1]
abline(v = complexityVals[minI], col ="grey", lty =3, lwd=2)

text(0.0007, errs[1, minI]+0.01, formatC(errs[1, minI], digits = 2))
text(0.0007, errs[2, minI]+0.01, formatC(errs[2, minI], digits = 3))

save(emailDFrp,file="data.Rda")






################
# pick up here #
################
library(caret)

# Ok so first of all our data is in T/F 'factors'.  
# We need to change it to numbers.  And as it turns out, there are quite a few NANs as well.  Let's set those to zero.
setupRnum = function(data) {
  logicalVars = which(sapply(data, is.logical))
  facVars = lapply(data[ , logicalVars], function(x) {
    x = as.numeric(x)
    })
  cbind(facVars, data[ , - logicalVars])
  }

emailDFnum = setupRnum(emailDF)

emailDFnum[is.na(emailDFnum)]<-0


# Because our authors prefer Type I/II errors, but the cool kids know that precision/recall/F1 is where its at, while the default of caret is accuracy and kappa.  
# To get us all on the same page, I create a function that returns the metrics we want.  
# However, rather than re-invent the wheel, I just install a package.  I am not sure if it had Type I/II errors so those I made my self.

library(MLmetrics)
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  p <- Precision(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  r <- Recall(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  fp <-sum(data$pred==0 & data$obs==1)/length(data$pred)  
  
  fn <-sum(data$pred==1 & data$obs==0)/length(data$pred)
  c(F1 = f1_val,
    prec = p,
    rec = r,
    Type_I_err=fp,
    Type_II_err=fn)
  }

library(naivebayes)
library(e1071)
library(rpart)

# make a dataframe of all the parameters to check
# https://topepo.github.io/caret/available-models.html
#nb_grid<-expand.grid(laplace=c(0,0.1,0.3,0.5,1), usekernel=c(T,F), adjust=c(T,F))

# Then we create a trainControl object.  It tells caret how to train--using a cross-validation ('cv') with 3 folds in this case (number = 3).  
# We want the final predictions of the best model and our summary is the custom function from above.
#train_control<-trainControl(method="cv", number=3, savePredictions = 'final',summaryFunction = f1)

# Then we create our model: "model_nb".  We use the caret::train method.  We make 'isSpam' a factor because R is dumb and can't figure out that 1 and 0 are classes.  
#model_nb<-caret::train(as.factor(isSpam) ~ .,data=emailDFnum, trControl = train_control, method='naive_bayes',tuneGrid = nb_grid, na.action = na.omit)
#model_nb

#Did the boss fool us with the folds?  Nope.
#table(model_nb$pred['Resample'])


### rpart ###
results_combined <- data.frame(minsplit=double(),
                               maxdepth=double(),
                               cp=double(),
                               F1=double(),
                               prec=double(),
                               rec=double(),
                               Type_I_err=double(),
                               Type_II_err=double(),
                               stringsAsFactors = FALSE)

control_grid <- expand.grid(minsplit=seq(5,25,1),
                            maxdepth=seq(15,30,1))

cart_grid<-expand.grid(cp = seq(from = 0, to=0.01, by=0.0005))

train_control<-trainControl(method="cv", number =5, savePredictions = 'final',summaryFunction = f1)

for(i in 1:nrow(control_grid)) {
  set.seed(1234)
  model_rpart<-caret::train(as.factor(isSpam) ~ .,data=emailDFnum, trControl = train_control, method='rpart',
                            control=rpart.control(minsplit=control_grid$minsplit[i],
                                                  maxdepth=control_grid$maxdepth[i]
                                                  ),
                            tuneGrid = cart_grid, na.action = na.omit)

  results=as.data.frame(model_rpart$results)
  results1=results[which(results$F1==max(results$F1)),]
  
  results_combined[i,"minsplit"] = control_grid$minsplit[i]
  results_combined[i,"maxdepth"] = control_grid$maxdepth[i]
  results_combined[i,"cp"] = results1$cp
  results_combined[i,"F1"] = results1$F1
  results_combined[i,"prec"] = results1$prec
  results_combined[i,"rec"] = results1$rec
  results_combined[i,"Type_I_err"] = results1$Type_I_err
  results_combined[i,"Type_II_err"] = results1$Type_II_err
}


#model_rpart
#plot(model_rpart)

library(rattle)
fancyRpartPlot(model_rpart$finalModel)




