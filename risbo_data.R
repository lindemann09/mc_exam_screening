# reading risbo zip file, Oliver Lindemann

.version_risbo_data = "0.3.5"

library(tidyverse)
library(stringi)
# requires also zip (do not import zip) 

check_risbo_file <- function(zipfile, warnings=FALSE) {
  # checks is file is a risbo file in good shape 
  # needs to be a zip that includes one instance of the following files:
  #    *.sta.txt, *.lsr.txt, *.frm.ini
  #
  # returns list with relevant files name in zip or NULL
  
  fls = NULL
  try(fls <- unzip(zipfile, list=TRUE), 
      silent = TRUE)
  
  if (is.null(fls)) { 
    if (warnings) warning(paste(zipfile, ": Can't open zip file"))
    return(NULL)
  
  } else {
    sta_file <- filter(fls, str_ends(Name,  "-sta.txt"))
    lsr_file <- filter(fls, str_ends(Name,  "-lsr.txt"))
    ini_file <- filter(fls, str_ends(Name,  "-frm.ini"))
    if (nrow(sta_file)!=1 | nrow(lsr_file)!=1 | nrow(ini_file)!=1) {
      if (warnings) 
        warning(paste0(zipfile, ": File not in good shape."))
      return(NULL)
    } else
      return(list(sta_file=sta_file$Name, 
                  lsr_file=lsr_file$Name,
                  ini_file=ini_file$Name))
  }
}


read_risbo_exam_info <- function(zipfile) {
  rtn <- list()
  rtn$n_options = NULL
  
  fls <- zip::zip_list(zipfile) # do not import zip
  ini_file <- filter(fls, str_ends(filename,  "-frm.ini"))
  if (nrow(ini_file)!=1) {
    stop(paste0(zipfile, "is not a correct risbo file. No *-frm.ini file"))
  }

  tags = c("ProcessingNumber", "CourseCode", "ProfName", "ExamName","ExamDate",
           "ExamType", "NumQuestion", "NumGroup")

  fl = unz(zipfile, ini_file$filename)
  lines <- readLines(fl)
  for (x in str_split(lines, "=")) {
    if (sum(x[1] == tags)>0) {
      cmd <- stri_unescape_unicode(paste0("rtn$", x[1], "<-'", x[2], "'"))
      eval(parse(text=cmd ))
    } else if (is.null(rtn$n_options) & x[1] == "Question") {
       # assuming n_options is the same for all questions in this test
      rtn$n_options = as.numeric(unlist(str_split(x[2], ":"))[3])
    }
  }
  close(fl)
  
  rtn$file = ini_file$filename
  rtn$timestamp = ini_file$timestamp
  rtn$NumQuestion = as.numeric(rtn$NumQuestion)
  rtn$NumGroup = as.numeric(rtn$NumGroup)
  rtn$ProcessingNumber = as.numeric(rtn$ProcessingNumber)
  
  return(rtn)
}


read_risbo_mcexam <- function(zipfile, skip_item_profiles=FALSE) {
  # read content of risbo file

  fls = check_risbo_file(zipfile)
  if (is.null(fls)) {
    stop(paste0(zipfile, "is not a correct risbo file"))
  }
  info = read_risbo_exam_info(zipfile)
  rtn <- list(processing_number = as.numeric(info$ProcessingNumber),
              info = info, 
              grades = read_risbo_grades(zipfile, fls$lsr_file))

  if (!skip_item_profiles) 
    rtn <- append(rtn, read_risbo_item_profile(zipfile, fls$sta_file))
  
  class(rtn) <- "RisboMCExam"
  return(rtn)
}


risbo_folder_overview <- function(data_folder) {
  
  files = list.files(path=data_folder, pattern=".zip")
  files = lapply(files, function(x) {file.path(data_folder, x)} )
  df = data.frame()
  broken_files = c()
  for (x in files) {
    fls = check_risbo_file(x, warnings=TRUE)
    if (!is.null(fls)) {
      info = data.frame(read_risbo_exam_info(x))
      
      info$file = x
      df = rbind(df, info)
    } else {
      broken_files = append(broken_files, x)
    }
  }
  print(broken_files)
  df = df %>%
        arrange(CourseCode, ExamDate) %>%
        mutate(id = c(1:n()))
  return(df)
}


read_risbo_grades <- function(zipfile, lsr_file) {
  fl = unz(zipfile, lsr_file)
  rtn <- read_delim(fl, delim="\t", col_names = FALSE,
                   skip=1, trim_ws = TRUE, na=c("NO"),
                   col_types = c(col_integer(),col_double()))
  names(rtn) <- c("student_id", "grade")
  return(rtn)
}


read_risbo_item_profile <- function(zipfile, sta_file) {
  question_id = NA
  tmp_table = NA
  item_profile = tibble() # --> item_profile
  percentages = tibble()
  M_values = tibble()
  restcorr = tibble()
  lcnt = 0
  
  fl = unz(zipfile, sta_file)
  for (l in readLines(fl)) {
    if (str_starts(l, "Vraag")) {
      x = str_split(l, " ")[[1]]
      if (length(x)==2) {
          question_id = as.numeric(x[2])
          tmp_table = data.frame()
          lcnt = 0
      } 
    } else if (!is.na(question_id)) { 
      lcnt = lcnt + 1
      if (lcnt == 4) { # varnames
        varnames = .parse_columns(l, skip_chars = 55)
        is_true_false = (varnames[length(varnames)] == "score")
        correct = which(str_locate(varnames, "[*]")[,1]>0) -1
        n_correct = length(correct)
        varnames = str_remove_all(varnames, "[*]")
        if (n_correct>2) {
          message(paste0("WARNING: More than 2 options are correct in question ", question_id, " in ", zipfile))
        }
      
        } else if (lcnt>=6 & lcnt<= 10) { 
        # table item_profile
        d = .parse_columns(l, skip_chars = 55, as_numeric = TRUE)
        tmp_table = rbind(tmp_table, data.frame(t(d)))
        if (lcnt==10) {
          names(tmp_table) = varnames
          tmp_table$group = rownames(tmp_table)
          tmp_table$question = question_id
          if (is_true_false) {
            tmp_table$score = c()
          }
          item_profile = rbind(item_profile, tmp_table)
          tmp_table = NA
        }
      
      } else if (lcnt == 12) { 
        tmp = .parse_columns(l, skip_chars = 55, as_numeric = TRUE)
        tmp = data.frame(t(tmp))
        names(tmp) = varnames
        tmp$correct = correct[1]
        if (n_correct>1) tmp$correct2 = correct[2]
        else tmp$correct2 = NA
        tmp$n_correct = n_correct
        tmp$question = question_id
        if (is_true_false) {
          tmp$score = c()
        }
        percentages = rbind(percentages, tmp)
        
        } else if (lcnt == 14) { 
        # m-values
        tmp = .parse_columns(l, skip_chars = 65, as_numeric = TRUE)
        tmp = data.frame(t(tmp))
        if (is_true_false) {
          names(tmp) = varnames[2:(length(varnames)-1)]
        } else
          names(tmp) = varnames[2:length(varnames)]
        tmp$question = question_id
        M_values = rbind(M_values, tmp)
      } else if (lcnt == 15) { 
        # restcorr
        tmp = .parse_columns(l, skip_chars = 65, as_numeric = TRUE)
        tmp = data.frame(t(tmp))
        if (is_true_false) {
          names(tmp) = varnames[2:(length(varnames)-1)]
        } else
          names(tmp) = varnames[2:length(varnames)]
        tmp$question = question_id
        restcorr = rbind(restcorr, tmp)
        
      }
    } 
  }
  close(fl)
  return(list(item_profile=item_profile, 
              percentages = percentages,
              M_values=M_values, 
              restcorr=restcorr))
}

.parse_columns <- function(txt, skip_chars=0, as_numeric=FALSE) {
  row = str_split(str_sub(txt, start=skip_chars+1), " ", simplify = TRUE)
  row = Filter(str_length, row) # remove null length
  if (as_numeric)  return(suppressWarnings(as.numeric(row)))
  else return(row)
}

