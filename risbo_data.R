# reading risbo zip file, Oliver Lindemann

.version_risbo_data = "0.4.0"

library(tidyverse)
library(stringi)
# requires also zip (do not import zip) 

zipfile_check <- function(zipfile, warnings=FALSE) {
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


parse_exam_info <- function(zipfile) {
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


parse_mc_exam <- function(zipfile, skip_item_statistics=FALSE) {
  # read content of risbo file

  message(paste0("reading ", zipfile))
  fls = zipfile_check(zipfile)
  if (is.null(fls)) {
    stop(paste0(zipfile, "is not a correct risbo file"))
  }
  info = parse_exam_info(zipfile)
  rtn <- list(processing_number = as.numeric(info$ProcessingNumber),
              info = info, 
              grades = .parse_grades(zipfile, fls$lsr_file))

  if (!skip_item_statistics) 
    rtn <- append(rtn, .parse_item_statistics(zipfile, fls$sta_file))
  
  class(rtn) <- "RisboMCExam"
  return(rtn)
}


get_course_list <- function(data_folder, only_newest_processing_version=FALSE) {
  
  files = list.files(path=data_folder, pattern=".zip")
  files = lapply(files, function(x) {file.path(data_folder, x)} )
  df = data.frame()
  broken_files = c()
  for (x in files) {
    fls = zipfile_check(x, warnings=TRUE)
    if (!is.null(fls)) {
      info = data.frame(parse_exam_info(x))
      
      info$file = x
      df = rbind(df, info)
    } else {
      broken_files = append(broken_files, x)
    }
  }
  print(broken_files)
  df = df %>%
        arrange(CourseCode, ExamDate) %>%
        group_by(ProcessingNumber) %>%
        arrange(desc(timestamp)) %>% 
        mutate(n_risbo_processings = n()) %>% 
        ungroup()
  if (only_newest_processing_version) {
    df = arrange(df, desc(n_risbo_processings)) %>% 
          group_by(ProcessingNumber) %>%
          slice(1) %>%                  # take only the first one
          ungroup() 
  }
  return(df)
}


.parse_grades <- function(zipfile, lsr_file) {
  fl = unz(zipfile, lsr_file)
  rtn <- read_delim(fl, delim="\t", col_names = FALSE,
                   skip=1, trim_ws = TRUE, na=c("NO"),
                   col_types = c(col_integer(),col_double()))
  names(rtn) <- c("student_id", "grade")
  return(rtn)
}


.parse_item_statistics <- function(zipfile, sta_file) {
  question_id = NA
  item_statistics_paragraph= FALSE 
  tmp_table = NA
  choices_profile = data.frame() # --> choices_profile
  choices = data.frame()
  M_values = data.frame()
  restcorr = data.frame()
  item_statistics = data.frame()
  lcnt = 0
  
  fl = unz(zipfile, sta_file)
  for (l in readLines(fl)) {
    if (str_starts(l, "Vraag") & !item_statistics_paragraph) {
      x = str_split(l, " ")[[1]]
      if (length(x)==2) {
          question_id = as.numeric(x[2])
          tmp_table = data.frame()
          lcnt = 0
      } 
    } 
    else if (str_starts(l, "STATISTISCH OVERZICHT")) {
      question_id = NA
      item_statistics_paragraph = TRUE
      tmp_table = data.frame()
      lcnt = 0
    }
    else if (!is.na(question_id)) { 
          lcnt = lcnt + 1
          if (lcnt == 4) { # varnames
            varnames = .parse_columns(l, skip_chars = 55)
            n_options = length(varnames) - 2
            is_true_false = (varnames[length(varnames)] == "score")
            if (is_true_false) {n_options = 2}
            correct = which(str_locate(varnames, "[*]")[,1]>0) -1
            n_correct = length(correct)
            varnames = str_remove_all(varnames, "[*]")
            if (n_correct>2) {
              message(paste0("WARNING: More than 2 options are correct in question ", question_id, " in ", zipfile))
            }
          
            } else if (lcnt>=6 & lcnt<= 10) { 
            # table choices_profile
            d = .parse_columns(l, skip_chars = 55, as_numeric = TRUE)
            tmp_table = rbind(tmp_table, data.frame(t(d)))
            if (lcnt==10) {
              names(tmp_table) = varnames
              tmp_table$group = rownames(tmp_table)
              tmp_table$question = question_id
              if (is_true_false) {
                tmp_table$score = c()
              }
              choices_profile = rbind(choices_profile, tmp_table)
              tmp_table = NA
            }
          
          } else if (lcnt == 12) { 
            #choices 
            tmp = .parse_columns(l, skip_chars = 55, as_numeric = TRUE)
            tmp = data.frame(t(tmp))
            names(tmp) = varnames
            if (n_options == 3) { #three options only, add empty d options for later joining
              tmp$d = NA
            }
            tmp$n_options = n_options
            tmp$correct = correct[1]
            if (n_correct>1) tmp$correct2 = correct[2]
            else tmp$correct2 = NA
            tmp$n_correct = n_correct
            tmp$correct_choosen = tmp[,correct[1] + 1]
            tmp$question = question_id
            if (is_true_false) {
              tmp$score = c()
            }
            choices = rbind(choices, tmp)
            
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
            } else {
              names(tmp) = varnames[2:length(varnames)]
            }
            tmp$question = question_id
            restcorr = rbind(restcorr, tmp)
            
          } 
    } # end !is.na(question_id)
    
    else if (item_statistics_paragraph) { 
          lcnt = lcnt + 1
          
          if (lcnt==3) {
            varnames = .parse_columns(l)
            varnames[length(varnames)] = "Vraag2"
          }
          else if (lcnt>3) {
            
            if (str_detect(l, "TENTAMENPROGRAMMA")) {
              item_statistics_paragraph = FALSE
            } 
            else if (!str_starts(l, "---")) {
              tmp = .parse_columns(l, as_numeric = TRUE)
              tmp = data.frame(t(tmp))
              names(tmp) = varnames
              item_statistics = rbind(item_statistics, tmp)
            }
          }
          
    }
  } # end for
  close(fl)
  item_statistics = tibble(item_statistics) %>%
                      select(-Vraag2) %>%
                      rename("question"="Vraag")
  
  return(list(choices_profile=tibble(choices_profile), 
              item_statistics=item_statistics,
              choices = tibble(choices),
              M_values=tibble(M_values), 
              restcorr=tibble(restcorr)))
}

.parse_columns <- function(txt, skip_chars=0, as_numeric=FALSE) {
  row = str_split(str_sub(txt, start=skip_chars+1), " ", simplify = TRUE)
  row = Filter(str_length, row) # remove null length
  if (as_numeric)  return(suppressWarnings(as.numeric(row)))
  else return(row)
}


course_list_find_resits <- function(course_list) {
  # find resits
  course_list = course_list %>% 
    mutate(resit=0) %>%
    arrange(CourseCode, ExamDate) 
  
  .prev_course = ""
  .prev_date = ""
  for (x in c(1:nrow(course_list))) {
    if (course_list$CourseCode[x] !=.prev_course) {
      # next course
      course_list$resit[x] = 0 
      .prev_course = course_list$CourseCode[x]
      .prev_date = course_list$ExamDate[x]
    } else if (course_list$ExamDate[x] != .prev_date) {
      # same course, different date --> resit
      course_list$resit[x] = course_list$resit[x-1] + 1
      .prev_date = course_list$ExamDate[x]
    } else {
      # same course, same data
      course_list$resit[x] = course_list$resit[x-1]
    }
  }
  return(course_list)
}

get_all_mc_exams <- function(course_list) {
  # read all exam that are in the course list (see get_course_list)
  exams = list()
  for (x in c(1:nrow(course_list))) {
    fl = course_list$file[x]
    tmp = parse_mc_exam(fl, skip_item_statistics = FALSE)
    exams[[as.character(tmp$processing_number)]] = tmp
  }  
  return(exams)
}

extract_all_grades <- function(exams) {
  all_grades = tibble()
  for (e in exams) {
    tmp = mutate(e$grades, ProcessingNumber = e$processing_number)
    all_grades = rbind(all_grades, tmp)
  }
  return(filter(all_grades, grade>0))
} 


extract_all_mc_choices <- function(exams) {
  all_items = tibble()
  for (e in exams) {
    if (e$info$ExamType == "MC") {
      tmp = mutate(e$choices, 
                   processing_number = e$processing_number,
                   n_options = e$info$n_options)
      all_items = rbind(all_items, tmp)
    }
  }
  
  ## find incorrect choices ##
  # wide -> long format
  all_items_long = all_items %>% 
    select(a,b,c,d, processing_number, question, correct_choosen) %>%
    gather("option", "percent_choices", -processing_number, -question,
           -correct_choosen) 
  
  ## add response ranking for each question (Max_1, Max_2,...)
  all_items_long = all_items_long %>% 
    group_by(processing_number, question) %>%
    arrange(percent_choices) %>%
    slice(seq_len(4)) %>%
    mutate(Max = paste0("max_", row_number())) %>%
    ungroup() %>%
    arrange(processing_number,question, option)
  
  
  # spread max responses (wide) & find highest incorrect response
  tmp = all_items_long %>%
    select(-option) %>%
    spread(Max, percent_choices) %>%
    mutate(preferred_incorrect_choosen = if_else(is.na(max_4),
                                           if_else(max_3==correct_choosen, max_2, max_3), # 3 options
                                           if_else(max_4==correct_choosen, max_3, max_4)  # 4 options
                                           )
                  ) %>%
    select(processing_number, question, preferred_incorrect_choosen)
  
  # join preferred_incorrect_choosen with
  all_items = all_items %>%
    left_join(tmp, by=c("processing_number", "question"))
    
  return(all_items)
}

extract_all_mc_item_statistics <- function(exams) {
  rtn = tibble()
  for (e in exams) {
    if (e$info$ExamType == "MC") {
      tmp = mutate(e$item_statistics, processing_number = e$processing_number)
      rtn = rbind(rtn, tmp)
    }
  }
  return(rtn)
} 

mc_item_summary <- function(all_items, all_item_statistics){
  a = all_items %>% 
          mutate(very_easy = correct_choosen > 95,
                 below_guessing = correct_choosen < (100/n_options),
                 incorr_preferred = preferred_incorrect_choosen - correct_choosen>10) %>%
          group_by(processing_number) %>%
          summarise(n_very_easy = sum(very_easy, na.rm = TRUE),
                    n_below_guessing = sum(below_guessing, na.rm = TRUE),
                    n_incorr_preferred = sum(incorr_preferred, na.rm = TRUE)) 
  
  b = all_item_statistics %>%
          mutate(low_restcorr = restcorr < 0.02) %>%
          group_by(processing_number) %>%
          summarise(n_low_restcorr = sum(low_restcorr, na.rm = TRUE))
              
  return(left_join(a,b, by="processing_number"))
}

exam_descriptive_stats <- function(exams) {
  exam_stats = tibble()
  for (e in exams) {
    if (!is.null(e)) {
      tmp = filter(e$grades, grade>0)
      stats = tibble(
        ProcessingNumber=e$processing_number,
        n_grades = nrow(tmp),
        n_failed = nrow(filter(tmp, grade<5.5)),
        mean_grade = mean(tmp$grade),
        std_grade = sd(tmp$grade),
        n_multiple_correct = nrow(filter(e$choices, n_correct>1)),
        n_inactive = e$info$NumQuestion - nrow(e$choices),
        adjustments_made = (n_multiple_correct>0 | n_inactive>0)
      ) %>%
        mutate(perc_failed = 100* n_failed / n_grades)
      
      exam_stats = rbind(exam_stats, stats)
    } #endif
  }
  return(exam_stats)
}
  
