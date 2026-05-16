# ============================================================================
# Functions Released
# ============================================================================

# This package is for criminal CJO data analysis.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
# use pkgdown::build_site() to build the manual.

#####################################
# functions begin here###############
#####################################

#####################################
## Function to convert Chinese numerals to Arabic numerals
## ignore this function, not for useage. (ends here)
#####################################

#' Converting Chinese into Numbers
#'
#' We provide a chinese_to_arabic function to convert Chinese to Arabic numbers.
#' This function will be used by the function for getting sentence information.
#' However, users can also use this function to convert other Chinese numbers.
#' The maximum unit of converting is one hundred million.
#' Our converting will consider "两", which is same to "二" in Chinese. The unicode of "两" is u4e24.
#'
#' @param chinese_num a Chinese character representing numbers, e.g., "三百二十八".
#'
#' @return converting results, a number.
#'
#' @examples
#' # beginto show results: this function includes "\u4e24".
#' chinese_to_arabic("\u4e8c\u767e\u4e09\u5341\u516d")
#' chinese_to_arabic("\u4e24\u767e\u4e00\u5341\u516b")
#' # you can also provide the variable.
#' ch_num <- "\u4e94\u767e\u516b\u5341\u4e09"
#' chinese_to_arabic(ch_num)
#'
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u4e24 \tab "两" \cr
#' \\u4e8c\\u767e\\u4e09\\u5341\\u516" \tab "二百三十六" \cr
#' \\u4e24\\u767e\\u4e00\\u5341\\u516b" \tab "两百一十八" \cr
#' \\u4e94\\u767e\\u516b\\u5341\\u4e09" \tab "五百八十三" \cr
#'}
#'
#' @export
chinese_to_arabic <- function(chinese_num) {
  # digit number
  digits <- c("\u96f6"=0, "\u4e00"=1, "\u4e8c"=2, "\u4e24"=2,"\u4e09"=3, "\u56db"=4, "\u4e94"=5, "\u516d"=6, "\u4e03"=7, "\u516b"=8, "\u4e5d"=9)
  # tenth and above units
  units <- c("\u5341"=10, "\u767e"=100, "\u5343"=1000, "\u4e07"=10000, "\u4ebf"=1e8)
  # select big units (seldom used)
  big_units <- c("\u4e07", "\u4ebf")

  total <- 0
  section <- 0   # default section number
  num <- 0       # current deal number

  chars <- strsplit(chinese_num, "")[[1]]
  for (ch in chars) {
    if (ch %in% names(digits)) {
      num <- digits[ch]
    } else if (ch %in% names(units)) {
      unit_val <- units[ch]
      if (ch %in% big_units) {
        # big units section
        section <- section + num
        total <- total + section * unit_val
        section <- 0
        num <- 0
      } else {
        # transfer units into section
        if (num == 0) {
          section <- section + 1 * unit_val
        } else {
          section <- section + num * unit_val
        }
        num <- 0
      }
    } else {
      # ignore unkown values
      next
    }
  }
  # add last number and sum up sections.
  section <- section + num
  total <- total + section
  names(total) <- NULL
  return(total)
}
#####################################
## Function to convert Chinese numerals to Arabic numerals
## ignore this function, not for usage. (ends here)
#####################################

#' Get Sentencing Data in Months
#'
#' We provide a get_prison_month function to extract the sentencing results from one single sentencing document.
#' This function only considers "有期徒刑", imprisonment.
#' For "无期徒刑", life sentence, we will return an Inf value.
#' Other two kinds of sentencing, "拘役" and "管制" -- criminal detention and public surveillance, will be ignored, like many other studies have done.
#' The unit of output is "month".
#'
#' @param text a long text containing sentencing information
#'
#' @return the length of sentencing in months of imprisonment
#' @examples
#' # directly convert the sentence.
#' get_prison_month("\u88ab\u544a\u4eba\u738b\u67d0\u72af\u5bfb\u8845\u6ecb\u4e8b\u7f6a\uff0c\u5224\u5904\u6709\u671f\u5f92\u5211\u4e24\u5e74\u516d\u4e2a\u6708")
#' # you can also provide a variable.
#' sentence_text <- "\u88ab\u544a\u4eba\u738b\u67d0\u72af\u5bfb\u8845\u6ecb\u4e8b\u7f6a\uff0c\u5224\u5904\u6709\u671f\u5f92\u5211\u4e24\u5e74\u516d\u4e2a\u6708"
#' get_prison_month(sentence_text)
#'
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u88ab\\u544a\\u4eba...\\u4e2a\\u6708 \tab "被告人王某犯寻衅滋事罪，判处有期徒刑两年六个月" \cr
#'}
#'
#' @export
get_prison_month <- function(text) {
  if (is.na(text) || nchar(trimws(text)) == 0) {
    return(NA_real_)
  }

  # Clean the text
  text <- stringr::str_trim(text)

  if (grepl("\u5224\u5904\u65e0\u671f\u5f92\u5211", text)) {
    return(Inf)
  }

  pattern <- "\u5224\u5904\u6709\u671f\u5f92\u5211([^\uff0c\u3002\uff1b]*(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5e74)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u4e2a\u6708)?)"

  # Extract the matching part
  match <- stringr::str_extract(text, pattern)
  if (is.na(match)) {
    return(NA_real_)
  }

  # Extract years (if present)
  years <- 0
  year_match <- stringr::str_extract(match, "[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5e74")
  if (!is.na(year_match)) {
    year_text <- stringr::str_replace(year_match, "\u5e74", "")
    years <- chinese_to_arabic(year_text)
    if (is.na(years)) return(NA_real_)
  }

  # Extract months (if present)
  months <- 0
  month_match <- stringr::str_extract(match, "[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u4e2a\u6708")
  if (!is.na(month_match)) {
    month_text <- stringr::str_replace(month_match, "\u4e2a\u6708", "")
    months <- chinese_to_arabic(month_text)
    if (is.na(months)) return(NA_real_)
  }

  # If no years or months extracted, return NA
  if (years == 0 && months == 0) {
    return(NA_real_)
  }
  # Convert to total months
  return(years * 12 + months)
}


#' Get Sentencing Data in Days
#'
#' We provide a get_sentence_day function to extract the sentencing results from one single sentencing document.
#' This function considers "有期徒刑", "无期徒刑" and "拘役" and "管制" -- imprisonment, life sentence, criminal detention, public surveillance.
#' Again, "无期徒刑", life sentence, will return an Inf.
#' The unit of output is "days". One year is considered as 365 days, and one month is considered as 30 days.
#'
#' @param text a long text containing sentencing information
#'
#' @return the length of sentencing in days of imprisonment
#' @examples
#' # directly convert the sentence.
#' get_sentence_day("\u88ab\u544a\u4eba\u738b\u67d0\u72af\u5bfb\u8845\u6ecb\u4e8b\u7f6a\uff0c\u5224\u5904\u62d8\u5f79\u516d\u4e2a\u6708\u516b\u5929")
#' # you can also provide a variable.
#' sentence_text <- "\u88ab\u544a\u4eba\u738b\u67d0\u72af\u5bfb\u8845\u6ecb\u4e8b\u7f6a\uff0c\u5224\u5904\u62d8\u5f79\u516d\u4e2a\u6708\u516b\u5929"
#' get_sentence_day(sentence_text)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u88ab\\u544a\\u4eba...\\u516b\\u5929 \tab "被告人王某犯寻衅滋事罪，判处拘役六个月八天" \cr
#' \\u88ab\\u544a\\u4eba...\\u5236\\u4e5d\\u5929 \tab "被告人王某犯寻衅滋事罪，判处管制九天" \cr
#'}
#'
#' @export
# get sentencing days
get_sentence_day <- function(text) {
  if (is.na(text) || nchar(trimws(text)) == 0) {
    return(NA_real_)
  }

  # Clean the text
  text <- stringr::str_trim(text)

  if (grepl("\u5224\u5904\u65e0\u671f\u5f92\u5211", text)) {
    return(Inf)
  }

  # Pattern for year, month, day,
  pattern <- "\u5224\u5904(?:\u6709\u671f\u5f92\u5211|\u62d8\u5f79|\u7ba1\u5236)([^\uff0c\u3002\uff1b]*(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5e74)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u4e2a\u6708)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5929)?)"

  # Extract the matching part
  match <- stringr::str_extract(text, pattern)
  if (is.na(match)) {
    return(NA_real_)
  }

  # Extract years (if present)
  years <- 0
  year_match <- stringr::str_extract(match, "[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5e74")
  if (!is.na(year_match)) {
    year_text <- stringr::str_replace(year_match, "\u5e74", "")
    years <- chinese_to_arabic(year_text)
    if (is.na(years)) return(NA_real_)
  }

  # Extract months (if present)
  months <- 0
  month_match <- stringr::str_extract(match, "[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u4e2a\u6708")
  if (!is.na(month_match)) {
    month_text <- stringr::str_replace(month_match, "\u4e2a\u6708", "")
    months <- chinese_to_arabic(month_text)
    if (is.na(months)) return(NA_real_)
  }

  day <- 0
  day_match <- stringr::str_extract(match, "[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5929")
  if (!is.na(day_match)) {
    day_text <- stringr::str_replace(day_match, "\u5929", "")
    day <- chinese_to_arabic(day_text)
    if (is.na(day)) return(NA_real_)
  }


  # If no years or months extracted, return NA
  if (years == 0 && months == 0 && day == 0) {
    return(NA_real_)
  }
  # Convert to total months
  return(years * 365 + months * 30 + day)
}

#' Get Probation Data in Month
#'
#' We provide a get_probation function to extract the "缓刑", probation, results from one single sentencing document.
#' This function only considers probation and its lenght.
#' The unit of the probation length output is "month".
#'
#' @param text a long text containing sentencing information
#'
#' @return two values: whether sentenced to probation, yes = 1, no = 0; the length of probation in month.
#' @examples
#' # directly convert the sentence.
#' get_probation("\u88ab\u544a\u4eba\u738b\u67d0\u72af\u5bfb\u8845\u6ecb\u4e8b\u7f6a\uff0c\u5224\u5904\u6709\u671f\u5f92\u5211\u516d\u4e2a\u6708\uff0c\u7f13\u5211\u4e24\u5e74")
#' # you can also provide a variable.
#' probation_text <- "\u88ab\u544a\u4eba\u738b\u67d0\u72af\u76d7\u7a83\u7f6a\uff0c\u5224\u5904\u6709\u671f\u5f92\u5211\u516b\u4e2a\u6708,\uff0c\u7f13\u5211\u4e09\u4e2a\u6708"
#' get_probation(probation_text)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u88ab\\u544a\\u4eba...\\u5211\\u4e24\\u5e74 \tab "被告人王某犯寻衅滋事罪，判处有期徒刑六个月, 缓刑两年" \cr
#' \\u88ab\\u544a\\u4eba...\\u4e09\\u4e2a\\u6708 \tab "被告人王某犯盗窃罪，判处有期徒刑八个月, 缓刑三个月" \cr
#'}
#'
#' @export
# get probation
get_probation <- function(text) {
  if (is.na(text) || nchar(trimws(text)) == 0) {
    return(NA_real_)
  }

  # Clean the text
  text <- stringr::str_trim(text)

  pattern <- "\u5224\u5904(?:\u6709\u671f\u5f92\u5211|\u65e0\u671f\u5f92\u5211|\u62d8\u5f79|\u7ba1\u5236)((?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5e74)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u4e2a\u6708)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5929)?)[[:punct:][:space:]]*\u7f13\u5211((?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5e74)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u4e2a\u6708)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5929)?)"
  match <- stringr::str_extract(text, pattern)
  pattern2 <- "\u7f13\u5211((?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5e74)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u4e2a\u6708)?(?:[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5929)?)"
  match <- stringr::str_extract(match, pattern2)

  # return value 0
  if (is.na(match)) {
    value <- c(0,0)
    return(value)
  }

  years <- 0
  year_match <- stringr::str_extract(match, "[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u5e74")
  if (!is.na(year_match)) {
    year_text <- stringr::str_replace(year_match, "\u5e74", "")
    years <- chinese_to_arabic(year_text)
    if (is.na(years)) return(NA_real_)
  }

  # Extract months (if present)
  months <- 0
  month_match <- stringr::str_extract(match, "[\u4e00\u4e24\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341]+\u4e2a\u6708")
  if (!is.na(month_match)) {
    month_text <- stringr::str_replace(month_match, "\u4e2a\u6708", "")
    months <- chinese_to_arabic(month_text)
    if (is.na(months)) return(NA_real_)
  }

  if (years == 0 && months == 0) {
    value <- c(0,0)
    return(value)
  }

  # final value
  value <- c(1, years * 12 + months)
  return(value)
}

#' Get Document Term Matrix Without Weight
#'
#' We provide a get_dtm function to get the DTM of a list of texts for further analysis.
#' We make use of tm package and chinese.misc package to calculate this matrix.To cut sentences into tokens, we use jiebaR.
#' This function contains no weight to generate a matrix. DTM is the most easy, and also most common used matrix to deal with text data.
#'
#'
#' @param text a long text, better to be a column of texts from a data frame.
#'
#' @return a matrix, each row contains a text's vector information
#' @examples
#' # please remember that, we need a column from a data.frame.
#' df <- data.frame(text = c("\u6211\u662f\u4e00\u4e2a\u5211\u4e8b\u8bc9\u8bbc\u6cd5\u5f8b\u5e08",
#'                  "\u6211\u662f\u4e00\u4e2a\u5f8b\u5e08"),
#'                   stringsAsFactors = FALSE
#'                  )
#' # this function only requires the "text" column.
#' get_dtm(df$text)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u6211\\u662f\\u4e00...\\u6cd5\\u5f8b\\u5e08 \tab "我是一个刑事诉讼法律师" \cr
#' \\u6211\\u662f\\u4e00\\u4e2a\\u5f8b\\u5e08 \tab "我是一个律师" \cr
#'}
#'
#' @importFrom tm DocumentTermMatrix
#' @importFrom chinese.misc corp_or_dtm
#' @importFrom jiebaR worker
#'
#' @export
# create dtm without weight: a list of data
get_dtm <- function(text) {
    corpus <- as.character(text)
    cutted <- chinese.misc::corp_or_dtm(
      corpus,
      from = "v",
      type = "corpus",
      enc = "auto",
      mycutter =  jiebaR::worker(),
      stop_word = "jiebaR",
      stop_pattern = NULL,
      control = "auto",
      myfun1 = NULL,
      myfun2 = NULL,
      special = "",
      use_stri_replace_all = FALSE
    )
    dtm <- tm::DocumentTermMatrix(cutted)
    dtm.mat <- as.matrix(dtm)
    return(dtm.mat)
}

#' Get Document Term Matrix With Tf-Idf Weight
#'
#' We provide a get_dtm_tfidf function to get the DTM of a list of texts for further analysis.
#' We make use of tm package and chinese.misc package to calculate this matrix. To cut sentences into tokens, we use jiebaR.
#' This function contains a tf-idf weight function to generate a matrix.
#' From Wiki: tf-idf is a measure of importance of a word to a document in a collection or corpus,
#' adjusted for the fact that some words appear more frequently in general
#'
#' @param text a long text, better to be a column of texts from a data frame.
#'
#' @return a matrix, each row contains a text's vector information.
#' @examples
#' # please rememver that, we need a column from a data.frame.
#' df <- data.frame(text = c("\u6211\u662f\u4e00\u4e2a\u5211\u4e8b\u8bc9\u8bbc\u6cd5\u5f8b\u5e08",
#'                  "\u6211\u662f\u4e00\u4e2a\u5f8b\u5e08"),
#'                   stringsAsFactors = FALSE
#'                  )
#' # this function only requires the "text" column.
#' get_dtm_tfidf(df$text)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u6211\\u662f\\u4e00...\\u6cd5\\u5f8b\\u5e08 \tab "我是一个刑事诉讼法律师" \cr
#' \\u6211\\u662f\\u4e00\\u4e2a\\u5f8b\\u5e08 \tab "我是一个律师" \cr
#'}
#'
#' @importFrom tm DocumentTermMatrix
#' @importFrom chinese.misc corp_or_dtm
#' @importFrom jiebaR worker
#'
#' @export
# create dtm with weight
get_dtm_tfidf <- function(text) {
    corpus <- as.character(text)
    cutted <- corp_or_dtm(
      corpus,
      from = "v",
      type = "corpus",
      enc = "auto",
      mycutter =  worker(),
      stop_word = "jiebaR",
      stop_pattern = NULL,
      control = "auto",
      myfun1 = NULL,
      myfun2 = NULL,
      special = "",
      use_stri_replace_all = FALSE
    )
    dtm <- DocumentTermMatrix(cutted)

    # adding weight: some tokens occur in all cases
    dtm.tfidf <- tm::weightTfIdf(dtm)
    dtm.tfidf.mat <- as.matrix(dtm.tfidf)
    return(dtm.tfidf.mat)
}

#####################################
####function to calculate similarity
#####################################
#' Similarity Function -- Cosine Similarity
#'
#' These functions can be used to calculate a series of data,
#' while functions in other packages can not do it.
#' These functions are very basic and we also allow users to make use of them.
#' Usually we consider the input is a matrix. However, you can try other as well.
#' However, other functions will make use of these functions in a correct way.
#' So, it is not a must to know the detail of the function. If you are interested, please go to the package's original codes.
#'
#' @param a the baseline
#' @param b a series of data being compared with the baseline
#'
#' @return a series of num. data
#'
#' @export
sim_cosine <- function(a,b){
  numer <- apply(a * t(b), 2, sum)
  denom <- sqrt(sum(a^2)) * sqrt(apply(b^2, 1, sum))
  return(numer / denom)
}

#' Similarity Function -- Jaccard Similarity
#'
#' These functions can be used to calculate a series of data,
#' while functions in other packages can not do it.
#' These functions are very basic and we also allow users to make use of them.
#' Usually we consider the input is a matrix. However, you can try other as well.
#' However, other functions will make use of these functions in a correct way.
#' So, it is not a must to know the detail of the function. If you are interested, please go to the package's original codes.
#'
#' @param a the baseline
#' @param b a series of data being compared with the baseline
#'
#' @return a series of num. data
#'
#' @export
sim_jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

#' Similarity Function -- Euclidean Similarity
#'
#' These functions can be used to calculate a series of data,
#' while functions in other packages can not do it.
#' These functions are very basic and we also allow users to make use of them.
#' Usually we consider the input is a matrix. However, you can try other as well.
#' However, other functions will make use of these functions in a correct way.
#' So, it is not a must to know the detail of the function. If you are interested, please go to the package's original codes.
#' NOTE: You can use this function to calculate the similarity of sentencing data.
#'
#' @param a the baseline
#' @param b a series of data being compared with the baseline
#'
#' @return a matrix containing a series of num. data
#'
#' @examples
#' mat <- matrix(c(20, 10, 40, 50), nrow = 4, ncol = 1)
#' print(mat)
#' sim_euclidean(mat[1,], mat)
#'
#' @export
sim_euclidean <- function(a, b) {
  diff <- b - matrix(a, nrow = nrow(b), ncol = length(a), byrow = TRUE)
  dist <- sqrt(rowSums(diff^2))
  return(dist)
}

#' Similarity Function -- Correlation Similarity
#'
#' These functions can be used to calculate a series of data,
#' while functions in other packages can not do it.
#' These functions are very basic and we also allow users to make use of them.
#' Usually we consider the input is a matrix. However, you can try other as well.
#' However, other functions will make use of these functions in a correct way.
#' So, it is not a must to know the detail of the function. If you are interested, please go to the package's original codes.
#'
#' @param a the baseline
#' @param b a series of data being compared with the baseline
#'
#' @return a series of num. data
#'
#' @export
sim_correlation <- function(a, b) {
  a_cent <- a - mean(a)
  b_cent <- b - rowMeans(b)

  numer <- apply(a_cent * t(b_cent), 2, sum)
  denom <- sqrt(sum(a_cent^2)) * sqrt(apply(b_cent^2, 1, sum))

  denom[denom == 0] <- NA
  return(numer / denom)
}
#####################################
####function to calculate similarity (ends here)
#####################################

#' Treating Like Cases Alike Function
#'
#' We provide a TLCA function that can complete the analysis by Chen et. al.'s paper on Law probability and risk in a single row of data.
#' This functions can be used to calculate a series of data,
#' The detail of this method should be found on https://doi.org/10.1093/lpr/mgag007
#' The original method of this paper is RWMD, but it only affects the DTM.
#' Users are welcome to provide their own DTM using LLMs, RWMD, Bert or other methods.
#'
#' @param matrix the DTM matrix you get using get_dtm function or other methods you employs.
#' @param method can be "cosine", "jaccard", "correlation" or "euclidean", which leads to a usage of corresponding similarity calculation method.
#' @param k use the first k cases as the baseline cases. The default value is total number of your case in the matrix.
#'
#' @return a matrix contains similarity data. Each data represent the similarity between the row and column case.
#'
#' @examples
#' # assuming we have a matrix contains 3 rows and 4 columns, which means 3 cases with 4 factors
#' mat <- matrix(c(1,0,0,0,1,1,0,0,1,1,1,0), nrow = 3, ncol = 4)
#' # show data structure
#' print(mat)
#' # the default value of \code{k} is 3 in this example.
#' TLCA(mat, method = "cosine")
#'
#' @export
TLCA <- function(matrix, method = c("cosine", "jaccard", "correlation", "euclidean"), k) {
  if (missing(k)) {
    k <- nrow(matrix)
  }
  n <- nrow(matrix)
  numberSim <- data.frame(matrix(NA, nrow = k, ncol = n))
  rownames(numberSim) <- 1:k
  colnames(numberSim) <- 1:n

  # cosine
  if (method == "cosine") {
    for (i in 1:k) {
      numberSim[i,] <- sim_cosine(matrix[i,], matrix)
    }
  }

  # jaccard
  if (method == "jaccard") {
    for (i in 1:k) {
      numberSim[i,] <- sim_jaccard(matrix[i,], matrix)
    }
  }

  # correlation
  if (method == "correlation") {
    for (i in 1:k) {
      numberSim[i,] <- sim_correlation(matrix[i,], matrix)
    }
  }

  # euclidean
  if (method == "euclidean") {
    for (i in 1:k) {
      numberSim[i,] <- sim_euclidean(matrix[i,], matrix)
    }
  }
  return(numberSim)
}

#' No-Reasoning Function
#'
#' We provide a homo_index function that can be used to analyze Chinese judges' no-reasoning problems.
#' Compared with other regions, Chinese sentencing documents are criticized that they do not provide enough reasoning.
#' We first sum up all cases and get a vector as the baseline, and use cosine similarity to calculate the similarity between each case and this baseline to get a series of data, and get the mean of these data.
#' The mean is the index. We also provide original data, which is the series of data, because users may want to give a weight to them.
#' The logic behind this is the overall similarity between reasoning texts.
#' The detail of this logic is that, if most judges do not provide enough reasoning, it means that they simply copy from others' sayings. Thus, we can use this index to reprent it.
#' This functions can be used to calculate a series of data.
#' homo_index uses cosine similarity because it is not sensitive to length.
#'
#' @param matrix the DTM matrix you get using get_dtm function or other methods you employs.
#' @param output can be "mean" or "original". "mean" will provide the index, while "original" provide the similarity data.
#'
#' @return an index, or a set of similarities.
#'
#' @examples
#' # this function requires a matrix, we create a 3 rwo, 4 column matrix
#' mat <- matrix(c(1,0,0,0,1,1,0,0,1,1,1,0), nrow = 3, ncol = 4)
#' # show data structure
#' print(mat)
#' # you can get index, or get original data
#' homo_index(matrix = mat, output = "mean")
#' # not that this function will ignore NAs automatically.
#' homo_index(matrix = mat, output = "original")
#'
#' @export
# homo-analysis
# users can choose mean value or original value
# homo_index uses cosine similarity because it is not sensitive to length.
# homo_index can analyze english documents as well.
homo_index <- function(matrix, output = c("mean", "original")) {
  unit <- colSums(matrix)
  homo_values <- sim_cosine(unit, matrix)

  # mean value
  if (output == "mean"){
    mean_value <- mean(homo_values, na.rm = TRUE)
    print("Note: you will get the index! ")
    return(mean_value)
  }

  # original value
  else {
    print("Note: you will get the original data!")
    return(homo_values)
  }
}

#' Legal Information Extraction Function
#'
#' We provide a get_legal_factor function that can be used to extract legal factors users want.
#' The method is "regular expression", and we have already provide a relatively stable, cheap method.
#' We require users to write the keywords, and also provide the legal_article numbers in Chinese.
#' Then, users should also input the reasoning text and the legal article part of the sentencing document.
#' Only when keywords and legal_article numbers can be found in reasoning text and legal article text, we return a value 1 representing there is a factor here.
#' Otherwise, the function will return a 0.
#' Please note that, in this function, we can not extract the amount of property crimes. To extract the amount, one needs to ask help from LLMs or design their own regular expressions.
#' However, this function can save much time and also be used as a baseline function.
#'
#' @param maindoc the reasoning text
#' @param legaldoc the legal article text from the appendix
#' @param keyword the legal factor you want to get, e.g. "认罪认罚"
#' @param legal_article the legal factor's corresponding legal article number, e.g., "第三十条"
#'
#' @return whether there is a legal factor that users care about. yes = 1, no = 0.
#'
#' @examples
#' # note: please get reason and appendix use function we provide. this is for illustration.
#' reason <- data.frame(text = c("\u672c\u9662\u8ba4\u4e3a\uff0c\u88ab\u544a\u4eba\u72af\u5bfb\u8845\u6ecb\u4e8b\u7f6a\uff0c\u81ea\u9996\uff0c\u4e14\u8ba4\u7f6a\u8ba4\u7f5a\uff0c\u56e0\u6b64\u53ef\u4ee5\u914c\u60c5\u51cf\u514d\u5211\u7f5a", "\u672c\u9662\u8ba4\u4e3a\uff0c\u88ab\u544a\u4eba\u72af\u5bfb\u8845\u6ecb\u4e8b\u7f6a\uff0c\u81ea\u9996\uff0c\u56e0\u6b64\u53ef\u4ee5\u914c\u60c5\u51cf\u514d\u5211\u7f5a"))
#' appendix <- data.frame(text = c("\u7b2c\u4e8c\u767e\u4e5d\u5341\u4e09\u6761\u6709\u4e0b\u5217\u5bfb\u8845\u6ecb\u4e8b\u884c\u4e3a\u4e4b\u4e00\uff0c\u7834\u574f\u793e\u4f1a\u79e9\u5e8f\u7684\uff0c\u5904\u4e94\u5e74\u4ee5\u4e0b\u6709\u671f\u5f92\u5211\u3001\u62d8\u5f79\u6216\u8005\u7ba1\u5236\uff1a\u5211\u4e8b\u8bc9\u8bbc\u6cd5\u7b2c\u5341\u4e94\u6761\uff1a\u7b2c\u5341\u4e94\u6761\u3000\u72af\u7f6a\u5acc\u7591\u4eba\u3001\u88ab\u544a\u4eba\u81ea\u613f\u5982\u5b9e\u4f9b\u8ff0\u81ea\u5df1\u7684\u7f6a\u884c\uff0c\u627f\u8ba4\u6307\u63a7\u7684\u72af\u7f6a\u4e8b\u5b9e\uff0c\u613f\u610f\u63a5\u53d7\u5904\u7f5a\u7684\uff0c\u53ef\u4ee5\u4f9d\u6cd5\u4ece\u5bbd\u5904\u7406\u3002", "\u7b2c\u4e8c\u767e\u4e5d\u5341\u4e09\u6761\u6709\u4e0b\u5217\u5bfb\u8845\u6ecb\u4e8b\u884c\u4e3a\u4e4b\u4e00\uff0c\u7834\u574f\u793e\u4f1a\u79e9\u5e8f\u7684\uff0c\u5904\u4e94\u5e74\u4ee5\u4e0b\u6709\u671f\u5f92\u5211\u3001\u62d8\u5f79\u6216\u8005\u7ba1\u5236\uff1a\u5211\u4e8b\u8bc9\u8bbc\u6cd5\u7b2c\u5341\u4e94\u6761\uff1a\u7b2c\u5341\u4e94\u6761\u3000\u72af\u7f6a\u5acc\u7591\u4eba\u3001\u88ab\u544a\u4eba\u81ea\u613f\u5982\u5b9e\u4f9b\u8ff0\u81ea\u5df1\u7684\u7f6a\u884c\uff0c\u627f\u8ba4\u6307\u63a7\u7684\u72af\u7f6a\u4e8b\u5b9e\uff0c\u613f\u610f\u63a5\u53d7\u5904\u7f5a\u7684\uff0c\u53ef\u4ee5\u4f9d\u6cd5\u4ece\u5bbd\u5904\u7406\u3002"))
#' # I want to find whether the defendant plead guilty, corresponding article number 15.
#' target <- "\u8ba4\u7f6a\u8ba4\u7f5a"
#' articleNum <- "\u7b2c\u5341\u4e94\u6761"
#' ## 01. run the function: one keyword, one article.
#' get_legal_factor(maindoc = reason$text, legaldoc = appendix$text, keyword = target, legal_article = articleNum)
#' # you may want to have several corresponding articles referring to single factor
#' articleNum <- c("\u7b2c\u5341\u4e94\u6761", "\u7b2c\u4e8c\u767e\u4e5d\u5341\u4e09\u6761")
#' ## 02: run the function: one keyword, two articles.
#' get_legal_factor(maindoc = reason$text, legaldoc = appendix$text, keyword = target, legal_article = articleNum)
#' # or you want to have several keywords to make sure your extraction is correct.
#' target <- c("\u8ba4\u7f6a\u8ba4\u7f5a", "\u5177\u7ed3\u4e66")
#' ## 03: run the function: two keywords, two articles.
#' get_legal_factor(maindoc = reason$text, legaldoc = appendix$text, keyword = target, legal_article = articleNum)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u672c\\u9662...\\u8ba4\\u7f6a...\\u5211\\u7f5a \tab "本院认为，被告人犯寻衅滋事罪，自首，且认罪认罚，因此可以酌情减免刑罚" \cr
#' \\u672c\\u9662...\\u5211\\u7f5a \tab "本院认为，被告人犯寻衅滋事罪，自首，因此可以酌情减免刑罚" \cr
#' \\u7b2c\\u4e8c...\\u7406\\u3002 \tab 此段为刑法第二百九十三条与刑事诉讼法第十五条 \cr
#' \\u7b2c\\u5341\\u4e94\\u6761 \tab "第十五条" \cr
#' \\u7b2c\\u4e8c\\u767e\\u4e5d\\u5341\\u4e09\\u6761 \tab "第二百九十三条" \cr
#' \\u8ba4\\u7f6a\\u8ba4\\u7f5a \tab "认罪认罚" \cr
#' \\u5177\\u7ed3\\u4e66 \tab "具结书" \cr
#'}
#'
#'
#' @export
# extract certain legal factors (regular expression)
get_legal_factor <- function(maindoc, legaldoc, keyword, legal_article){
  n <- length(maindoc)
  value <- data.frame(rep(NA, n))
  names(value) <- keyword[1]
  for (i in 1:n) {
    if (all(sapply(keyword, grepl, x = maindoc[i], fixed = TRUE)) && all(sapply(legal_article, grepl, x = legaldoc[i], fixed = TRUE))){
      value[i,] = 1
    } else {
      value[i,] = 0
    }
  }
  return(value)
}

#' Guess Gender Function
#'
#' We provide a ngenderR_CN function that can be used to guess the gender of a name.
#' The method is originally a python package and we transfer this into a R function.
#' The mathematical reason behind the method can be found here: https://github.com/observerss/ngender.
#' This function is to get judges' gender information through their names.
#'
#' @param judgename judge's name in Chines, should be a full name. It should be a character variable.
#'
#' @return male = 1, female = 0; and the probability of being that gender.
#'
#' @examples
#' # male example
#' name1 <- "\u8d75\u672c\u5c71"
#' ngenderR_CN(name1)
#' # female example
#' name2 <- "\u5b8b\u4e39\u4e39"
#' ngenderR_CN(name2)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u8d75\\u672c\\u5c71 \tab "赵本山" \cr
#' \\u5b8b\\u4e39\\u4e39 \tab "宋丹丹" \cr
#'}
#'
#' @export
# extract certain legal factors (regular expression)
# you can calcualte the gender by names. We introduce Ngender in Python here.
# a beysian stats tool is used here.
ngenderR_CN <- function(judgename) {
  firstname <- substr(judgename, 2, nchar(judgename))
  prior_male <- sum(freq_dta$male) / (sum(freq_dta$male) + sum(freq_dta$female))
  prior_female <- sum(freq_dta$female) / (sum(freq_dta$male) + sum(freq_dta$female))

  # calculate score
  score_male <- 1
  for (i in 1:nchar(firstname)) {
    score_male <- score_male *
      freq_dta[freq_dta$char == substr(firstname,i,i),]$male /
      sum(freq_dta$male)
  }
  score_male <- score_male * prior_male
  score_female <- 1
  for (i in 1:nchar(firstname)) {
    score_female <- score_female *
      freq_dta[freq_dta$char == substr(firstname,i,i),]$female /
      sum(freq_dta$female)
  }
  score_female <- score_female * prior_female
  probability <- score_male / (score_female + score_male)

  # print final result
  if (score_male > score_female) {
    value <- c(1, probability)
    print(paste0("male, probability: ",probability))
  } else {
    value <- c(0, probability)
    print(paste0("female, probability: ", 1-probability))
  }
  return(value)
}


# cut judgements into several parts (facts, reasoning, sentencing, legal articles)
#' Cutting Function - Single Case
#'
#' This function is to split documents into four parts: "facts", "reasoning", and "decision" and "law".
#' This function is only for single case.
#' You are required to input a long text, and this function will automatically cut this document into four parts.
#' You can use \code{for} or \code{lapply} function or use Cutting Function - DataFrame (\code{split_judgment_df}) to deal with a set of cases
#' As for the cutting logic, please refer to our paper "Judges are trained as good explainers but maligned sentencers: a text similarity approach"
#'
#' @param text the document characters.
#' @return `FACTS`, `REASON`, `DECISION` and `LAW`.
#'
#' @examples
#' text <- "\u7ecf\u5ba1\u7406\u67e5\u660e\uff1a\u76d7\u7a83\u884c\u4e3a\u3002\u672c\u9662\u8ba4\u4e3a\u662f\u72af\u7f6a\u3002\u4f9d\u7167\u300a\u5211\u6cd5\u300b\u7b2c\u4e09\u5341\u6761\uff0c\u300a\u5211\u4e8b\u8bc9\u8bbc\u6cd5\u300b\u7b2c\u4e8c\u5341\u6761\uff0c\u5224\u51b3\u5982\u4e0b:\u4e94\u5e74\u6709\u671f\u5f92\u5211"
#' result <- split_judgment(text)
#' print(result)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u7ecf\\u5ba1...\\u5211 \tab "经审理查明：盗窃行为。本院认为是犯罪。依照《刑法》第三十条，《刑事诉讼法》第二十条，判决如下:五年有期徒刑" \cr
#'}
#'
#' @importFrom stringi stri_match_all_regex stri_match_first_regex stri_detect_regex
#'
#' @export
split_judgment <- function(text) {

  if (!is.character(text) || length(text) != 1L || nchar(trimws(text)) == 0L) {
    return(list(FACTS = "", REASON = "", DECISION = ""))
  }

  # cutting rules
  ## facts
  fact_patterns <- c(
    "\u7ecf\\s*\u5ba1\\s*\u7406\\s*\u67e5\\s*\u660e[\uff1a:]",
    "\u73b0\\s*\u67e5\\s*\u660e[\uff1a:]",
    "\u672c\\s*\u9662\\s*\u67e5\\s*\u660e[\uff1a:]",
    "(?:\u516c\\s*\u8bc9\\s*\u673a\\s*\u5173|\u68c0\\s*\u5bdf\\s*\u9662)[^\u3002]{0,40}?\u6307\\s*\u63a7[\uff1a:]"
    )
  ## reasoning
  reason_pattern <- "\u672c\\s*\u9662\\s*\u8ba4\\s*\u4e3a[\uff0c,:]?"
  ## legal appendix: start
  decision_primary <- c("\u5224\\s*\u51b3\\s*\u5982\\s*\u4e0b[\uff1a:]", "\u88c1\\s*\u5b9a\\s*\u5982\\s*\u4e0b[\uff1a:]")
  ## legal appendix: end
  decision_fallback <- c("\u5982\\s*\u4e0d\\s*\u670d\\s*\u672c\\s*\u5224\\s*\u51b3", "\u5ba1\\s*\u5224\\s*\u957f[\uff1a:\\s]")

  # find_match
  find_first_match <- function(txt, patterns) {
    for (pat in patterns) {
      m <- regexpr(pat, txt, perl = TRUE)
      if (m != -1L) {
        start_pos <- as.integer(m)
        len <- attr(m, "match.length")
        py_start <- start_pos - 1L
        py_end <- py_start + len
        return(c(py_start, py_end))
      }
    }
    return(NULL)
  }

  # get each text
  fact_match <- find_first_match(text, fact_patterns)
  reason_match <- regexpr(reason_pattern, text, perl = TRUE)
  reason_start_py <- if (reason_match != -1L) reason_match - 1L else NULL
  # decision: two key parts
  decision_match <- find_first_match(text, decision_primary)
  if (is.null(decision_match)) {
    decision_match <- find_first_match(text, decision_fallback)
  }
  decision_start_py <- if (!is.null(decision_match)) decision_match[1L] else NULL

  # all empty: all in appendix
  if (is.null(fact_match) && is.null(reason_start_py) && is.null(decision_start_py)) {
    return(list(FACTS = "", REASON = "", DECISION = trimws(text)))
  }

  # get starting and ending point
  fact_start_py <- if (!is.null(fact_match)) fact_match[2L] else 0L
  fact_end_py <- if (!is.null(reason_start_py)) {
    reason_start_py
  } else if (!is.null(decision_start_py)) {
    decision_start_py
  } else {
    nchar(text)
  }

  reason_end_py <- if (!is.null(decision_start_py)) decision_start_py else nchar(text)

  # get facts
  facts <- ""
  if (!is.null(fact_match)) {
    start_R <- fact_start_py + 1L
    end_R <- fact_end_py
    if (start_R <= end_R) {
      facts <- substr(text, start_R, end_R)
    }
    facts <- trimws(facts)
  }

  # get reason
  reasoning <- ""
  if (!is.null(reason_start_py)) {
    start_R <- reason_start_py + 1L
    end_R <- reason_end_py
    if (start_R <= end_R) {
      reasoning <- substr(text, start_R, end_R)
    }
    reasoning <- trimws(reasoning)
  }

  # get decision
  decision <- ""
  if (!is.null(decision_start_py)) {
    start_R <- decision_start_py + 1L
    end_R <- nchar(text)
    if (start_R <= end_R) {
      decision <- substr(text, start_R, end_R)
    }
    decision <- trimws(decision)
  }

  # get legal article
  laws <- ""

  extract_laws_block <- function(txt) {
    if (is.null(txt) || nchar(trimws(txt)) == 0L) return("")
    pattern <- "\u4f9d\u7167\\s*((?:\u300a[^\u300b]+\u300b\\s*\u7b2c[\u96f6\u3007\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u5341\u767e\u5343\u4e070-9]+\u6761\\s*[\u3001\uff0c]?\\s*)+)"
    match <- regexec(pattern, txt, perl = TRUE)
    if (match[[1]][1] == -1) return("")
    start <- match[[1]][1]
    end <- start + attr(match[[1]], "match.length") - 1
    result <- substr(txt, start, end)
    trimws(result)
  }

  laws <- extract_laws_block(text)

  return(list(FACTS = facts, REASON = reasoning, DECISION = decision, LAW = laws))
}


#' Cutting Function - DataFrame
#'
#' This function is for cutting a set of documents.
#' It is an extension to Cutting Function - Single Case.
#' You can also use \code{for} or \code{lapply} and Cutting Function - Single Case (\code{split_judgment}) to replace this function.
#'
#' @param df a data frame. We recommend you to re-name the document column as \code{document}.
#' @param text_col the name of the column you want to cut. Default name is \code{"document"}. Put a character here.
#' @return three new columns added to the original \code{df}.
#' @examples
#' # build the dataframe
#' df <- data.frame(document = c("\u7ecf\u5ba1\u7406\u67e5\u660e\uff1a\u76d7\u7a83\u884c\u4e3a\u3002\u672c\u9662\u8ba4\u4e3a\u662f\u72af\u7f6a\u3002\u5224\u51b3\u5982\u4e0b:\u4e94\u5e74\u6709\u671f\u5f92\u5211", "\u7ecf\u5ba1\u7406\u67e5\u660e\uff1a\u62a2\u52ab\u884c\u4e3a\u3002\u672c\u9662\u8ba4\u4e3a\u4e0d\u6784\u6210\u72af\u7f6a\u3002\u5224\u51b3\u5982\u4e0b:\u65e0\u7f6a"))
#' result <- split_judgment_df(df, "document")
#' # print result
#' print(result)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u7ecf\\u5ba1...\\u5f92\\u5211 \tab "经审理查明：盗窃行为。本院认为是犯罪。判决如下:五年有期徒刑" \cr
#' \\u7ecf\\u5ba1...\\u65e0\\u7f6a \tab "经审理查明：抢劫行为。本院认为不构成犯罪。判决如下:无罪" \cr
#'}
#'
#' @export
split_judgment_df <- function(df, text_col = "document") {
  if (!text_col %in% names(df)) {
    stop("column ", text_col, " does not exist")
  }

  # apply split function to each row
  parts_list <- lapply(df[[text_col]], split_judgment)

  # get new row
  facts <- vapply(parts_list, `[[`, "FACTS", FUN.VALUE = character(1L))
  reasonings <- vapply(parts_list, `[[`, "REASON", FUN.VALUE = character(1L))
  decisions <- vapply(parts_list, `[[`, "DECISION", FUN.VALUE = character(1L))
  laws <- vapply(parts_list, `[[`, "LAW", FUN.VALUE = character(1L))

  # write into the df
  result <- df
  result[["FACTS"]] <- facts
  result[["REASON"]] <- reasonings
  result[["DECISION"]] <- decisions
  result[["LAWS"]] <- laws
  return(result)
}


#' Extract Judge Name Function
#'
#' This function can extract the main judge's name.
#' We will ignore other associate judges' names like other research has done.
#' We use four regular expressions to extract the name of judges.
#' It considers both situations when there is only one judge and there are over one judges.
#' If you need deal with a data.frame's texts, you may consider use \code{for} or \code{lapply} function.
#'
#' @param text a full text of judicial documents
#' @return the name of judge
#'
#' @examples
#' df <- data.frame(document = c("\u5ba1 \u5224 \u957f \u7ae0 \u4e09","\u7531\u5ba1\u5224\u5458\u6b27\u9633\u660e\u72ec\u4efb\u5ba1\u7406", "\u5ba1\u5224\u957f:\u5434\u5de5"))
#' for (i in 1:length(df$document)){
#'   df$name[i] <- extract_judge_name(df$document[i])
#'   }
#' # print the name
#' print(df$name)
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u5ba1...\\u4e09 \tab "审 判 长 章 三 " \cr
#' \\u7531...\\u7406 \tab "由审判员欧阳明独任审理" \cr
#' \\u5ba1...\\u5de5 \tab "审判长:吴工" \cr
#'}
#'
#' @export
extract_judge_name <- function(text) {
  if (!is.character(text) || nchar(text) == 0) return("")
  if (grepl("(?:\u5ba1\\s*\u5224\\s*\u957f|\u5ba1\\s*\u5224\\s*\u5458)[\uff1a:\\s]*[\uff38X*\u25a1\u00d7\u67d0]{1,4}", text, perl = TRUE)) return("")

  text <- gsub("<[^>]+>", " ", text)
  text <- gsub("[ \t\n\r]+", " ", text)

  patterns <- c(
    "\u5ba1\\s*\u5224\\s*\u957f[\uff1a:\\s]+([\u4e00-\u9fa5\\s\u3000]{2,8})",
    "\u5ba1\u3000\u5224\u3000\u957f[\uff1a:\\s\u3000]+([\u4e00-\u9fa5\\s\u3000]{2,8})",
    "\u7531\\s*\u5ba1\\s*\u5224\\s*\u5458([\u4e00-\u9fa5\\s\u3000]{2,8}?)(?=\u72ec\u4efb(?:\u5ba1\u7406)?)",
    "\u5ba1\\s*\u5224\\s*\u5458[\uff1a:\\s]+([\u4e00-\u9fa5\\s\u3000]{2,8})"
  )

  for (pat in patterns) {
    m <- regexec(pat, text, perl = TRUE)
    match <- regmatches(text, m)[[1]]
    if (length(match) >= 2) {
      name <- gsub("[\\s\u3000]", "", match[2], perl = TRUE)
      if (nchar(name) >= 2 && nchar(name) <= 4 && !grepl("\u4e3b\u5ba1|\u4e66\u8bb0|\u6cd5\u9662|\u5408\u8bae|\u4e8c\u3007|[0-9]", name))
        return(name)
    }
  }
  ""
}


#' Extract Defendant Extra-legal Information
#'
#' This function is to extract several common extra-legal information from the full document.
#' The logic behind this is: we first extract the certain block (a long sentence) from the full text.
#' Next, we extract all concerned features through designed regular expression.
#' If you need to extract other information, you can use \code{extract_defendant_block} function to get the certain block, and then use your own rule to extract certain features.
#'
#' @param text Full judgment text.
#' @param first_only Logical; if TRUE, only the first defendant (the primary defendant) is processed. The default value is \code{TRUE}.
#' @return A data frame with the 11 extra-legal variables. Including name, gender, born-year, born-date, age, ethnic info., education, hukou, hometown, occupation, hukou type
#'
#' @examples
#' text <- "\u516c\u8bc9\u673a\u5173\u5e7f\u4e1c\u7701\u5e7f\u5dde\u5e02\u4eba\u6c11\u68c0\u5bdf\u9662\u3002\u88ab\u544a\u4eba\u7ae5\u6842\u8fde\uff0c\u5973\uff0c1990\u5e746\u67082\u65e5\u51fa\u751f\uff0c\u6c49\u65cf\uff0c\u6587\u5316\u7a0b\u5ea6\u9ad8\u4e2d\uff0c\u65e0\u4e1a\uff0c\u9ad8\u4e2d\u5b66\u5386\uff0c\u6237\u7c4d\u6240\u5728\u5730\u5e7f\u4e1c\u7701\u4e91\u6d6e\u5e02\u4e91\u5b89\u533a\uff0c\u4f4f\u4e91\u6d6e\u5e02\u4e91\u57ce\u533a\u3002\u56e0\u672c\u6848\u4e8e2019\u5e743\u67082\u65e5\u88ab\u5211\u4e8b\u62d8\u7559\uff0c\u540c\u5e744\u67084\u65e5\u88ab\u902e\u6355\u30022021\u5e742\u670810\u65e5\u88ab\u53d6\u4fdd\u5019\u5ba1\u3002\u88ab\u544a\u4eba\u8881\u6797\u6797\uff0c\u5973\uff0c1992\u5e74\u51fa\u751f\u3002\u8fa9\u62a4\u4eba\u5362\u7fe0\u73b2\uff0c\u5e7f\u4e1c\u4e2d\u6cfd\u5f8b\u5e08\u4e8b\u52a1\u6240\u5f8b\u5e08\u30022019\u5e742\u6708\uff0c\u4e3a\u725f\u53d6\u975e\u6cd5\u5229\u76ca\uff0c\u88ab\u544a\u4eba\u7ae5\u6842\u8fde\u4ece\u5e7f\u5dde\u524d\u5f80\u8377\u5170\u738b\u56fd\u5e2e\u52a9\u4ed6\u4eba\u8fd0\u9001\u884c\u674e\u7bb1\u81f3\u6cf0\u738b\u56fd\u3002"
#' # consider both defendants
#' extract_defendant_extra(text, FALSE)
#' # consider only the primary defendant (the first one)
#' extract_defendant_extra(text, TRUE)
#' # if you want to extract all from a df, run:
#' # result_list <- lapply(df[[text_col]], function(txt) {
#' #   if (is.na(txt) || txt == "") return(NULL)
#' #   extract_defendant_extra(txt)
#' # })
#'
#' @section Unicode Meaning in Examples:
#' \tabular{ll}{
#'  Unicode \tab Meaning \cr
#' \\u516c...\\u3002 \tab "公诉机关广东省广州市人民检察院。被告人童桂连，女，1990年6月2日出生，汉族，文化程度高中，无业，高中学历，户籍所在地广东省云浮市云安区，住云浮市云城区。因本案于2019年3月2日被刑事拘留，同年4月4日被逮捕。2021年2月10日被取保候审。被告人袁林林，女，1992年出生。辩护人卢翠玲，广东中泽律师事务所律师。2019年2月，为牟取非法利益，被告人童桂连从广州前往荷兰王国帮助他人运送行李箱至泰王国。" \cr
#'}
#'
#' @section Some Variables' Values:
#' \tabular{ll}{
#'   Variable \tab Corresponding Value \cr
#'   Ethnic Info. \tab ethnic groups, e.g., Han, Zhuang, Manchu, ..., totally 56 values) \cr
#'   Education \tab education level categories (postgraduate, bachelor, associate, high school, middle school, primary, illiterate) \cr
#'   Occupation \tab unemployed, farmer, self-employed, worker, student, teacher, doctor, ..., totally 33 values \cr
#'   Hukou Type \tab agricultural, non-agricultural, urban, rural)\cr
#' }
#'
#' @export
extract_defendant_extra <- function(text, first_only = TRUE) {
  # use the function to get df
  df <- extract_defendant_info(text, first_only)
  # the df may contain some other info, delete them.
  rows_to_remove <- apply(df[, -1, drop = FALSE], 1, function(row) {
    all(is.na(row) | row == "")
  })
  df_clean <- df[!rows_to_remove, ]
  return(df_clean)
}

#' Extract Defendant Information Block
#'
#' This function is to extract the certain defendant information block (a long sentence) from the full text.
#' As we have mentioned, you can use this function combined with your own rule to extract certain features that we do not include.
#'
#' @param text Full judgment text.
#' @return A data frame with: \code{name} (defendant's name), \code{body} (the sentence without the name), \code{raw} (the full sentence), \code{judgement_year} (the year this judgement is produced).
#'
#' @export
extract_defendant_block <- function(text) {
  empty <- data.frame(
    name = character(0), body = character(0),
    raw = character(0), judgment_year = integer(0),
    stringsAsFactors = FALSE
  )
  if (.is_blank(text)) return(empty)
  ms <- stringi::stri_match_all_regex(text, .DEFENDANT_BLOCK_RE)[[1]]
  if (is.na(ms[1, 1])) return(empty)
  jy <- infer_judgment_year(text)
  data.frame(
    name = ms[, 2],
    body = ms[, 3],
    raw  = ms[, 1],
    judgment_year = rep(if (is.na(jy)) NA_integer_ else jy, nrow(ms)),
    stringsAsFactors = FALSE
  )
}





# ============================================================================
# Internal constants and helpers (not exported)
# ============================================================================


.FIELD_NAMES <- c("\u59d3\u540d", "\u6027\u522b", "\u51fa\u751f\u5e74", "\u51fa\u751f\u65e5\u671f", "\u5e74\u9f84", "\u6c11\u65cf",
                  "\u6587\u5316\u7a0b\u5ea6", "\u6237\u7c4d\u5730", "\u7c4d\u8d2f", "\u804c\u4e1a", "\u6237\u7c4d\u7c7b\u578b")

.empty_record <- function() {
  out <- as.list(rep("", length(.FIELD_NAMES)))
  names(out) <- .FIELD_NAMES
  out
}

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

.is_blank <- function(x) {
  is.null(x) || length(x) != 1L || is.na(x) || !is.character(x) || !nzchar(x)
}

.cn_to_arabic <- function(s) {
  CN_DIGIT_MAP <- c("\u96f6" = "0", "\u3007" = "0",
                    "\u4e00" = "1", "\u4e8c" = "2", "\u4e09" = "3", "\u56db" = "4",
                    "\u4e94" = "5", "\u516d" = "6", "\u4e03" = "7", "\u516b" = "8", "\u4e5d" = "9")
  for (i in seq_along(CN_DIGIT_MAP)) {
    s <- gsub(names(CN_DIGIT_MAP)[i], CN_DIGIT_MAP[[i]], s, fixed = TRUE)
  }
  s
}

.match_first <- function(text, pattern) {
  if (.is_blank(text)) return(NULL)
  m <- stringi::stri_match_first_regex(text, pattern)
  if (is.na(m[1, 1])) NULL else m
}

.cap <- function(m, k = 1L) {
  if (is.null(m)) return("")
  v <- m[1, k + 1L]
  if (is.na(v)) "" else v
}

# Regex patterns (copied from original)
.DEFENDANT_BLOCK_RE <- paste0(
  "(?s)\u88ab\u544a\u4eba[\\s\u3000]*",
  "([\u4e00-\u9fa5\u00b7\u2022.]{1,8})",
  "[\uff0c,\u3001\\s\u3000]",
  "(.*?)",
  "(?=\u88ab\u544a\u4eba|\u8fa9\u62a4\u4eba|\u516c\u8bc9\u673a\u5173|\u68c0\u5bdf\u9662.*?\u6307\u63a7|\u7ecf\u5ba1\u7406\u67e5\u660e|\u73b0\u7f6e\u62bc|\u88ab\u544a\u5355\u4f4d|$)"
)

.GENDER_RE <- "(?:\\A|[\uff0c,\u3001])\\s*(\u7537|\u5973)\\s*[\uff0c,\u3001]"

.BIRTH_FULL_RE <- paste0(
  "((?:19|20)\\d{2})\\s*\u5e74\\s*",
  "(?:(\\d{1,2})\\s*\u6708\\s*)?",
  "(?:(\\d{1,2})\\s*\u65e5)?\\s*",
  "(?:\u51fa\u751f|\u751f)"
)
.BIRTH_YEAR_ONLY_RE <- "((?:19|20)\\d{2})\\s*\u5e74\\s*\u751f"

.EXPLICIT_AGE_RE <- "(?:\u73b0\\s*\u5e74|\u5e74\\s*\u9f84)\\s*(\\d{1,3})\\s*\u5468\u5c81"

.ETHNICITY_RE <- paste0(
  "(\u6c49|\u58ee|\u6ee1|\u56de|\u82d7|\u7ef4\u543e\u5c14|\u5f5d|\u571f\u5bb6|\u85cf|\u8499\u53e4|\u5e03\u4f9d|\u4f97|\u7476|\u671d\u9c9c|\u767d|\u54c8\u5c3c|\u54c8\u8428\u514b|",
  "\u9ece|\u50a3|\u7572|\u50e6\u50f1|\u4e1c\u4e61|\u4ed0\u4ed4|\u62c9\u7976|\u4f6e|\u6c34|\u7eb3\u897f|\u7f8c|\u571f|\u4ef0\u4ed4|\u9521\u4f2f|\u67ef\u5c14\u514b\u5b5c|",
  "\u8fbe\u9c81\u5c14|\u666f\u9887|\u6bdb\u5357|\u6492\u62c9|\u5e03\u6d6e\u6717|\u5854\u5409\u514b|\u963f\u660c|\u666e\u7c73|\u90c1\u6e29\u514b|\u6012|\u4eac|\u57fa\u8bfa|\u5fb7\u660c|",
  "\u4fdd\u5b89|\u4fc4\u7f57\u65af|\u88d5\u56fa|\u4e4c\u5b5c\u522b\u514b|\u95e8\u5df4|\u90c1\u4f26\u6625|\u72ec\u9f99|\u5854\u5854\u5c14|\u8d6b\u54f2|\u9ad8\u5c71|\u73de\u5df4",
  ")\\s*\u65cf"
)

.EDUCATION_LEVELS <- list(
  list(label = "\u7814\u7a76\u751f", re = "\u7814\u7a76\u751f|\u7855\u58eb|\u535a\u58eb"),
  list(label = "\u672c\u79d1",   re = "\u5927\u5b66(?:\u672c\u79d1)?|\u672c\u79d1"),
  list(label = "\u5927\u4e13",   re = "\u5927\u4e13|\u4e13\u79d1"),
  list(label = "\u9ad8\u4e2d",   re = "\u9ad8\u4e2d|\u4e2d\u4e13|\u804c\u9ad8|\u6280\u6821"),
  list(label = "\u521d\u4e2d",   re = "\u521d\u4e2d"),
  list(label = "\u5c0f\u5b66",   re = "\u5c0f\u5b66"),
  list(label = "\u6587\u76f2",   re = "\u6587\u76f2|\u672a\u53d7\u6559\u80b2")
)

.HUKOU_RE <- "\u6237\u7c4d(?:\u5730|\u6240\u5728\u5730)?[\uff0c,\u3001:\uff1a\\s]*([^\uff0c,\u3001\u3002\uff1b;]{2,40})"
.NATIVE_PLACE_RE <- "\u7c4d\u8d2f[\uff0c,\u3001:\uff1a\\s]*([^\uff0c,\u3001\u3002\uff1b;]{2,40})"

.OCCUPATION_KEYWORDS <- c(
  "\u65e0\u4e1a", "\u65e0\u56fa\u5b9a\u804c\u4e1a", "\u5f85\u4e1a", "\u5931\u4e1a",
  "\u519c\u6c11", "\u52a1\u519c", "\u79cd\u690d\u4e1a",
  "\u4e2a\u4f53\u7ecf\u8425", "\u4e2a\u4f53\u5de5\u5546\u6237", "\u4e2a\u4f53", "\u7ecf\u5546",
  "\u5de5\u4eba", "\u5efa\u7b51\u5de5", "\u53f8\u673a", "\u53a8\u5e08", "\u670d\u52a1\u5458", "\u4fdd\u5b89", "\u4fdd\u6d01",
  "\u516c\u53f8\u804c\u5458", "\u804c\u5458", "\u5458\u5de5", "\u96c7\u5458",
  "\u5b66\u751f", "\u5728\u6821\u5b66\u751f",
  "\u6559\u5e08", "\u533b\u751f", "\u62a4\u58eb", "\u5f8b\u5e08", "\u4f1a\u8ba1",
  "\u5e72\u90e8", "\u516c\u52a1\u5458", "\u4e8b\u4e1a\u5355\u4f4d",
  "\u9000\u4f11"
)
.OCCUPATION_RE <- paste(.OCCUPATION_KEYWORDS, collapse = "|")

.HUKOU_TYPE_RE <- "(\u519c\u4e1a|\u975e\u519c\u4e1a|\u57ce\u9547|\u519c\u6751)\\s*(?:\u6237\u53e3|\u6237\u7c4d)"

.JUDGMENT_DATE_RE <- paste0(
  "([\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u96f6\u30070-9]{4})\\s*\u5e74\\s*",
  "[\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u53410-9]{1,2}\\s*\u6708\\s*",
  "[\u4e00\u4e8c\u4e09\u56db\u4e94\u516d\u4e03\u516b\u4e5d\u53410-9]{1,3}\\s*\u65e5"
)

# Internal functions
infer_judgment_year <- function(text) {
  if (.is_blank(text)) return(NA_integer_)
  ms <- stringi::stri_match_all_regex(text, .JUDGMENT_DATE_RE)[[1]]
  if (is.na(ms[1, 1])) return(NA_integer_)
  for (i in rev(seq_len(nrow(ms)))) {
    raw <- ms[i, 2]
    if (is.na(raw)) next
    digits <- .cn_to_arabic(raw)
    if (grepl("^\\d{4}$", digits)) {
      y <- as.integer(digits)
      if (!is.na(y) && y >= 1990L && y <= 2100L) return(y)
    }
  }
  NA_integer_
}

.extract_gender <- function(body) .cap(.match_first(body, .GENDER_RE), 1L)

.extract_birth <- function(body) {
  m <- .match_first(body, .BIRTH_FULL_RE)
  if (!is.null(m)) {
    yr <- suppressWarnings(as.integer(m[1, 2]))
    mo <- suppressWarnings(as.integer(m[1, 3]))
    dy <- suppressWarnings(as.integer(m[1, 4]))
    if (!is.na(mo) && !is.na(dy) && !is.na(yr)) {
      iso <- sprintf("%04d-%02d-%02d", yr, mo, dy)
      d <- suppressWarnings(as.Date(iso, format = "%Y-%m-%d"))
      if (!is.na(d) && format(d, "%Y-%m-%d") == iso) {
        return(list(year = yr, date = iso))
      }
      return(list(year = yr, date = ""))
    }
    return(list(year = yr, date = ""))
  }
  m <- .match_first(body, .BIRTH_YEAR_ONLY_RE)
  if (!is.null(m)) {
    return(list(year = suppressWarnings(as.integer(m[1, 2])), date = ""))
  }
  list(year = NA_integer_, date = "")
}

.extract_age <- function(body, birth_year, judgment_year) {
  v <- .cap(.match_first(body, .EXPLICIT_AGE_RE), 1L)
  if (nzchar(v)) return(v)
  if (!is.null(birth_year) && !is.na(birth_year) &&
      !is.null(judgment_year) && !is.na(judgment_year)) {
    return(as.character(judgment_year - birth_year))
  }
  ""
}

.extract_ethnicity <- function(body) {
  v <- .cap(.match_first(body, .ETHNICITY_RE), 1L)
  if (nzchar(v)) paste0(v, "\u65cf") else ""
}

.extract_education <- function(body) {
  if (.is_blank(body)) return("")
  for (lvl in .EDUCATION_LEVELS) {
    if (stringi::stri_detect_regex(body, lvl$re)) return(lvl$label)
  }
  ""
}

.extract_hukou        <- function(body) trimws(.cap(.match_first(body, .HUKOU_RE), 1L))
.extract_native_place <- function(body) trimws(.cap(.match_first(body, .NATIVE_PLACE_RE), 1L))

.extract_occupation <- function(body) {
  if (.is_blank(body)) return("")
  m <- stringi::stri_match_first_regex(body, .OCCUPATION_RE)
  if (is.na(m[1, 1])) "" else m[1, 1]
}

.extract_hukou_type <- function(body) .cap(.match_first(body, .HUKOU_TYPE_RE), 1L)

.fields_from_block <- function(name, body, judgment_year) {
  by <- .extract_birth(body)
  list(
    "\u59d3\u540d"       = if (is.null(name) || is.na(name)) "" else as.character(name),
    "\u6027\u522b"       = .extract_gender(body),
    "\u51fa\u751f\u5e74"     = if (is.na(by$year)) "" else as.character(by$year),
    "\u51fa\u751f\u65e5\u671f"   = by$date,
    "\u5e74\u9f84"       = .extract_age(body, by$year, judgment_year),
    "\u6c11\u65cf"       = .extract_ethnicity(body),
    "\u6587\u5316\u7a0b\u5ea6"   = .extract_education(body),
    "\u6237\u7c4d\u5730"     = .extract_hukou(body),
    "\u7c4d\u8d2f"       = .extract_native_place(body),
    "\u804c\u4e1a"       = .extract_occupation(body),
    "\u6237\u7c4d\u7c7b\u578b"   = .extract_hukou_type(body)
  )
}

extract_extralegal_vars <- function(block, judgment_year = NA_integer_) {
  if (is.data.frame(block)) {
    if (nrow(block) == 0L) {
      out <- as.data.frame(.empty_record(), stringsAsFactors = FALSE)
      return(out[0, , drop = FALSE])
    }
    rows <- lapply(seq_len(nrow(block)), function(i) {
      jy_i <- if ("judgment_year" %in% names(block))
        block$judgment_year[i] else judgment_year
      nm_i <- if ("name" %in% names(block)) block$name[i] else ""
      bd_i <- if ("body" %in% names(block)) block$body[i] else ""
      .fields_from_block(nm_i, bd_i, jy_i)
    })
    do.call(rbind, lapply(rows, function(r)
      as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)))
  } else if (is.list(block) && !is.null(block$body)) {
    jy <- if (!is.null(block$judgment_year) && !is.na(block$judgment_year))
      block$judgment_year else judgment_year
    .fields_from_block(block$name %||% "", block$body, jy)
  } else if (is.character(block) && length(block) == 1L && !is.na(block)) {
    m <- .match_first(block, .DEFENDANT_BLOCK_RE)
    if (!is.null(m)) {
      .fields_from_block(m[1, 2], m[1, 3], judgment_year)
    } else {
      .fields_from_block("", block, judgment_year)
    }
  } else {
    .empty_record()
  }
}

extract_defendant_info <- function(text, first_only = TRUE) {
  blocks <- extract_defendant_block(text)
  if (nrow(blocks) == 0L) {
    out <- as.data.frame(.empty_record(), stringsAsFactors = FALSE)
    return(if (first_only) out else out[0, , drop = FALSE])
  }
  if (first_only) blocks <- blocks[1, , drop = FALSE]
  extract_extralegal_vars(blocks)
}


