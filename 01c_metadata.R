#' @title ADJECTIVES METADATA
#' @name 01a_adjSL.R
#' @description This data set contains corpus-based adjectives data for 446 sentences from newspaper data and several potential predictors.
#' @docType data
#'
#' @format A data frame with 446 observations/cases and 27 columns:
#' \describe{
#'   \item{NO}{Numeric: a unique identifier for each data point [150557, 234528]}
#'   \item{FILE}{Numeric: The filename of the corpus text from which the adjective was extracted [4703076, 41855485]}
#'   \item{COMPARISON}{Factor: the kind of comparison and the dependent variable, with levels "analytic", "synthetic"}
#'   \item{ADJ_LEMMA}{Factor: the lemma of the extracted adj}
#'   \item{CORPUS}{Factor: the corpus from which the adj was extracted, with levels "NOW2020", "SAVE2020"}
#'   \item{YEAR}{Numeric: publication year of the newspaper article, in this case only 2020}
#'   \item{VARIETY}{Factor: the variety of the adj, British or Sri Lankan English, with levels "BrE", "LK"}
#'   \item{NEWSPAPER}{Factor: the newspaper from which the adj was extracted, with levels "Daily Mail", "Daily Mirror", "Daily News", "Independent"}
#'   \item{WORD_COUNT}{Numeric: the number of words in the newspaper article [140, 5629]}
#'   \item{FORM}{Factor: the form of the comparison, with levels "comparative", "superlative"}
#'   \item{ADJ_LEN}{Numeric: The length of the adjective lemma in characters [3, 13]}
#'   \item{READABILITY}{Numeric: the readability score of the newspaper text, computed with a pca based on the quanteda package [-492.56, 4743.51]}
#'   \item{LEXDIV}{Numeric: the lexical diversity score of the newspaper text, computed with a pca based on the quanted package [-203.241, 61.193]}
#'   \item{STRESS_LAST_SYLL}{Factor: whether the lemma of the adjective is stressed on the last syllable, no or yes, with levels "n", "y"}
#'   \item{PERSIST_COMP}{Factor: the comparison of the preceding adj, if any, with levels "analytic", "none", "synthetic"}
#'   \item{PERSIST_FORM}{Factor: the form of the preceding adj, if any, with levels "comparative", "none", "superlative"}
#'   \item{PERSIST_DIST}{Numeric: the distance between the current and the previous adjective in characters excluding white spaces}
#'   \item{SYNT_FUN}{Factor: the syntactic function, either attributive, nominal, predicative or post-nominal, with levels "a", "n", "p", "pn"}
#'   \item{ADVMOD}{Factor: whether the adj is adverbially modified, no or yes, with levels "n", "y"}
#'   \item{COMPL}{Factor: whether the adj is followed by a complement, either to-infinitive, none, prepositional phrase, or a than-phrase, with levels "i", "n", "p", "t"}
#'   \item{ZIPF_FREQ}{Numeric: the zipfian frequency of the adjective in the corpus [2.438, 6.377]}
#'   \item{DPNOFREQ}{Numeric: the dispersion (Gries's DP) of the adjective in the corpus [0.00009, 0.2237]}
#'   \item{RHY_SCORE_POS}{Numeric: the score of the rhythmic alternation of stressed and unstressed syllables the more alternating the stress pattern the lower the score, monosyllabic words were annotated based on their part of speech tag [0, 1]}
#'   \item{RHY_SCORE_ALT_POS}{Numeric: the score of the rhythmic alternation of the alternative adjective that wasn't chosen by the author [0, 1]}
#'   \item{SEG_SCORE}{Numeric: the score of the segment alternation of consonant and vowels; the more alternating the segment pattern the lower the score [0, 0.625]}
#'   \item{SEG_SCORE_ALT}{Numeric: the score of the segment alternation of the alternative adjective that wasn't chosen by the author [0, 0.5]}
#'   \item{FINAL_SEGMENT}{Numeric: a score measuring the similarity of the lemma's final segment and the synthetic ending [0, 1]}