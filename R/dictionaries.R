#' Known Likert Response Scales
#'
#' Internal dictionary of commonly used Likert-style response scales.
#' Used by [match_likert()] to identify and order survey response options.
#'
#' @format A named list of character vectors. Each list element represents
#'   a known response scale in its intended display order.
#'
#' @keywords internal
likert_dictionary <- list(
  # Agreement ------------------------------------------------

  Agreement = c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree"
  ),

  Agreement2 = c(
    "Strongly disagree",
    "Disagree a little",
    "Neither agree nor disagree",
    "Agree a little",
    "Strongly agree"
  ),

  Agreement3 = c(
    "Strongly disagree",
    "Somewhat disagree",
    "Neutral",
    "Somewhat agree",
    "Strongly agree"
  ),

  Agreement4 = c(
    "Strongly disagree",
    "Somewhat disagree",
    "Neither agree nor disagree",
    "Somewhat agree",
    "Strongly agree"
  ),

  Agreement5 = c(
    "Disagree completely",
    "Disagree somewhat",
    "Unsure",
    "Agree somewhat",
    "Agree completely"
  ),

  # Familiarity / awareness ---------------------------------

  Familiarity = c(
    "Very unfamiliar",
    "Somewhat unfamiliar",
    "Neutral",
    "Somewhat familiar",
    "Very familiar"
  ),

  Familiarity2 = c(
    "Not at all familiar",
    "Slightly familiar",
    "Somewhat familiar",
    "Very familiar",
    "Extremely familiar"
  ),

  Awareness = c(
    "Never heard of it",
    "Heard of it but know almost nothing about it",
    "Know a little about it",
    "Know a fair amount about it",
    "Know a lot about it"
  ),

  # Satisfaction --------------------------------------------

  Satisfaction = c(
    "Very dissatisfied",
    "Dissatisfied",
    "Neutral",
    "Satisfied",
    "Very satisfied"
  ),

  Satisfaction2 = c(
    "Very dissatisfied",
    "Somewhat dissatisfied",
    "Neither satisfied nor dissatisfied",
    "Somewhat satisfied",
    "Very satisfied"
  ),

  # Importance ----------------------------------------------

  Importance = c(
    "Not important",
    "Slightly important",
    "Neutral",
    "Important",
    "Very important"
  ),

  Importance2 = c(
    "Not at all important",
    "Slightly important",
    "Moderately important",
    "Very important",
    "Extremely important"
  ),

  # Frequency ------------------------------------------------

  Frequency = c(
    "Never",
    "Rarely",
    "Sometimes",
    "Often",
    "Always"
  ),

  Frequency2 = c(
    "Rarely / Never",
    "Less than once a week",
    "Once a week",
    "Several times a week",
    "Daily"
  ),

  Frequency3 = c(
    "Never",
    "Rarely",
    "Sometimes",
    "Often",
    "All the time"
  ),

  Frequency4 = c(
    "Never",
    "Once or twice",
    "A few times",
    "Often",
    "Very often"
  ),

  # Likelihood ----------------------------------------------

  Likelihood = c(
    "Very unlikely",
    "Unlikely",
    "Neutral",
    "Likely",
    "Very likely"
  ),

  Likelihood2 = c(
    "Very unlikely",
    "Somewhat unlikely",
    "Neutral",
    "Somewhat likely",
    "Very likely"
  ),

  Likelihood3 = c(
    "Not at all likely",
    "Slightly likely",
    "Moderately likely",
    "Very likely",
    "Extremely likely"
  ),

  # Confidence ----------------------------------------------

  Confidence = c(
    "Much less confident",
    "Somewhat less confident",
    "About the same",
    "Somewhat more confident",
    "Much more confident"
  ),

  Confidence2 = c(
    "Not at all confident",
    "Slightly confident",
    "Moderately confident",
    "Very confident",
    "Extremely confident"
  ),

  # Trust ----------------------------------------------------

  Trust = c(
    "Do not trust at all",
    "Trust a little",
    "Trust somewhat",
    "Trust a lot",
    "Trust completely"
  ),

  Trust2 = c(
    "Very untrustworthy",
    "Somewhat untrustworthy",
    "Neutral",
    "Somewhat trustworthy",
    "Very trustworthy"
  ),

  # Safety ---------------------------------------------------

  Safety = c(
    "Very unsafe",
    "Somewhat unsafe",
    "Neutral",
    "Somewhat safe",
    "Very safe"
  ),

  # Comfort --------------------------------------------------

  Comfort = c(
    "Very uncomfortable",
    "Somewhat uncomfortable",
    "Neutral",
    "Somewhat comfortable",
    "Very comfortable"
  ),

  # Interest -------------------------------------------------

  Interest = c(
    "Not at all interested",
    "Slightly interested",
    "Moderately interested",
    "Very interested",
    "Extremely interested"
  ),

  Interest2 = c(
    "Very uninterested",
    "Somewhat uninterested",
    "Neutral",
    "Somewhat interested",
    "Very interested"
  ),

  # Appeal ---------------------------------------------------

  Appeal = c(
    "Not at all appealing",
    "Slightly appealing",
    "Moderately appealing",
    "Very appealing",
    "Extremely appealing"
  ),

  Appeal2 = c(
    "Very unappealing",
    "Somewhat unappealing",
    "Neutral",
    "Somewhat appealing",
    "Very appealing"
  ),

  # Relevance ------------------------------------------------

  Relevance = c(
    "Not at all relevant",
    "Slightly relevant",
    "Moderately relevant",
    "Very relevant",
    "Extremely relevant"
  ),

  # Believability -------------------------------------------

  Believability = c(
    "Not at all believable",
    "Slightly believable",
    "Moderately believable",
    "Very believable",
    "Extremely believable"
  ),

  Believability2 = c(
    "Very unbelievable",
    "Somewhat unbelievable",
    "Neutral",
    "Somewhat believable",
    "Very believable"
  ),

  # Credibility ---------------------------------------------

  Credibility = c(
    "Not at all credible",
    "Slightly credible",
    "Moderately credible",
    "Very credible",
    "Extremely credible"
  ),

  # Persuasiveness ------------------------------------------

  Persuasiveness = c(
    "Not at all persuasive",
    "Slightly persuasive",
    "Moderately persuasive",
    "Very persuasive",
    "Extremely persuasive"
  ),

  # Clarity --------------------------------------------------

  Clarity = c(
    "Not at all clear",
    "Slightly clear",
    "Moderately clear",
    "Very clear",
    "Extremely clear"
  ),

  Clarity2 = c(
    "Very unclear",
    "Somewhat unclear",
    "Neutral",
    "Somewhat clear",
    "Very clear"
  ),

  # Ease / difficulty ---------------------------------------

  Ease = c(
    "Very difficult",
    "Somewhat difficult",
    "Neither easy nor difficult",
    "Somewhat easy",
    "Very easy"
  ),

  Difficulty = c(
    "Very easy",
    "Somewhat easy",
    "Neither easy nor difficult",
    "Somewhat difficult",
    "Very difficult"
  ),

  # Value ----------------------------------------------------

  Value = c(
    "Very poor value",
    "Somewhat poor value",
    "Neutral",
    "Somewhat good value",
    "Very good value"
  ),

  Value2 = c(
    "Not at all valuable",
    "Slightly valuable",
    "Moderately valuable",
    "Very valuable",
    "Extremely valuable"
  ),

  # Uniqueness / distinctiveness ----------------------------

  Uniqueness = c(
    "Not at all unique",
    "Slightly unique",
    "Moderately unique",
    "Very unique",
    "Extremely unique"
  ),

  Distinctiveness = c(
    "Not at all distinctive",
    "Slightly distinctive",
    "Moderately distinctive",
    "Very distinctive",
    "Extremely distinctive"
  ),

  # Likeability ---------------------------------------------

  Likeability = c(
    "Dislike very much",
    "Dislike somewhat",
    "Neither like nor dislike",
    "Like somewhat",
    "Like very much"
  ),

  # Recommendation / advocacy -------------------------------

  Recommendation = c(
    "Very unlikely",
    "Somewhat unlikely",
    "Neutral",
    "Somewhat likely",
    "Very likely"
  ),

  # Purchase / consideration --------------------------------

  Consideration = c(
    "Definitely would not consider",
    "Probably would not consider",
    "Might or might not consider",
    "Probably would consider",
    "Definitely would consider"
  ),

  PurchaseIntent = c(
    "Definitely would not buy",
    "Probably would not buy",
    "Might or might not buy",
    "Probably would buy",
    "Definitely would buy"
  ),

  # Emotional valence ---------------------------------------

  Positivity = c(
    "Very negative",
    "Somewhat negative",
    "Neutral",
    "Somewhat positive",
    "Very positive"
  ),

  # Concern / worry -----------------------------------------

  Concern = c(
    "Not at all concerned",
    "Slightly concerned",
    "Moderately concerned",
    "Very concerned",
    "Extremely concerned"
  ),

  Worry = c(
    "Not at all worried",
    "Slightly worried",
    "Moderately worried",
    "Very worried",
    "Extremely worried"
  ),

  # Excitement ----------------------------------------------

  Excitement = c(
    "Not at all excited",
    "Slightly excited",
    "Moderately excited",
    "Very excited",
    "Extremely excited"
  ),

  # Political ideology --------------------------------------

  PID = c(
    "Very conservative",
    "Lean conservative",
    "Moderate / Middle of the road",
    "Lean liberal",
    "Very liberal"
  )
)
