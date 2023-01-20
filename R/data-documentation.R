#' @title Schmidt (2007)
#'
#' @description Data from a study by Schmidt (2007). The variables are as
#'   follows:
#'
#'   \itemize{ \item \code{Behavior_type}. Outcome measure description
#'   (disruptive behavior or on task behavior). \item \code{Procedure}. The
#'   observation recording procedure used to measure the outcome. \item
#'   \code{Metric}. The metric in which the outcome measurement is expressed
#'   ("count" for natural counts; "percentage" for percentage of intervals)
#'   \item \code{Session_length}. Length (in minutes) of the observation
#'   sessions \item \code{Interval_length}. If an interval method was used, the
#'   length of the intervals (in seconds); \code{NA} otherwise. \item
#'   \code{Case_Pseudonym}. Case Pseudonym provided by the authors. \item
#'   \code{Session_number}. Within-series session-number. \item \code{Phase}.
#'   Label for each unique phase (e.g., A1 is the first baseline phase, B2 is
#'   the second treatment phase). \item \code{Condition}. Label indicating
#'   whether the outcome is in the baseline (A) or treatment (B) phase. \item
#'   \code{Outcome}. Outcome measurement. \item \code{Phase_num}. Indicator for
#'   each pair of baseline and treatment phases. \item \code{direction}.
#'   Direction of therapeutic improvement for the outcome. \item
#'   \code{n_Intervals}. If an interval method was used, the total number of
#'   intervals; \code{NA} otherwise. }
#'
#' @note Data were reconstructed from figures in the source document.
#'   Consequently, outcome measurements in this dataset include additional error
#'   from the reconstruction process and may not exactly match the original
#'   source data.
#'
#'   In earlier versions of the package (0.5.0 and earlier), the
#'   \code{Case_Pseudonym} variable contained incorrect labels for each case,
#'   which did not match the labels in the source document. Specifically, Faith
#'   was incorrectly labeled as Albert; Lilly was incorrectly labeled as Faith;
#'   and Albert was incorrectly labeled as Lilly.
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 172 rows and 13 variables
#' @name Schmidt2007
#' @source Schmidt, A. C. (2007). The effects of a group contingency on group
#'   and individual behavior in an urban first-grade classroom.  Masters Thesis,
#'   University of Kansas, Department of Applied Behavioral Sciences. ProQuest
#'   Dissertations & Theses Global, thesis number 1443719.
#'   https://kuscholarworks.ku.edu/bitstream/handle/1808/32097/Schmidt_Anna_C_2007_5349292.pdf
#'   
NULL

#' @title McKissick et al. (2010)
#' 
#' @description  Disruptive behavior data from a study by McKissick et al. (2010). All data were collected
#' via event counting. The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Case_Pseudonym}. Case Pseudonym provided by the authors.
#'   \item \code{Session_number}. Within-series session-number
#'   \item \code{Condition}. Describes whether the outcome is in the baseline (A) or treatment (B) phase.
#'   \item \code{Outcome}. Value for the outcome.
#'   \item \code{Session_length}. Length of the observation session.
#'   \item \code{Procedure}. The metric in which the outcome measurement is expressed, all "count".
#'   \item \code{Session_length}. The length of the observation session.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 35 rows and 4 variables
#' @name McKissick
#' @source McKissick, C., Hawkins, R. O., Lentz, F. E., Hailley, J., & McGuire, S. (2010). 
#' Randomizing multiple contingency components to decrease disruptive behaviors and increase 
#' student engagement in an urban second-grade classroom. \emph{Psychology in the Schools, 47}(9), 
#' 944–959. https://doi.org/10.1002/pits.20516
NULL

#' @title Shogren et al. (2004)
#' 
#' @description Data from a systematic review by Shogren et al. (2004) 
#' on the effects of choice-making interventions. 
#' These data were compiled and re-analyzed in Pustejovsky (2015).
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Study}. An ID for each study in the systematic review.
#'   \item \code{Case}. Case Pseudonym provided by the authors.
#'   \item \code{Measure}. Type of behavior observed as the outcome measure
#'   \item \code{Phase}. Phase indicator, baseline phase is "No Choice" and treatment phase is "Choice."
#'   \item \code{Percentage}. For those outcomes measured as percentage, outcomes value. \code{NA} for count outcomes.
#'   \item \code{Observed}. For those outcomes measured as count, outcome value. \code{NA} for percentage outcomes.
#'   \item \code{Possible}. For counts out of a maximum, lists the maximum value. \item \code{Recording_procedure}  Recording procedure. CDR = "Continuous Duration Recording", EC = "Event Counting", "MTS = "Momentary Time Sampling", and PIR = "Partial Interval Recording."
#'   \item \code{Session_length}. Length of the observation session in minutes.
#'   \item \code{interval_length}. Length of the observation intervals for data observed using MTS or PIR.
#'   \item \code{outcome}. Value for the outcome for all outcome types.
#'   \item \code{direction}. Direction of therapeutic improvement for the outcome.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 634 rows and 15 variables
#' @name Shogren
#' @source Shogren, K. A., Faggella-Luby, M. N., Bae, S. J., & Wehmeyer, M. L. (2004). 
#' The effect of choice-making as an intervention for problem behavior. \emph{Journal 
#' of Positive Behavior Interventions, 6}(4), 228–237. 
#'
#' @references Pustejovsky, J.E. (2015). Measurement-comparable effect 
#' sizes for single-case studies of free-operant behavior. 
#' \emph{Psychological Methods, 20}(3), 342–359.
#' 

NULL

#' @title Thorne and Kamps (2008)
#' 
#' @description Data from an ABAB design conducted by Thorne and Kamps (2008). 
#' These data were used as an example in Swan and Pustejovsky (2017). Academic engagement 
#' was collected via continuous recording (marked as "other") and inappropriate 
#' verbalizations were collected via event counting (marked as "count").
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Measure}. Outcome measure description (academic engagement or inappropriate verbalizations).
#'   \item \code{Case}. Participant identifier.  
#'   \item \code{Session_number}. Measurement occasion.
#'   \item \code{Outcome}. Outcome scores
#'   \item \code{Trt}. Treatment indicators.
#'   \item \code{Session_length}. Length of the observation session.
#'   \item \code{Measure}. The metric in which the outcome measurement is expressed (count or other).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 776 rows and 5 variables
#' @name Thorne
#' @source Thorne, S., & Kamps, D. (2008). The effects of a group contingency intervention on academic engagement and problem 
#' behavior of at-risk students. \emph{Behavior Analysis in Practice, 1}(2), 12-18.
#' 
#' @references Swan, D. M., & Pustejovsky, J. E. (2017). 
#' A gradual effects model for single-case designs. http://doi.org/10.17605/OSF.IO/GAXRV
NULL

#' @title Schmidt and Stichter (2012)
#' 
#' @description Data from an ABAB design conducted by Schmidt and 
#' Stichter (2012). All data were collected via continuous recording.
#'  The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Case}. Participant identifier.
#'   \item \code{Behavior}. Behavior type (Conversation, Initiations, or Responses).
#'   \item \code{Trt} Treatment indicators.
#'   \item \code{Outcome}. Outcome scores.
#'   \item \code{Session_num}. Measurement occasion.
#'   \item \code{Session_length} Length of the observation session.
#'   \item \code{Procedure}  The metric in which the outcome measurement is expressed, all "other".
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 180 rows and 5 variables
#' @name Schmidt2012
#' @source Schmidt, C., & Stichter, J. P. (2012). The use of peer-mediated interventions to 
#' promote the generalization of social competence for adolescents with high-functioning autism 
#' and Asperger's syndrome. \emph{Exceptionality}, 20(2), 94-113. doi:10.1080/09362835.2012
NULL

#' @title Wright & McCathren (2012)
#' 
#' @description Data from a multiple baseline design conducted by Wright and
#'   McCathren (2012), which evaluated the effects of two types of social story
#'   interventions (a basic social story and a modified social story) on the
#'   behavior of four children with autism. Both dependent variables were
#'   measured using frequency counting for 10 minute observation sessions. The
#'   variables are as follows:
#' 
#' \itemize{
#'   \item \code{Participant} Participant identifier.
#'   \item \code{Session} Session number.
#'   \item \code{Condition} Phase of the design (baseline, intervention A, or intervention B.
#'   \item \code{Problem_behavior} Outcome scores.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 97 rows and 5 variables
#' @name Wright2012
#' @source Wright, L. A., & McCathren, R. B. (2012). 
#' Utilizing social stories to increase prosocial behavior and reduce 
#' problem behavior in young children with autism. 
#' \emph{Child Development Research}, 2012, 1-13. doi:10.1155/2012/357291
NULL

#' @title Crozier and Tincani (2015)
#' 
#' @description Data from an ABAB design that evaluated a social story intervention 
#' on the disruptive behavior of a preschool child with autism. The dependent variable, 
#' number of disruptions, was measured using frequency counting for a thirty minute observation
#' session. The variables are as follows:
#' 
#' \itemize{
#' \item \code{session}. Within-series session-number.
#' \item \code{phase}. Lable for each unique phase (e.g., A1 is the first baseline phase, B2 is the second treatment phase).
#' \item \code{score}. Value for the outcome.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 24 rows and 3 variables
#' @name Crozier2005
#' @source Crozier, S., & Tincani, M. J. (2005). Using a modified social story to decrease disruptive behavior of a child with autism. 
#' Focus on Autism and Other Developmental Disabilities, 20(3), 150-157.
NULL

#' @title Olszewski, et al. (2017)
#' 
#' @description Data from a multiple baseline design across behavior looking at four fluency measures (blends, segmenting, first part ID, first sound ID).
#' The dependent variable is a score on a subtest, with a maximum score of 20.
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{behavior}. Outcome measure description (blends, segmenting, first part ID, or first sound ID)
#'   \item \code{session}. Within-series session-number
#'   \item \code{phase}. Label for each unique phase (A for baseline, B for treatment)
#'   \item \code{score}. Value for the outcome. 
#'   \item \code{Include}. Value for inclusion in POGO calculation (0 if included in article, 1 if not included)
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 72 rows and 4 variables
#' @name Olszewski2017
#' @source Olszewski, A., Soto, X., & Goldstein, H. (2017). Modeling alphabet skills as instructive feedback within a phonological awareness intervention. American Journal of Speech-Language Pathology, 26(3), 769–790. 
NULL

#' @title English, et al. (1997)
#' 
#' @description Data from a multiple baseline design across participants conducted by English, et al. (1997), which evaluated the effects of peer-training on interaction skills. This dependent variable, number of communicative acts directed by the participant to their peers, was measured using frequency counting for a 10-minute observation session. 
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{case}. Participant pseudonym given by author
#'   \item \code{session}. Within-series session-number
#'   \item \code{phase}. Label for each unique phase (A for baseline, B for treatment)
#'   \item \code{score}. Value for the outcome. 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 59 rows and 4 variables
#' @name English1997
#' @source English, K., Goldstein, H., Shafer, K., & Kaczmarek, L. (1997). Promoting interactions among preschoolers with and without disabilities: Effects of a buddy skills-training program. Exceptional Children, 63(2), 229–243. 
NULL

#' @title Facon, et al. (2008)
#' 
#' @description Data from a changing criterion design conducted by Facon, et al. (2008), which evaluated two operant learning procedures, shaping and fading, for treating selective mutism. The dependent variable, average speech loudness in dB, was measured during 15-minute observation sessions.
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{session}. Within-series session-number
#'   \item \code{phase}. Label for each unique phase (A for baseline, each following for a change in criteria.
#'   \item \code{score}. Value for the outcome. 
#'   \item \code{criterion}. The fixed criterion value for each of the phases. The student moved to the next criterion when 80% of their utterances are equal or greater than the current criterion during at least 3 consecutive sessions. The first criterion was set at 43 dB with each following criterion equal to the mean score for the five best trials in the previous phase.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 49 rows and 4 variables
#' @name Facon2008
#' @source Facon, B., Sahiri, S., & Riviѐre, V. (2008). A controlled single-case treatment of severe long-term selective mutism in a child with mental retardation. Behavior Therapy, 39, 313–321. 
NULL

#' @title Spencer et al. (2012)
#' 
#' @description Data from a repeated acquisition single-case experimental design 
#' evaluating vocabulary knowledge using an expressive definition task.
#' The variables are as follows:
#' 
#' \itemize{
#' \item \code{Observation}. Participant number.
#' \item \code{Book}. Book used for evaluation.
#' \item \code{Pre}. Pre-intervention score on the expressive definition task.
#' \item \code{Post}. Post-intervention score on the expressive definition task.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 81 rows and 4 variables
#' @name Spencer2012
#' @source Spencer, E., Goldstein, H., Sherman, A., Noe, S., Tabbah, R., Ziolkowski, R., & Schneider, N. (2012). 
#' Effects of an automated vocabulary and comprehension intervention: An early efficacy study. Journal of Early Intervention, 
#' 34, 195-221. https://doi.org/10.1177/1053815112471990
NULL

#' @title Kelley et al. (2015)
#' 
#' @description Data from a randomized control trial with embedded repeated acquisition single- case experimental design evaluating vocabulary knowledge using an expressive definition task.
#' The variables are as follows:
#' 
#' \itemize{
#' \item \code{condition}. Indicates whether control or treatment group.
#' \item \code{observation}. Identifier for participant
#' \item \code{unit}. Classroom unit.
#' \item \code{Pre}. Pre-intervention score on the expressive definition task. 
#' \item \code{Post}. Post-intervention score on the expressive definition task. 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 54 rows and 5 variables
#' @name Kelley2015
#' @source Kelley, E. S., Goldstein, H., Spencer, T., & Sherman, A. (2015). Effects of automated Tier 2 storybook intervention on vocabulary and comprehension learning in preschool children with limited oral language skills. Early Childhood Research Quarterly, 31, 47–61. https://doi.org/10.1016/j.ecresq.2014.12.004
NULL

#' @title Peters-Sanders et al. (2020)
#' 
#' @description Data from a Repeated acquisition single-case experimental design evaluating vocabulary knowledge using an expressive definition task.
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{observation}. Identifier for participant
#'   \item \code{Book}.Book used for evaluation.
#'   \item \code{Pre}. Pre-intervention score on the expressive definition task. 
#'   \item \code{Post}. Post-intervention score on the expressive definition task. 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 153 rows and 4 variables
#' @name Peters2020
#' @source Peters-Sanders, L., Kelley, E., Haring, C., Madsen, K., Soto, X., Seven, Y., Hull, K., & Goldstein, H. (2020). Moving forward four words at a time: Effects of a supplemental preschool vocabulary. Language, Speech, and Hearing Services in Schools, 51, 165-175. https://doi.org/10.1044/2019_LSHSS-19-00029
NULL

#' @title Dennis & Whalon, (2021)
#' 
#' @description Data from a Repeated acquisition single-case experimental design evaluating vocabulary knowledge using an expressive definition task.
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{Participant}. Participant identifier.
#'   \item \code{Condition}. Indicates condition, application (App) or teacher (TCH).
#'   \item \code{Observation}. Within-series session-number
#'   \item \code{Pre}. Pre-intervention score on the expressive definition task. 
#'   \item \code{Post}. Post-intervention score on the expressive definition task. 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 90 rows and 5 variables
#' @name Dennis2021
#' @source Dennis, L. R., & Whalon, K. J. (2021). Effects of teacher-versus application-delivered instruction on the expressive vocabulary of at-risk preschool children. Remedial and Special Education, 42(4), 195-206. https://doi.org/10.1177/0741932519900991
NULL

