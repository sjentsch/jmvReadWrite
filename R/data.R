#' Imagine that you worked for a record company and that your boss was interested in predicting album sales from
#' advertising.
#'
#' The data set is fictional and was constructed by Andy Field who therefore owns the copyright. The data set is also publicly
#' available on the website that accompanies Andy Field's book, https://edge.sagepub.com/field5e.
#' Without Andy Field's explicit consent, this data set may not be distributed for commercial purposes, this data set may not be
#' edited, and this data set may not be presented without acknowledging its source (i.e., the terms of a CC BY-NC-ND license).
#'
#' Reference: Field, A. P. (2017). Discovering Statistics Using IBM SPSS Statistics (5th ed.). Sage.
#'
#'@format A data frame with 60 rows, each one representing a different album, and 5 variables:
#'\describe{
#'   \item{\code{selSbj}}{Select the data in this row (1) or not (0)}
#'   \item{\code{Adverts}}{Amount (in thousands of pounds) spent promoting the album before release}
#'   \item{\code{Airplay}}{How many times songs from the album were played on a prominent national radio station in the week before release}
#'   \item{\code{Image}}{How attractive people found the band's image (out of 10)}
#'   \item{\code{Sales}}{Sales (in thousands) of each album in the week after release}
#'}
"AlbumSales"

#' The Effect of Vitamin C on Tooth Growth in Guinea Pigs
#'
#'@format A data frame with 60 rows and 6 variables:
#'\describe{
#'   \item{\code{ID}}{ID of the guinea pig}
#'   \item{\code{supp}}{Supplement type (VC: Vitamin C or OJ: Orange juice)}
#'   \item{\code{supp2}}{Transformation of the supplement type (factor to numerical: VC = 1; OJ = 2)}
#'   \item{\code{dose}}{Dose in grams / day}
#'   \item{\code{dose2}}{Dose in grams / day}
#'   \item{\code{len}}{Tooth length}
#'   \item{\code{logLen}}{Natural logarithm of the tooth length (len)}
#'}
"ToothGrowth"

#' Twenty-five personality self-report items taken from the International Personality Item Pool
#'
#' The data set contains responses from 250 participants filling in twenty-five personality self-report items
#' taken from the International Personality Item Pool (https://ipip.ori.org) as part of the Synthetic Aperture
#' Personality Assessment (SAPA) web-based personality assessment (https://sapa-project.org) project.
#' The 25 items are organized by five putative factors: Agreeableness (A1 to A5), Conscientiousness (C1 to C5),
#' Extraversion (E1 to E5), Neuroticism (N1 to N5), and Openness (N1 to N5). The items were short phrases that
#' the respondent should answer by indicating how accurately the statement describes their typical behaviour or
#' attitude. Responses were collected using a 6-point scale: 1 - Very inaccurate, 2 - Moderately inaccurate,
#' 3 - Slightly inaccurate, 4 - Slightly accurate, 5 - Moderately accurate, 6 - Very accurate.
#'
#'@format A data frame with 254 rows (250 original respondents, 4 generated for testing) and 33 variables:
#'\describe{
#'   \item{\code{ID}}{Respondent ID}
#'   \item{\code{A1}}{Am indifferent to the feelings of others. (reversed)}
#'   \item{\code{A2}}{Inquire about others' well-being.}
#'   \item{\code{A3}}{Know how to comfort others.}
#'   \item{\code{A4}}{Love children.}
#'   \item{\code{A5}}{Make people feel at ease.}
#'   \item{\code{C1}}{Am exacting in my work.}
#'   \item{\code{C2}}{Continue until everything is perfect.}
#'   \item{\code{C3}}{Do things according to a plan.}
#'   \item{\code{C4}}{Do things in a half-way manner. (reversed)}
#'   \item{\code{C5}}{Waste my time. (reversed)}
#'   \item{\code{E1}}{Don't talk a lot. (reversed)}
#'   \item{\code{E2}}{Find it difficult to approach others. (reversed)}
#'   \item{\code{E3}}{Know how to captivate people.}
#'   \item{\code{E4}}{Make friends easily.}
#'   \item{\code{E5}}{Take charge.}
#'   \item{\code{N1}}{Get angry easily.}
#'   \item{\code{N2}}{Get irritated easily.}
#'   \item{\code{N3}}{Have frequent mood swings.}
#'   \item{\code{N4}}{Often feel blue.}
#'   \item{\code{N5}}{Panic easily.}
#'   \item{\code{O1}}{Am full of ideas.}
#'   \item{\code{O2}}{Avoid difficult reading material. (reversed)}
#'   \item{\code{O3}}{Carry the conversation to a higher level.}
#'   \item{\code{O4}}{Spend time reflecting on things.}
#'   \item{\code{O5}}{Will not probe deeply into a subject. (reversed)}
#'   \item{\code{gender}}{Gender of the respondent (female, male)}
#'   \item{\code{age}}{Age of the respondent (years)}
#'   \item{\code{AD}}{Exponent of age (computed: EXP(age))}
#'   \item{\code{AF}}{Random data (for testing)}
#'   \item{\code{AG}}{Random data (for testing)}
#'   \item{\code{age_tr}}{Age of the respondent (transformed, as decades: 1 - 10-19, 2 - 20-29, 3 - 30-39, 4 - 40-49, 5 - 50-59, 6 - 60 and over)}
#'   \item{\code{ID2}}{Respondent ID (for testing; "A" + random-generated 5-digit-code)}
#'}
"bfi_sample"

#' Twenty-five personality self-report items taken from the International Personality Item Pool
#' (includes jamovi-attributes; should result in a file identical to bfi_sample2.omv under "extdata" when
#' written with write_omv)
#'
#' The data set contains responses from 250 participants filling in twenty-five personality self-report items
#' taken from the International Personality Item Pool (https://ipip.ori.org) as part of the Synthetic Aperture
#' Personality Assessment (SAPA) web-based personality assessment (https://sapa-project.org) project.
#' The 25 items are organized by five putative factors: Agreeableness (A1 to A5), Conscientiousness (C1 to C5),
#' Extraversion (E1 to E5), Neuroticism (N1 to N5), and Openness (N1 to N5). The items were short phrases that
#' the respondent should answer by indicating how accurately the statement describes their typical behaviour or
#' attitude. Responses were collected using a 6-point scale: 1 - Very inaccurate, 2 - Moderately inaccurate,
#' 3 - Slightly inaccurate, 4 - Slightly accurate, 5 - Moderately accurate, 6 - Very accurate.
#' The data set includes the jamovi-attributes. It is supposed to result in an identical file compared to the
#' bfi_sample2.omv-file contained in the extdata-directory of the package when written with write_omv.
#'
#'@format A data.frame with 250 rows and 29 variables
#'\describe{
#'   \item{\code{ID}}{Respondent ID}
#'   \item{\code{A1}}{Am indifferent to the feelings of others. (reversed)}
#'   \item{\code{A2}}{Inquire about others' well-being.}
#'   \item{\code{A3}}{Know how to comfort others.}
#'   \item{\code{A4}}{Love children.}
#'   \item{\code{A5}}{Make people feel at ease.}
#'   \item{\code{C1}}{Am exacting in my work.}
#'   \item{\code{C2}}{Continue until everything is perfect.}
#'   \item{\code{C3}}{Do things according to a plan.}
#'   \item{\code{C4}}{Do things in a half-way manner. (reversed)}
#'   \item{\code{C5}}{Waste my time. (reversed)}
#'   \item{\code{E1}}{Don't talk a lot. (reversed)}
#'   \item{\code{E2}}{Find it difficult to approach others. (reversed)}
#'   \item{\code{E3}}{Know how to captivate people.}
#'   \item{\code{E4}}{Make friends easily.}
#'   \item{\code{E5}}{Take charge.}
#'   \item{\code{N1}}{Get angry easily.}
#'   \item{\code{N2}}{Get irritated easily.}
#'   \item{\code{N3}}{Have frequent mood swings.}
#'   \item{\code{N4}}{Often feel blue.}
#'   \item{\code{N5}}{Panic easily.}
#'   \item{\code{O1}}{Am full of ideas.}
#'   \item{\code{O2}}{Avoid difficult reading material. (reversed)}
#'   \item{\code{O3}}{Carry the conversation to a higher level.}
#'   \item{\code{O4}}{Spend time reflecting on things.}
#'   \item{\code{O5}}{Will not probe deeply into a subject. (reversed)}
#'   \item{\code{gender}}{Gender of the respondent (female, male)}
#'   \item{\code{age}}{Age of the respondent (years)}
#'   \item{\code{ID2}}{Respondent ID (for testing; "A" + random-generated 4-digit-code)}
#'}
"bfi_sample2"

#' Twenty-five personality self-report items taken from the International Personality Item Pool
#' (testing file for ordered factors / "Ordinal"-variables in jamovi)
#'
#' The data set contains responses from 250 participants filling in twenty-five personality self-report items
#' taken from the International Personality Item Pool (https://ipip.ori.org) as part of the Synthetic Aperture
#' Personality Assessment (SAPA) web-based personality assessment (https://sapa-project.org) project.
#' The 25 items are organized by five putative factors: Agreeableness (A1 to A5), Conscientiousness (C1 to C5),
#' Extraversion (E1 to E5), Neuroticism (N1 to N5), and Openness (N1 to N5). The items were short phrases that
#' the respondent should answer by indicating how accurately the statement describes their typical behaviour or
#' attitude. Responses were collected using a 6-point scale: 1 - Very inaccurate, 2 - Moderately inaccurate,
#' 3 - Slightly inaccurate, 4 - Slightly accurate, 5 - Moderately accurate, 6 - Very accurate.
#' The data set includes the jamovi-attributes. It is supposed to result in an identical file compared to the
#' bfi_sample2.omv-file contained in the extdata-directory of the package when written with write_omv.
#'
#'@format A data.frame with 250 rows and 28 variables
#'\describe{
#'   \item{\code{ID}}{Respondent ID}
#'   \item{\code{A1}}{Am indifferent to the feelings of others. (reversed)}
#'   \item{\code{A2}}{Inquire about others' well-being.}
#'   \item{\code{A3}}{Know how to comfort others.}
#'   \item{\code{A4}}{Love children.}
#'   \item{\code{A5}}{Make people feel at ease.}
#'   \item{\code{C1}}{Am exacting in my work.}
#'   \item{\code{C2}}{Continue until everything is perfect.}
#'   \item{\code{C3}}{Do things according to a plan.}
#'   \item{\code{C4}}{Do things in a half-way manner. (reversed)}
#'   \item{\code{C5}}{Waste my time. (reversed)}
#'   \item{\code{E1}}{Don't talk a lot. (reversed)}
#'   \item{\code{E2}}{Find it difficult to approach others. (reversed)}
#'   \item{\code{E3}}{Know how to captivate people.}
#'   \item{\code{E4}}{Make friends easily.}
#'   \item{\code{E5}}{Take charge.}
#'   \item{\code{N1}}{Get angry easily.}
#'   \item{\code{N2}}{Get irritated easily.}
#'   \item{\code{N3}}{Have frequent mood swings.}
#'   \item{\code{N4}}{Often feel blue.}
#'   \item{\code{N5}}{Panic easily.}
#'   \item{\code{O1}}{Am full of ideas.}
#'   \item{\code{O2}}{Avoid difficult reading material. (reversed)}
#'   \item{\code{O3}}{Carry the conversation to a higher level.}
#'   \item{\code{O4}}{Spend time reflecting on things.}
#'   \item{\code{O5}}{Will not probe deeply into a subject. (reversed)}
#'   \item{\code{gender}}{Gender of the respondent (Females, Males)}
#'   \item{\code{age}}{Age of the respondent (years)}
#'}
"bfi_sample3"
