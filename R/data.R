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
#'@format A data.frame with 60 rows, each one representing a different album, and 5 variables
#'\describe{
#'   \item{Adverts}{numeric}{Amount (in thousands of pounds) spent promoting the album before release}
#'   \item{Airplay}{integer}{How many times songs from the album were played on a prominent national radio station in the week before release}
#'   \item{Image}{integer}{How attractive people found the band's image (out of 10)}
#'   \item{Sales}{integer}{Sales (in thousands) of each album in the week after release}
#'}
"AlbumSales"

#' The Effect of Vitamin C on Tooth Growth in Guinea Pigs
#'
#'@format A data.frame with 60 rows and 6 variables
#'\describe{
#'   \item{ID}{character}{ID of the guinea pig}
#'   \item{supp}{factor}{Supplement type (VC: Vitamin C or OJ: Orange juice)}
#'   \item{supp2}{factor}{Transformation of the supplement type (factor to numerical: VC = 1; OJ = 2)}
#'   \item{dose}{numeric}{Dose in grams / day}
#'   \item{dose2}{ordered factor}{Dose in grams / day}
#'   \item{len}{numeric}{Tooth length}
#'   \item{logLen}{numeric}{Natural logarithm of the tooth length (len)}
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
#'@format A data.frame with 254 rows (250 original respondents, 4 manually generated for testing) and 33 variables
#'\describe{
#'   \item{ID}{character}{Respondent ID}
#'   \item{A1}{integer}{Am indifferent to the feelings of others. (reversed)}
#'   \item{A2}{integer}{Inquire about others' well-being.}
#'   \item{A3}{integer}{Know how to comfort others.}
#'   \item{A4}{integer}{Love children.}
#'   \item{A5}{integer}{Make people feel at ease.}
#'   \item{C1}{integer}{Am exacting in my work.}
#'   \item{C2}{integer}{Continue until everything is perfect.}
#'   \item{C3}{integer}{Do things according to a plan.}
#'   \item{C4}{integer}{Do things in a half-way manner. (reversed)}
#'   \item{C5}{integer}{Waste my time. (reversed)}
#'   \item{E1}{integer}{Don't talk a lot. (reversed)}
#'   \item{E2}{integer}{Find it difficult to approach others. (reversed)}
#'   \item{E3}{integer}{Know how to captivate people.}
#'   \item{E4}{integer}{Make friends easily.}
#'   \item{E5}{integer}{Take charge.}
#'   \item{N1}{integer}{Get angry easily.}
#'   \item{N2}{integer}{Get irritated easily.}
#'   \item{N3}{integer}{Have frequent mood swings.}
#'   \item{N4}{integer}{Often feel blue.}
#'   \item{N5}{integer}{Panic easily.}
#'   \item{O1}{integer}{Am full of ideas.}
#'   \item{O2}{integer}{Avoid difficult reading material. (reversed)}
#'   \item{O3}{integer}{Carry the conversation to a higher level.}
#'   \item{O4}{integer}{Spend time reflecting on things.}
#'   \item{O5}{integer}{Will not probe deeply into a subject. (reversed)}
#'   \item{gender}{factor}{Gender of the respondent (female, male)}
#'   \item{age}{integer}{Age of the respondent (years)}
#'   \item{AD}{numeric}{Exponent of age (computed: EXP(age))}
#'   \item{AF}{factor}{Random data (for testing)}
#'   \item{AG}{factor}{Random data (for testing)}
#'   \item{age_tr}{factor}{Age of the respondent (transformed, as decades: 1 - 10-19, 2 - 20-29, 3 - 30-39, 4 - 40-49, 5 - 50-59, 6 - 60 and over)}
#'   \item{ID2}{character}{Respondent ID (for testing; "A" + random-generated 5-digit-code)}
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
#'   \item{ID}{character}{Respondent ID}
#'   \item{A1}{integer}{Am indifferent to the feelings of others. (reversed)}
#'   \item{A2}{integer}{Inquire about others' well-being.}
#'   \item{A3}{integer}{Know how to comfort others.}
#'   \item{A4}{integer}{Love children.}
#'   \item{A5}{integer}{Make people feel at ease.}
#'   \item{C1}{integer}{Am exacting in my work.}
#'   \item{C2}{integer}{Continue until everything is perfect.}
#'   \item{C3}{integer}{Do things according to a plan.}
#'   \item{C4}{integer}{Do things in a half-way manner. (reversed)}
#'   \item{C5}{integer}{Waste my time. (reversed)}
#'   \item{E1}{integer}{Don't talk a lot. (reversed)}
#'   \item{E2}{integer}{Find it difficult to approach others. (reversed)}
#'   \item{E3}{integer}{Know how to captivate people.}
#'   \item{E4}{integer}{Make friends easily.}
#'   \item{E5}{integer}{Take charge.}
#'   \item{N1}{integer}{Get angry easily.}
#'   \item{N2}{integer}{Get irritated easily.}
#'   \item{N3}{integer}{Have frequent mood swings.}
#'   \item{N4}{integer}{Often feel blue.}
#'   \item{N5}{integer}{Panic easily.}
#'   \item{O1}{integer}{Am full of ideas.}
#'   \item{O2}{integer}{Avoid difficult reading material. (reversed)}
#'   \item{O3}{integer}{Carry the conversation to a higher level.}
#'   \item{O4}{integer}{Spend time reflecting on things.}
#'   \item{O5}{integer}{Will not probe deeply into a subject. (reversed)}
#'   \item{gender}{factor}{Gender of the respondent (female, male)}
#'   \item{age}{integer}{Age of the respondent (years)}
#'   \item{ID2}{character}{Respondent ID (for testing; "A" + random-generated 4-digit-code)}
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
#'   \item{ID}{character}{Respondent ID}
#'   \item{A1}{ordered factor}{Am indifferent to the feelings of others. (reversed)}
#'   \item{A2}{ordered factor}{Inquire about others' well-being.}
#'   \item{A3}{ordered factor}{Know how to comfort others.}
#'   \item{A4}{ordered factor}{Love children.}
#'   \item{A5}{ordered factor}{Make people feel at ease.}
#'   \item{C1}{ordered factor}{Am exacting in my work.}
#'   \item{C2}{ordered factor}{Continue until everything is perfect.}
#'   \item{C3}{ordered factor}{Do things according to a plan.}
#'   \item{C4}{ordered factor}{Do things in a half-way manner. (reversed)}
#'   \item{C5}{ordered factor}{Waste my time. (reversed)}
#'   \item{E1}{ordered factor}{Don't talk a lot. (reversed)}
#'   \item{E2}{ordered factor}{Find it difficult to approach others. (reversed)}
#'   \item{E3}{ordered factor}{Know how to captivate people.}
#'   \item{E4}{ordered factor}{Make friends easily.}
#'   \item{E5}{ordered factor}{Take charge.}
#'   \item{N1}{ordered factor}{Get angry easily.}
#'   \item{N2}{ordered factor}{Get irritated easily.}
#'   \item{N3}{ordered factor}{Have frequent mood swings.}
#'   \item{N4}{ordered factor}{Often feel blue.}
#'   \item{N5}{ordered factor}{Panic easily.}
#'   \item{O1}{ordered factor}{Am full of ideas.}
#'   \item{O2}{ordered factor}{Avoid difficult reading material. (reversed)}
#'   \item{O3}{ordered factor}{Carry the conversation to a higher level.}
#'   \item{O4}{ordered factor}{Spend time reflecting on things.}
#'   \item{O5}{ordered factor}{Will not probe deeply into a subject. (reversed)}
#'   \item{gender}{factor}{Gender of the respondent (Females, Males)}
#'   \item{age}{integer}{Age of the respondent (years)}
#'}
"bfi_sample3"
