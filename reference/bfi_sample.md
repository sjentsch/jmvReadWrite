# Twenty-five personality self-report items taken from the International Personality Item Pool

The data set contains responses from 250 participants filling in
twenty-five personality self-report items taken from the International
Personality Item Pool (https://ipip.ori.org) as part of the Synthetic
Aperture Personality Assessment (SAPA) web-based personality assessment
(https://sapa-project.org) project. The 25 items are organized by five
putative factors: Agreeableness (A1 to A5), Conscientiousness (C1 to
C5), Extraversion (E1 to E5), Neuroticism (N1 to N5), and Openness (N1
to N5). The items were short phrases that the respondent should answer
by indicating how accurately the statement describes their typical
behaviour or attitude. Responses were collected using a 6-point scale:
1 - Very inaccurate, 2 - Moderately inaccurate, 3 - Slightly inaccurate,
4 - Slightly accurate, 5 - Moderately accurate, 6 - Very accurate.

## Usage

``` r
bfi_sample
```

## Format

A data frame with 254 rows (250 original respondents, 4 generated for
testing) and 33 variables:

- `ID`:

  Respondent ID

- `A1`:

  Am indifferent to the feelings of others. (reversed)

- `A2`:

  Inquire about others' well-being.

- `A3`:

  Know how to comfort others.

- `A4`:

  Love children.

- `A5`:

  Make people feel at ease.

- `C1`:

  Am exacting in my work.

- `C2`:

  Continue until everything is perfect.

- `C3`:

  Do things according to a plan.

- `C4`:

  Do things in a half-way manner. (reversed)

- `C5`:

  Waste my time. (reversed)

- `E1`:

  Don't talk a lot. (reversed)

- `E2`:

  Find it difficult to approach others. (reversed)

- `E3`:

  Know how to captivate people.

- `E4`:

  Make friends easily.

- `E5`:

  Take charge.

- `N1`:

  Get angry easily.

- `N2`:

  Get irritated easily.

- `N3`:

  Have frequent mood swings.

- `N4`:

  Often feel blue.

- `N5`:

  Panic easily.

- `O1`:

  Am full of ideas.

- `O2`:

  Avoid difficult reading material. (reversed)

- `O3`:

  Carry the conversation to a higher level.

- `O4`:

  Spend time reflecting on things.

- `O5`:

  Will not probe deeply into a subject. (reversed)

- `gender`:

  Gender of the respondent (female, male)

- `age`:

  Age of the respondent (years)

- `AD`:

  Exponent of age (computed: EXP(age))

- `AF`:

  Random data (for testing)

- `AG`:

  Random data (for testing)

- `age_tr`:

  Age of the respondent (transformed, as decades: 1 - 10-19, 2 - 20-29,
  3 - 30-39, 4 - 40-49, 5 - 50-59, 6 - 60 and over)

- `ID2`:

  Respondent ID (for testing; "A" + random-generated 5-digit-code)
