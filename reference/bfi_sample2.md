# Twenty-five personality self-report items taken from the International Personality Item Pool (includes jamovi-attributes; should result in a file identical to bfi_sample2.omv under "extdata" when written with write_omv)

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
4 - Slightly accurate, 5 - Moderately accurate, 6 - Very accurate. The
data set includes the jamovi-attributes. It is supposed to result in an
identical file compared to the bfi_sample2.omv-file contained in the
extdata-directory of the package when written with write_omv.

## Usage

``` r
bfi_sample2
```

## Format

A data.frame with 250 rows and 29 variables

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

- `ID2`:

  Respondent ID (for testing; "A" + random-generated 4-digit-code)
