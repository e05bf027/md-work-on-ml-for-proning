![](/images/prone_01.jpg)

# ML on dataset relating to the prone position in UHG

## Folder structure in this repo

This repo documents several areas and avenues in which analysis and machine learning is brought to bear on the data captured from the Intensive Care Unit (ICU) of University Hospital Galway (UHG). The folders, and general contents are as follows:

1. data - an analysis of the data itself, including demographics, and the wrangling process
2. logistic regression - initial attempts to use the patient's response to the initial prone positioning session in order to quantify the patient's subsequent risk of death using a logistic regression approach
3. gaussian naive bayes - initial attempts to use the patient's response to the initial prone positioning session in order to quantify the patient's subsequent risk of death using the Gaussian Naive Bayes algorithm
4. retained changes - this folder documents explorations based on whether changes that happen during the prone positioning session are 'retained' after the patient is returned supine, and whether this can be used to give insigh into mortality

## Dataset used for ML

The data is gathered from Metavision, the Electronic Health Record (EHR) system used in the ICU of UHG using the ‘Metavision Query Wizard’. The query designed for this particular purpose captured all 'verified' data (i.e. confirmed as accurate at least hourly by nursing or laboratory staff) on the system in the areas of:

- patient positioning
- ventilator settings
- cardiovascular measurements
- arterial blood gas values
- haematology blood tests
- biochemical blood tests

Data gathered from the Wizard can be logged as ‘verified’ or ‘not verified’. ‘Verified’ data has been reviewed and approved by nursing or laboratory staff at least once per hour. The Arterial Blood Gas (AB) data is manually entered by the nurse at the patient bedside, and is copied directly from a printout from the point-of-care (POC) analysers. The analysers are not directly connected to Metavision. Other blood tests are integrated directly into Metavision, having been verified in the hospital laboratory.

This data was then wrangled using `tidyverse` tools in R to gather it into a 'tidy' format.

The result of this process produces a time-series dataframe for each patient. A total of 135 patients were processed. From each of these patients available Arterial Blood Gas (ABG) data, basic laboratory blood tests, ventilator settings, and basic cardiovascular observations were isolated prior to when the patient was placed in the prone position. At other pints, all this ata (excepting that of laboratory blood tests) was documented at the following key moments:

- immediately prior to the patient being placed in the prone position
- immediately following being placed in the prone position
- immediately prior to being placed back in to the prone position
- immediately following being placed back to the supine position
