# Analysis Plan

All data processing, visualisation and analyses will use R Version 4.2.1 ("Funny Looking Kid"), the tidyverse package collection (v1.3.x), the brms package (v2.17.x) to fit Bayesian generalized models using Stan and associated packages like bridgesampler (1.1x).

## Responses for untrained items in transfer phase

- bayesian logistic regression
- Hx.x ("")


## Analysis of transfer phase

### Hx.x: Accuracy Estimates

- bayesian logistic regression
- dependent var.: number of extrapolated responses
- fixed effects:
    - main effect of experimental condition (int_only < rrl ~= srf < srf&rrl)
- random effects:
    - by-subject intercepts, slopes for experimental condition
- comparison of (log) odds for extrapolation between conditions



# Variables

## Measured

- (both) category decision (0 / 1)  
- (both) response time (continuous; milliseconds)  
- (both) image/stimulus ID  
- (training) decision correctness (0 / 1)  
- (training) training Block ID (integer)  
- (transfer) stimulus membership (trained / untrained / equivocal)  
- demographics:
    - participant ID (character; assigned)
    - age (integer)
    - native language (character)
    - education (character)
    - comments (character)

## Manipulated

- (both) experimental condition (control / rrl / srf / both)