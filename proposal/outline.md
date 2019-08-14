
PART I: Basics of Seasonal Adjustment
-------------------------------------

This section focuses on explaining the basics of seasonal adjustment and gets the reader involved with a minimal working example.
It keeps the technical jargon to a minimum.
Finally, the layout of the book and future sections is clearly spelled out.

### Introduction

- What is Seasonal Adjustment?

- What is X-13ARIMA-SEATS (X13)?

- Who uses X13?

- Available alternatives in and outside of R (some topics are covered in chapter VI)

- Case Study: A use cases outside of official statistics: seasonal adjustment of business data

Start with a concrete example as quick as possible and use it to explain the basic idea of seasonal adjustment:

    library(seasonal)
    m <- seas(AirPassengers)
    plot(m)

### How to use the book

- Overview of the book

- The Book wants to give concrete advise in case of a problem.

- Ideally we want to have a quick check list that gives readers a starting point where to look for further advice. That could be something like a Cheat Sheet (https://www.rstudio.com/resources/cheatsheets/) with quick advise and chapter references.

- For each section, we want to provide a concrete and informative case study. Some examples are provided in the outline.


PART II: X-13ARIMA-SEATS
------------------------

This section gets readers familiar with X-13ARIMA-SEATS.
It begins by explaining the history and pedagogy of the software.
This leads directly into discussing the principal elements of X-13ARIMA-SEATS.

### Overview of the software

- History of the software

- Elements of the software

- Overview of main choices a user needs to make


### Transform

- Discuss multiplicative vs additive adjustment

- How to use the transform spec

- Case Study idea: Decide between log vs non-log transformation


### SEATS

- How to use the SEATS spec

- SEATS vs X11

- Case Study:

For SEATS, can be quite challenging since it relies heavily on seasonal ARIMA modeling.


### X11

- How to use the X11 spec

- Case Study: Changing the length of trend and/or seasonal filter


### regARIMA Model

- Idea of regARIMA

- How to use the regression spec

- Case studies: Decide if you should include AO in May 2014.
Construct a simple user defined regressor to handle specific issue.


PART III: Data Problems
-----------------------

In part III we look at more in-depth at practical issues with seasonal adjustment.
The focus is on concrete solutions to each situation presented.
Each subsection will prominently feature a case study dedicated to each problem.

### Irregular holidays

- Why should we adjust for holiday effects

- Easter adjustment

- User defined adjustments (Chinese New Year, Diwali)

- Case Study: How to adjust for Ramadan (which is connected with some additional challenges)


### Trading days

- Why should we adjust for trading day effects

- Seven or two coefficient trading day

- Using country specific calendars

- Case Study: Movie tickets (or another series with very clear trading day effects)


### Outliers

- Why care about outliers?

- Additive outliers, level shifts, temporary changes


### Seasonal breaks

- Why to care about seasonal breaks?

- Detection of seasonal breaks

- Correction for seasonal breaks


PART IV: Other Issues
---------------------

Part IV investigates more holistic issues that practitioners face.
The main focus is to give classical methodology to answer their problems.
Since these types of issues can be highly specialized, we concentrate on known solutions to the topics.

### Should a series be seasonally adjusted at all?

- How to test for the presence of seasonality

### Annual constraining

- Should the annual values be restrained?

- How to use the force spec

### Indirect vs direct adjustment

- Should the subcomponents of a series be adjusted separately?


PART V: Quality assessment
--------------------------

This section focuses on diagnostic tools for seasonal adjustment.
This will be written as a stand-alone section as well as a continuance of prior sections.
The idea here is that many readers may be interested in checking the quality of their adjustments but not need help performing it.

### Quality measures

- What is a good seasonal adjustment?

- M statistics

- Other statistics available in X13

### Revisions

- How to measure revisions?

- Should a model be re-estimated each period?

- How to use the slidingspan and history spec


PART VI: The future of seasonal adjustment
------------------------------------------

This short section outlines the future projects in the seasonal adjustment field.
Daily or multiple seasonal adjustment plays a major role here.
Ideally, examples of how to solve these problems are given.

- Daily adjustment

- Multivariate seasonal adjustment

- Other methods
