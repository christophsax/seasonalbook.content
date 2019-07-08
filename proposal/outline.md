PART I: Basics of Seasonal Adjustment
======================================

This section focuses on explaining the very basics of seasonal adjustment and gets the reader involved with a minimal working example. It keeps the technical jargon to a minimum. Finally, the layout of the textbook and future sections is clearly spelled out.

## Introduction

- What is Seasonal Adjustment?

- What is X13?

- Who uses X13?

- Alternatives: Other things available in R (perhaps move to Chap VI)
  - stl
  - jDemetra
  - daily seas adjustment
  - https://otexts.com/fpp2/complexseasonality.html

- Discuss Alternative Use Cases: Seasonal Adjustment of Business Data


Start with a concrete example as quick as possible:

    library(seasonal)
    m <- seas(AirPassengers)
    plot(m)

Use it to explain the idea of seasonal adjustment.

    X = I x S x T


## How to use the Book

- Overview of the Book

- The Book wants to give concrete advise in case of a problem.
Ideally we want to have a quick check list that gives readers a starting point where to look for further advice. That could be something like a Cheat Sheet (https://www.rstudio.com/resources/cheatsheets/) with quick advise and chapter references.


PART II: X-13 ARIMA-SEATS
=========================

This section gets readers familiar with X-13ARIMA-SEATS. The section begins by explaining the history and pedagogy of the software. This leads directly into discussing the most crucial aspects of X-13ARIMA-SEATS.

## Overview of the Software

- History of the Software

- Elements of the Software

- Overview of main choices user needs to make


## Transform Spec

- short section about transform

- Could use to discuss multiplicative vs additive adjustment here.

- Case Study idea: Decide between log vs no-log transformation 


## SEATS

- Using the `seats` spec

- Case Study: How about having a box with a slightly challenging example for each spec? 
At least an informative example. For SEATS, challenging can be quite challenging since it relies heavily on seasonal ARIMA modeling. 


## X11

- Using the `x11` spec

- Case Study Ideas: length of trend or seasonal filter


## regARIMA Model

- Idea of regARIMA

- Using the `regression` spec (with examples)

- Case study ideas: Decide if you should include AO in May 2014 OR have reader construct very simple user defined regressor to handle specific issue.


PART III: Data Problems
=======================

In part III we look at more in depth issues seasonal adjustment practitioners face. Here, the focus is on practical solutions to each situation presented. Each subsection will prominently feature a case study dedicated to each problem.

## Irregular holidays

- Easter etc adjusment

- User defined adjustments (Chinese New Year, Diwali)

- Case Study: Ramadan (no idea how to do that, but would be nice to give some good answers on difficult questions)


## Trading Days

## Outliers

## Seasonal Breaks



PART IV: Other Issues
=====================

Part IV investigates more holistic issues that practitioners face. The main focus is to give classical methodology to answer their problems. Since these types of issues can be highly specialized, we concentrate on known solutions to the topics. 

## Should we seasonal adjust at all?

- How to test

## Annual Constraining

- Should the annual values be restrained?
- Force spec

## Indirect vs direct adjustment


PART V: Quality assessment
==========================

This section focuses on diagnostic tools for seasonal adjustment. This will be written as a stand alone section as well as a continuance of prior sections. The idea here is that many readers may be interested in checking the quality of their adjustments but not need help performing it.  

## Quality measures

- M statistics
- Other stuff

## Revisions

- How to measure?
- Should the model be reestimated?
- slidingspan, history


PART VI: The future of seasonal adjustment
=========================================

This short section outlines the future projects in the seasonal adjustment field. 

- daily adjustment
- multivariate seasonal adjustment
- other methods


