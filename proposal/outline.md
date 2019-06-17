PART I: Basics of Seasonal Adjsustment
======================================

## Introduction

- What is Seasonal Adjustment?

- What is X13?

- Who uses X13?

- Alternatives: Other things available in R (perhaps move to Chap VI)
  - stl
  - jDemetra
  - daily seas adjustment
  - https://otexts.com/fpp2/complexseasonality.html

- Discuss Alternative Use Cases: Seasonal Adjusmtent of Business Data


Start with a concrete example as quick as possible:

    library(seasonal)
    m <- seas(AirPassengers)
    plot(m)

Use it to explain the idea of seasonal adjustment.

    X = I x S x T


## How to use the Book

- Overview of the Book

- The Book wants to give concrete advise in case of a problem.
Idealy we want to have a quick check list that gives readers a starting point where to look for further advice. That could be something like a Cheat Sheet (https://www.rstudio.com/resources/cheatsheets/) with quick advise and chapter references.


PART II: X-13 ARIMA-SEATS
=========================

## Overview of the Sofware

- Elements of the Software

	* Input and Output structure
	* Specs

- History of the Software


PART III: Main Specs
====================


## SEATS

- Using the `seats` spec

- Case Study: How about having a box with a slightly challenging example for each spec?


## X11

- Using the `x11` spec


PART IV: Data Problems (secondary specs)
========================================

## regARIMA Model

- Idea of regARIMA

- Using the `regression` spec (with examples)

### Irregular holidays

- Easter etc adjusment

- User defined adjustments (Chinese New Year, Diwali)

- Case Study: Ramadan (no idea how to do that, but would be nice to give some good answers on difficult questions)

### Trading Days (regression spec)

## Outliers (outlier spec)

## Seasonal Breaks



PART V: Other Issues
=====================

## Should we seasonal adjust at all?

- How to test

## Annual Constraining

- Should the annual values be restrained?
- Force spec

## Indirect vs direct adjustment


PART VI: Quality assessment
==========================

## Quality measures

- M statistics
- Other stuff

## Revisions

- How to measure?
- Should the model be reestimated?
- slidingspan, history


PART VII: The future of seaonal adjustment
=========================================

- daily adjusmtent
- other methods


