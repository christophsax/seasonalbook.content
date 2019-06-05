STATISTICS BOOK PROPOSAL, Chapman & Hall/CRC
============================================

## TITLE AND AUTHOR(S)

### 1. Provisional title of your book.

Seasonal adjustment with X-13ARIMA-SEATS in R: A practical guide

### 2. Authors and affiliations.

Christoph Sax (University of Basel, cynkra LLC)
James Livsey (US Census Bureau)


## SUBJECT, AIMS AND FEATURES

### 3. Please describe in detail the subject of your book and indicate its academic level.

This text will focus on seasonal adjustment and its implementation in R. Specifically, the audience will be both R users who want to learn about seasonal adjustment as well as seasonal adjustment practitioners, such as those at governmental agencies, who are starting to become more interested in using R. The book will feature accessible background material and references for those theoretically minded but will be tailored more directly to the practical applications of seasonal adjustment with R. Specifically, we plan to showcase methods through detailed examples with associated code. This presentation will allow the academic level can be quite broad; understood by undergraduates and interesting all the way through final year Ph.D. students.

### 4. Please describe your motivation for writing the book; why it is important.

X-13ARIMA-SEATS is one of, if not the most, widely used seasonal adjustment software within federal and statistical agencies. Moreover, there is a movement in statistical agencies toward the use of R and open-source products. This text will serve the following:

1. Guide to professional seasonal adjustment with R
To make the entry to the world of seasonal adjustment more accessible for those with a beginner understanding of R. We leverage the users R knowledge to more easily understand the input/output of the X-13ARIMA-SEATS program. We also will give an overview of other possibilities of seasonal adjustment in R (e.g. stl, JDemetra).

2. Focus on practitioner’s problem
To bridge an important gap in the training for many seasonal adjustment practitioners. The book addresses practical problems and shows how they can be addressed in X-13ARIMA-SEATS. The use of R allows them to have reproducible examples at hand.

Some Examples: Chinese New Year, Structural Break, Direct or indirect seasonal adjustment, SEATS or X-11



### 5. Please list up to six key features of your proposed book.
• Each chapter start with a concrete practical problem and shows how X-13 can be used to address it
• Teach-by-example format
• Continuous connection of X-13ARIMA-SEATS input with R input and vice-versa
• Fundamental theoretical material when needed
• For each example given the book will give answers, code and provide data.



### 6. Will your book feature any supplementary material, e.g. code and datasets online, or a solutions manual?

All material from the text will be made available to the reader. This includes but is not limited to:

• R Package to accompany the book, containing all data and examples
• Interactive website, on which the examples can be run (similar to seasonal.website)


## AUDIENCE AND RELATED BOOKS

### 7. Please give details of the primary audience for the book. Will it be used for teaching, research or both? Are there any secondary markets?

There are two primary audiences:

A. Current practitioners of seasonal adjustment who are interested in learning how to implement in R. This audience includes researchers from statistical agencies who want to include the scripting language features of R to evaluate properties of their seasonal adjustments. It also serves as a guide to address concrete problems.

B. Current R users who, for one reason or another, want to learn seasonal adjustment. We are able to leverage the readers knowledge of R and make learning seasonal adjustment easier.



### 8. If your book is a textbook, for which courses will it be the primary text? For which will it be supplementary reading?

While the it wouldn’t be written to be a primary textbook for a course, it is highly applicable for a module in a time series or econometrics class.

### 9. What competitive and/or related books are available? (If possible, please indicate author, title, publisher and publication year).


There are no directly relevant competitors to the proposed textbook. There is a book that serves as a primary reference to the X-11 method, a single type of seasonal adjustment:

Seasonal Adjustment with the X-11 Method, Dominique Ladiray and Benoit Quenneville, 2001, Springer-Verlag New York

[Shall we mention the ONS ‘DRAFT’ document? From the style of the book, that’s the closest think around]


### 10. What advantages does your book have over those mentioned above, i.e. identify the niche that your book fills?

Ladiray: Focus on practical problems, rather than theory
ONS: Runable examples in R

Both: Covers latest X13, including SEATS


This textbook will be implemented in R and include all code and data for users to get ‘hands-on’ with. Moreover, the proposed textbook will include running or X-13ARIMA-SEATS not just the method behind the software.


## ADDITIONAL DETAILS

### 11. Approximately how many printed pages will your book contain? Approximately how many figures?

### 12. When would you hope to be able to submit the final draft of the book to us? And in which format, Latex or Word?

We will write the document as an RMarkdown document that includes reproducible examples. This will translate to a Latex document that we want to provide

### 13. Please give the names and e-mail addresses of four people who would be qualified to give an opinion on your proposed book. (We will not necessarily contact these people).


## TABLE OF CONTENTS

### 14. Please include a full table of contents, including chapter sub-headings and/or chapter abstracts.



```
PART I: Basics of Seasonal Adjsustment

Introduction

Seasonal Adjustment with X13

Start with a concrete example, use it to explain theory.


Basic example

seas(AirPassengers)

X = I x S x T



Seasonal Adjustment in R

Other stuff (may be moved to the end)

stl
jDemetra
daily seas adjustment
…


PART II: X-13 ARIMA-SEATS

Overview of the Sofware

regARIMA Model

SEATS vs X-11



PART III: Data Problems

Irregular holidays (Easter CNY Diwali Ramadan)

Trading Days

Outliers

Seasonal Breaks



PART IV: Other Issues

Should we seasonal adjust at all?

Annual Constraining

- Should the annual values be restrained?
- Force spec

Indirect vs direct adjustment

PART V: Quality assessment

Quality measures

- M statistics
- Other stuff

Revisions

- How to measure?
- Should the model be reestimated?
- slidingspan, history

```
