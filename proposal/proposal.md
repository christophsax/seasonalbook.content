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

This text will focus on seasonal adjustment and its implementation in R.

Specifically, the audience will be both R users who want to learn about seasonal adjustment as well as seasonal adjustment practitioners, who are interested in using R.
The book will be tailored to the practical applications of seasonal adjustment within R. It presents background material and references for the theoretically minded reader.
The main focus, however, is on concrete problems and examples.

We will showcase methods through detailed examples with associated code.
This presentation allows the academic level to be quite broad; understood by undergraduates all the way through advanced Ph.D. students.

### 4. Please describe your motivation for writing the book; why it is important.

X-13ARIMA-SEATS is one of, if not the most, widely used seasonal adjustment software within federal and statistical agencies.
Moreover, there is a movement in statistical agencies toward the use of R and open-source products.
This text is motivated to unify these two positions. 
Additionally, it and also serve the following:

1. Guide to professional seasonal adjustment with R
To make the entry to the world of seasonal adjustment more accessible for those with an understanding of R.
We leverage the users R knowledge to more easily understand the input/output of the X-13ARIMA-SEATS program.
We also will give an overview of other possibilities of seasonal adjustment in R (e.g.
stl, JDemetra).

2. Focus on practitioner’s problem
To bridge an important gap in the training for many seasonal adjustment practitioners.
The book addresses practical problems and shows how they can be addressed in X-13ARIMA-SEATS.
The use of R allows them to have reproducible examples at hand.

Some Examples: Chinese New Year, structural breaks, direct or indirect seasonal adjustment, SEATS or X-11.


### 5. Please list up to six key features of your proposed book.
- Each chapter include a concrete practical problem and shows how X-13 can be used to address it
- Teach-by-example format
- Continuous connection of X-13ARIMA-SEATS input with R input and vice-versa
- Fundamental theoretical material is referenced throughout (mainly as an option)
- For each example given the book will give answers, code and provide data


### 6. Will your book feature any supplementary material, e.g. code and datasets online, or a solutions manual?

All material from the text will be made available to the reader.
This includes but is not limited to:

- R Package to accompany the book, containing all data and examples
- Interactive website, on which the examples can be run (similar to www.seasonal.website)


## AUDIENCE AND RELATED BOOKS

### 7. Please give details of the primary audience for the book. Will it be used for teaching, research or both? Are there any secondary markets?

There are two primary audiences:

1. Current practitioners of seasonal adjustment who are interested in learning how to implement in R.
This audience includes researchers from statistical agencies who want to include the scripting language features of R to evaluate properties of their seasonal adjustments.

2. Current R users who want to learn seasonal adjustment.
We are able to leverage the readers knowledge of R to make learning seasonal adjustment easier. 
We will feature interesting applications outside of official statistics, such as the seasonal adjustment of business data.


### 8. If your book is a textbook, for which courses will it be the primary text? For which will it be supplementary reading?

While the book is not intended as a primary textbook for a course, it is highly applicable for a module in a time series or econometrics class.

### 9. What competitive and/or related books are available? (If possible, please indicate author, title, publisher and publication year).

There are no directly relevant competitors to the proposed textbook.

There is a book that serves as a primary reference to the X-11 method, a single type of seasonal adjustment:

Ladiray D, Quenneville B (2012). Seasonal Adjustment with the X-11 Method, volume 158. Springer-Verlag.

The following book covers the SEATS method:

Dagum EB, Bianconcini S (2016). “Seasonal Adjustment Based on ARIMA Model Decom- position: TRAMO-SEATS.” In Seasonal Adjustment Methods and Real Time Trend-Cycle Estimation, pp. 115–145. Springer-Verlag.


There are various documents by statistical agencies on topics of the book:

Monsell B (2007). “The X-13A-S Seasonal Adjustment Program.” In Proceedings of the 2007 Federal Committee on Statistical Methodology Research Conference. URL http://www. fcsm.gov/07papers/Monsell.II-B.pdf.

Caporello G, Maravall A, Sánchez FJ (2001). “Program TSW Reference Manual.” Technical Report 0112, Banco de España Madrid. URL https://ideas.repec.org/p/bde/wpaper/ 0112.html.

National Bank of Belgium, Deutsche Bundesbank, Eurostat (2017). JDemetra+: Econometric Software for Seasonal Adjustment and Other Time Series Methods. Eurostat. URL https: //ec.europa.eu/eurostat/cros/content/download.

UK Office for National Statistics (2007). Guide to Seasonal Adjustment with X-12-ARIMA. URL http://www.ons.gov.uk/ons/guide-method/method-quality/ general-methodology/time-series-analysis/guide-to-seasonal-adjustment.pdf.


This book relies on the 'seasonal' package to access X13, which is described in:

Sax C, Eddelbuettel D (2018). “Seasonal Adjustment by X-13ARIMA-SEATS
in R.” _Journal of Statistical Software_, *87*(11), 1-17. doi:
10.18637/jss.v087.i11 (URL: https://doi.org/10.18637/jss.v087.i11).



### 10. What advantages does your book have over those mentioned above, i.e. identify the niche that your book fills?

This is the first book that focuses on practical problems, rather than theory. 
It also the only book that covers all aspects of X13, i.e. both X-11 and SEATS. 
This textbook will focus on R and include all code and data for users to get ‘hands-on’ with.


## ADDITIONAL DETAILS

### 11. Approximately how many printed pages will your book contain? Approximately how many figures?

Judged from our proposed outline, and the heavy use of practical examples in R, we estimate between 40 and 80 figures. 
Including these figures, the content may result in 120 to 200 pages.


### 12. When would you hope to be able to submit the final draft of the book to us? And in which format, Latex or Word?

End of summer 2020.

We will write the document as an RMarkdown document that includes reproducible examples.
This will translate to a LaTeX document that we can provide.

### 13. Please give the names and e-mail addresses of four people who would be qualified to give an opinion on your proposed book.
(We will not necessarily contact these people).

- Brian Monsell
- Dirk Eddelbuettel, dirk@eddelbuettel.com

Dirk Eddelbuettel has also informally expressed his interest and would be available for collaboration.

## TABLE OF CONTENTS

### 14. Please include a full table of contents, including chapter sub-headings and/or chapter abstracts.


see `outline.md`
