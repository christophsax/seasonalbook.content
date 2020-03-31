### tillermanns-2018

- Comparison of different adjustment models, by OOS forecast accuracy, 5 and 20 periods.

- Dummy Models (ARIMA + Week + Month dummies) perform much better than TBATS

- It seems useful to have a method to evaluate different models. Are there alternatives to OOS forecast accuracy?


### berndsen-2020

- describes a 'traffic light approach', a two-valued benchmark to discriminate between the three possible colors. This is what the client ultimately wants.

- We are not interested in most measures, but they the tillermanns-2018 model to check if a value is outside of the forecasted interval.



### ladiray-2018

- closest to a general overview of methods

- chapter 4 describes non-parametric methods (X-11, stl), the closest to our loess workshop attempts ant to what would have been my natural approach.
Also has tips on a few details (such as Feb 29)

    > The tuning of X11 and STL parameters, namely the length of the filters used in the de-composition, needs to be improved. In the genuine X11 algorithm, and for monthly and quarterly series, the order of the moving averages is selected according to a “signal to noise” ratio (the I/C ratio for the order of the trend cycle moving averages and the I/S ratio for the seasonal moving averages). Thresholds have been defined by simulations and practice. Large scale simulations have still to be done to understand the behavior of these ratios and to elaborate a decision rule for high frequency data.

- chapter 5 describes parametric methods.
This is basically SEATS, applied to daily data. Any software that makes this available? Google tells me not in JDemetra.

- chapter 6 application with some ideas.
Validation of the results: check for residual seasonality

    > It turns out that the adjustments are not really satisfactory."


### mcelroy-2018

- Parametrized decomposition (as in chapter 5 in ladiray-2018?)

- Is there any software for that?



### taylor-2017

- prophet package

- intro compares some common R packages, esp. forecast

- comparison w auto.arima, tbats

- GAM Model

- my impression from using it was that the yearly effect is estimated too smoothly.
E.g. Christmas effects would start mid of December and build up until Christmas day and the smoothly decrease.


### ollech-2018

- a version of stl decomposition, probably similar to our loess

- it is quite slow, not sure why our loess thing is so much faster.




### Research Plan

Repeat tillermanns-2018

Also include:

- dsa (Bundesbank)
- loess procedure
- facebook prophet
- ???

Expectation: Beating the dummy models should not be too hard. Is TBATS really that bad?
Perhaps they just used it the wrong way?


Concretely:

- [ ] Adjust: TBATS
- [ ] Adjust: Prophet
- [ ] Adjust: Arima Dummy
- [ ] Adjust: dsa
- [ ] Compare results a la timmermans 2018



### Chapter Plan

- Division in non-parametrized (as in X-11) and parametrized (as in SEATS) decomposition methods?

- Strong focus on acutal application in R, not on theory

- But we still need to no what we are 'recommending'





