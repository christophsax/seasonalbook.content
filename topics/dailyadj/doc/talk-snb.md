# Seasonal adjustment of daily time series


## Intro

- Decomposition in weekday effects, monthday effects (e.g., salary payments) and yearday effects (e.g. 13th monthly salary), as well as irregular holiday effects (e.g. payments before Easter)

- More complex than standard (monthly or quartely) seasonal adjustment.

- Weakly explored topic: Many attempts but no widely accepted topic


## Our Sub-Project

- Gain overview over the methods

- Evaluate them in forecast competitions (as in Tmmermans 18)

- Implement an acceptable solution in terms of accuracy and speed

- Synergy with other projects: R-Journal Article, Book on Seasonal Adj (both with James Livsey, U.S. Census)


## Basic Mechanism

- Various attempts to seasonlly adjuste data can be broadly distingued into parametric and non-parametric approaches.

- Non-parametric approaches seem to be the more obvious candidates to use with the irregular structure of daily data. Parametric models require the time units to be regularily spaced.

- Non-parametric estimation is also what is used in the X-11 method of X-13, which is the standard method for quarterly or monthly data.

- Explain Basic Idea: Line up Mondays in a row, draw a smoothed line through them. This is your monday effect, which you subtract:

- Sepwise estimation: Trend, Weekdays, Holidays, Monthdays, Yeardays. Because we use LOESS to draw the smoothed line, this can be referred as STL, whcih is an acronym for "Seasonal and Trend decomposition using Loess" (Cleveland et al 1990).

- Same ideas are used in a package 'dsa' (Olech, Bundesbank), but the package performs a very extensive outlier and holiday detection, and is thus very slow. I am in contact with the Author and he promied an Example where he turns off some of it.


## Comparison of methods

- seas_daily() has a similar forecast performance to dsa()

- Both outperform simple parametric models (e.g. ARIMAX with montly and holiday dummies)

- also outperform the popular prophet() package by Facebook, which is also relatively slow.

- Speedcomarison: seas_daily() vs dsa() FIMXE


## Callenges

- Changing seasonal pattern: E.g., change in weekday effects in some series. Can be mitigated by lowering the smoothing parameter in LOESS

- Interactions between effects: Payments scheduled for the 25th may be executed earlier, if the 25th of March is an Easter Holiday.
Very difficult to catch these things; currently ignored.


## Live Demo

- Show how to adjust retail counts

- Show effects graphically






