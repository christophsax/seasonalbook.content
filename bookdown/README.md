Content is drated in R Markdown and **bookdown** (https://github.com/rstudio/bookdown). Please see the page "[Get Started](https://bookdown.org/yihui/bookdown/get-started.html)" at https://bookdown.org/yihui/bookdown/ for how to compile this example into HTML.

The book can be compiled into HTML, using:

```r
bookdown::render_book('index.Rmd', 'bookdown::gitbook')
```

Or into PDF

```r
bookdown::render_book('index.Rmd', 'bookdown::gitbook')
```

The result of both commands can be found in the `_book` folder.
