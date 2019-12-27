Content is drafted in R Markdown and **bookdown** (https://github.com/rstudio/bookdown).

The book can be compiled into HTML:

```r
bookdown::render_book('index.Rmd', 'bookdown::gitbook')
```

or into PDF:

```r
bookdown::render_book('index.Rmd', 'bookdown::pdf_book')
```

The result of both commands is written to the `_book` folder.
