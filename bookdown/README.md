Content is drafted in R Markdown and [bookdown](https://github.com/rstudio/bookdown).

The book can be compiled into HTML:

```r
bookdown::render_book('index.Rmd', 'bookdown::gitbook')
```

or into PDF:

```r
bookdown::render_book('index.Rmd', 'bookdown::pdf_book')
```

The result of both commands are available as `_book/index.html` or `_book/_main.pdf`.

**PDF is working badly, so we should focus on the HTML version.**
