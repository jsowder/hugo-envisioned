# What I want:
#   - read all the Rmd files in content/post
#   - render them with a modified tufte::tufte_html
#   - hugo should take it from there?

library(blogdown)
library(tufte)
library(xfun)

`%n%` <- knitr:::`%n%`
gsub_fixed <- function(...) gsub(..., fixed = TRUE)
pandoc2.0 <- function() rmarkdown::pandoc_available("2.0")

# Get the unexported functions blogdown and tufte
r1 <- unclass(lsf.str(envir = asNamespace("blogdown"), all = T))
r1 <- r1[-grep("%", r1)]
for (name in r1) eval(parse(text = paste0(name, "<-blogdown:::", name)))

r2 <- unclass(lsf.str(envir = asNamespace("tufte"), all = T))
for (name in r2) eval(parse(text = paste0(name, "<-tufte:::", name)))

tufte_hugo_html <- function(..., margin_references = TRUE) {
  format <- rmarkdown::html_document(...)
  pandoc2 <- pandoc2.0()

  # when fig.margin = TRUE, set fig.beforecode = TRUE so plots are moved before
  # code blocks, and they can be top-aligned
  ohooks <- knitr::opts_hooks$set(fig.margin = function(options) {
    if (isTRUE(options$fig.margin)) options$fig.beforecode <- TRUE
    options
  })

  # make sure the existing post processor is called first in our new processor
  post_processor <- format$post_processor
  format$post_processor <- function(metadata, input, output, clean, verbose) {
    if (is.function(post_processor)) {
      output <- post_processor(metadata, input, output, clean, verbose)
    }

    knitr::opts_hooks$restore(ohooks)

    x <- xfun::read_utf8(output)
    fn_label <- paste0(knitr::opts_knit$get("rmarkdown.pandoc.id_prefix"), "fn")
    footnotes <- parse_footnotes(x, fn_label)
    notes <- footnotes$items
    # replace footnotes with sidenotes
    for (i in seq_along(notes)) {
      num <- sprintf(
        '<a href="#%s%d" class="%s" id="%sref%d"><sup>%d</sup></a>',
        fn_label, i, if (pandoc2) "footnote-ref" else "footnoteRef", fn_label, i, i
      )
      con <- sprintf(paste0(
        '<label for="tufte-sn-%d" class="margin-toggle sidenote-number"></label>',
        '<input type="checkbox" id="tufte-sn-%d" class="margin-toggle">',
        '<span class="sidenote"> %s</span>'
      ), i, i, notes[i])
      x <- gsub_fixed(num, con, x)
    }
    # remove footnotes at the bottom
    if (length(footnotes$range)) x <- x[-footnotes$range]

    # replace citations with margin notes
    if (margin_references) x <- margin_references(x)

    # place figure captions in margin notes
    x[x == '<p class="caption">'] <- '<p class="caption marginnote shownote">'

    # move </caption> to the same line as <caption>; the previous line should
    # start with <table
    for (i in intersect(grep("^<caption>", x), grep("^<table", x) + 1)) {
      j <- 0
      while (!grepl("</caption>$", x[i])) {
        j <- j + 1
        x[i] <- paste0(x[i], x[i + j])
        x[i + j] <- ""
      }
    }
    # place table captions in the margin
    r <- "^<caption>(.+)</caption>$"
    for (i in grep(r, x)) {
      # the previous line should be <table> or <table class=...>
      if (!grepl("^<table( class=.+)?>$", x[i - 1])) next
      cap <- gsub(r, "\\1", x[i])
      x[i] <- x[i - 1]
      x[i - 1] <- paste0(
        "<p><!--\n<caption>-->", '<span class="marginnote shownote">',
        cap, "</span><!--</caption>--></p>"
      )
    }

    # add an incremental number to the id of <label> and <input> for margin notes
    r <- '(<label|<input type="checkbox") (id|for)(="tufte-mn)-(" )'
    m <- gregexpr(r, x)
    j <- 1
    regmatches(x, m) <- lapply(regmatches(x, m), function(z) {
      n <- length(z)
      if (n == 0) return(z)
      if (n %% 2 != 0) warning("The number of labels is different with checkboxes")
      for (i in seq(1, n, 2)) {
        if (i + 1 > n) break
        z[i + (0:1)] <- gsub(r, paste0("\\1 \\2\\3-", j, "\\4"), z[i + (0:1)])
        j <<- j + 1
      }
      z
    })

    xfun::write_utf8(x, output)
    output
  }

  if (is.null(format$knitr$knit_hooks)) format$knitr$knit_hooks <- list()
  format$knitr$knit_hooks$plot <- function(x, options) {
    # make sure the plot hook always generates HTML code instead of ![]()
    if (is.null(options$out.extra)) options$out.extra <- ""
    fig_margin <- isTRUE(options$fig.margin)
    fig_fullwd <- isTRUE(options$fig.fullwidth)
    if (fig_margin || fig_fullwd) {
      if (is.null(options$fig.cap)) options$fig.cap <- " " # empty caption
    } else if (is.null(options$fig.topcaption)) {
      # for normal figures, place captions at the top of images
      options$fig.topcaption <- TRUE
    }
    res <- knitr::hook_plot_md(x, options)
    if (fig_margin) {
      res <- gsub_fixed('<p class="caption">', '<!--\n<p class="caption marginnote This is a test ">-->', res)
      res <- gsub_fixed("</p>", "<!--</p>-->", res)
      res <- gsub_fixed("</div>", "<!--</div>--></span></p>", res)
      res <- gsub_fixed(
        '<div class="figure">', paste0(
          "<p>", '<span class="marginnote shownote">', '\n<!--\n<div class="figure">-->'
        ), res
      )
    }
    if (fig_fullwd) {
      res <- gsub_fixed('<div class="figure">', '<div class="figure fullwidth">', res)
      res <- gsub_fixed('<p class="caption">', '<p class="caption marginnote shownote">', res)
    }
    res
  }

  knitr::knit_engines$set(marginfigure = function(options) {
    options$type <- "marginnote"
    if (is.null(options$html.tag)) options$html.tag <- "span"
    options$html.before <- marginnote_html()
    eng_block <- knitr::knit_engines$get("block")
    eng_block(options)
  })

  format$inherits <- "html_document"

  format
}

tufte_html_page <- function(..., number_sections = FALSE, self_contained = FALSE,
                            highlight = NULL, template = NULL, post_processor = NULL) {
  if (identical(template, "default")) {
    stop('blogdown::html_page() does not support template = "default"')
  }
  if (identical(highlight, "textmate")) {
    stop('blogdown::html_page() does not support highlight = "textmate"')
  }
  if (is.character(post_processor)) {
    post_processor <- eval(parse(text = post_processor))
  }

  rmarkdown::output_format(
    knitr = NULL,
    pandoc = NULL,
    clean_supporting = self_contained,
    post_processor = post_processor,
    base_format = tufte_hugo_html(
      ...,
      number_sections = number_sections, theme = NULL,
      self_contained = self_contained, highlight = highlight,
      template = template %n% pkg_file("resources", "template-minimal.html")
    )
  )
}

build_rmds <- function(files) {
  if (length(files) == 0) {
    return()
  }
  lib1 <- by_products(files, c("_files", "_cache"))
  lib2 <- gsub("^content", "blogdown", lib1)
  i <- grep("_files$", lib2)
  lib2[i] <- gsub("^blogdown", "static", lib2[i])
  dirs_rename(lib2, lib1)
  on.exit(dirs_rename(lib1, lib2), add = TRUE)
  root <- getwd()
  base <- site_base_dir()
  shared_yml <- file.path(root, "_output.yml")
  copied_yaml <- character()
  on.exit(unlink(copied_yaml), add = TRUE)
  copy_output_yml <- function(to) {
    if (!file.exists(shared_yml)) {
      return()
    }
    if (file.exists(copy <- file.path(to, "_output.yml"))) {
      return()
    }
    if (file.copy(shared_yml, copy)) {
      copied_yaml <<- c(copied_yaml, copy)
    }
  }
  for (f in files) {
    d <- dirname(f)
    out <- output_file(f, to_md <- is_rmarkdown(f))
    copy_output_yml(d)
    message("Rendering ", f)
    rmarkdown::render(f, tufte_html_page(), envir = globalenv(), quiet = TRUE,
      encoding = "UTF-8", run_pandoc = !to_md, clean = !to_md
    )
    x <- read_utf8(out)
    x <- encode_paths(
      x, by_products(f, "_files"), d, base,
      to_md
    )
    if (to_md) {
      write_utf8(x, out)
    }
    else {
      if (getOption("blogdown.widgetsID", TRUE)) {
        x <- clean_widget_html(x)
      }
      prepend_yaml(f, out, x)
    }
  }
}

rmds <- list_rmds("content")
build_rmds(rmds)
