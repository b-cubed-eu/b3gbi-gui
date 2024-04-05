# Using this theme for any B-cubed shiny app
This `www` directory can be copied and used for any B-cubed shiny app using the `fluidPage` function. Some parts need to be integrated into that function, however.

1. The head section at the beginning of the `fluidPage` function:

```
  tags$head(
    tags$title("B³ Indicators"),
    tags$link(rel="icon", type="image/png", size="32x32", href="B3_logomark.png"),
    tags$meta(name="viewport", content="width=device-width"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
```

2. The title and introductory section just after that:

```
  titlePanel(title = span(img(src = "B3_logomark.png", height = 50),
                          "Your title here",
                          style="color:#000")),
  (
    div(
      HTML("<h3>Your subtitle here</h3>"),
      HTML("<p>Your introductory text here</p>"),
      HTML("<p>More introductory text</p>")
    )
  ),
```

3. The sidebar must be divided into two tabs:

```
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Name of tab 1",
          ...
        ),
        tabPanel(
	      "Name of tab 2",
	      ...
        ),
      ),
    ),
```