# Session 9: R Shiny malaria dashboard

These are the essential files for the AMMnet workshop on building an
interactive malaria dashboard with R Shiny.

## Files

- `app.R`: starter application for participants
- `app_teacher.R`: completed application for reference
- `data/malaria_data.rds`: simulated monthly district indicators
- `data/annual_summary.rds`: simulated annual summary indicators
- `data/bfa_districts.rds`: Burkina Faso district boundaries
- `09_rshiny_dashboard.qmd`: complete workshop lesson
- `images/`: figures used by the lesson

The indicator values are simulated for training and must not be used for
programme decisions.

## Run the application

Open this folder as an RStudio project or set it as the working directory.
Install the required packages once:

```r
packages <- c(
  "shiny", "bslib", "tidyverse", "leaflet", "sf", "plotly",
  "DT", "bsicons", "scales", "htmltools"
)

install.packages(
  packages[!packages %in% rownames(installed.packages())]
)
```

Open `app.R` and select **Run App** to work through the exercises. Open
`app_teacher.R` and select **Run App** to view the completed dashboard.

Read the local [complete workshop lesson](09_rshiny_dashboard.qmd), or use
the [rendered lesson](https://myalla-christina.github.io/ammnet-shiny-training/).
The [Session 9 blog post](https://ammnet.github.io/ammnet-hackathon/posts/shiny-dashboard/)
provides an overview and links to the materials.
