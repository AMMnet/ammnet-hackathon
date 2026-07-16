# Session 9: R Shiny malaria dashboard

These are the essential files for the AMMnet workshop on building an
interactive malaria dashboard with R Shiny.

## Files

- `app.R`: starter application for participants
- `app_teacher.R`: completed application for reference
- `data/malaria_data.rds`: simulated monthly district indicators
- `data/annual_summary.rds`: simulated annual summary indicators
- `data/bfa_districts.rds`: Burkina Faso district boundaries

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

See the
[Session 9 blog post](https://ammnet.github.io/ammnet-hackathon/posts/shiny-dashboard/)
for the workshop instructions and links to the complete lesson.
