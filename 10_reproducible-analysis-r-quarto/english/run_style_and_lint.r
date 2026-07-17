## run_style_and_lint.r
## Automatically style and lint the R scripts in this project.

# Load packages
library(here)
library(styler)
library(lintr)

# Define directories to check
target_dirs <- c(
  here::here("scripts")
)

# 2. Run Styler (Formatting) ------------------------------------------------
cat("========================================================================\n")
cat("1. STYLING R CODE FILES (styler)\n")
cat("========================================================================\n")

for (dir in target_dirs) {
  if (dir.exists(dir)) {
    cat("Styling directory:", basename(dir), "\n")
    styler::style_dir(dir)
  } else {
    cat("Directory not found:", dir, "\n")
  }
}
cat("\n")

# 3. Run Lintr (Style and Syntax Checking) ----------------------------------
cat("========================================================================\n")
cat("2. LINTING R CODE FILES (lintr)\n")
cat("========================================================================\n")

any_lints <- FALSE
for (dir in target_dirs) {
  if (dir.exists(dir)) {
    cat("Checking directory:", basename(dir), "\n")
    lint_results <- lintr::lint_dir(dir)

    if (length(lint_results) > 0) {
      any_lints <- TRUE
      print(lint_results)
    } else {
      cat("✔ No lints found in", basename(dir), "\n")
    }
  }
}

cat("========================================================================\n")
if (any_lints) {
  cat("Status: Done. Please review and fix the lint issues listed above.\n")
} else {
  cat("Status: Success! All scripts are clean and properly formatted.\n")
}
cat("========================================================================\n")
