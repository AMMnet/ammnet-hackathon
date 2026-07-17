@echo off
rem run_pipeline.bat
rem Windows batch script to run the entire reproducible biomedical analysis pipeline.

echo ========================================================================
echo RUNNING DATA PIPELINE AND COMPILING REPORTS
echo ========================================================================
echo.

echo 1. Running s01_clean_data.r...
Rscript scripts\s01_clean_data.r
if %errorlevel% neq 0 (
    echo [ERROR] Data cleaning failed. Exiting.
    exit /b %errorlevel%
)
echo.

echo 2. Running s02_process_data.r...
Rscript scripts\s02_process_data.r
if %errorlevel% neq 0 (
    echo [ERROR] Data processing failed. Exiting.
    exit /b %errorlevel%
)
echo.

echo 3. Running s03_analise_data.r...
Rscript scripts\s03_analise_data.r
if %errorlevel% neq 0 (
    echo [ERROR] Data analysis failed. Exiting.
    exit /b %errorlevel%
)
echo.

echo 4. Rendering Quarto Reports...
echo.

echo 4a. Rendering Data Cleaning Report (HTML)...
quarto render reports\r01_data_cleaning.qmd
if %errorlevel% neq 0 (
    echo [ERROR] Rendering r01_data_cleaning.qmd failed. Exiting.
    exit /b %errorlevel%
)
echo.

echo 4b. Rendering Epidemiological Data Analysis Report (HTML)...
quarto render reports\r02_data_analysis.qmd
if %errorlevel% neq 0 (
    echo [ERROR] Rendering r02_data_analysis.qmd failed. Exiting.
    exit /b %errorlevel%
)
echo.

rem Shift to the reports directory to render manuscripts.
rem This prevents the rmarkdown abs_path bug when rendering multiple formats.
cd reports

echo 4c. Rendering Manuscripts (HTML and Word)...
quarto render r03_manuscript.qmd
if %errorlevel% neq 0 (
    echo [ERROR] Rendering r03_manuscript.qmd failed. Exiting.
    cd ..
    exit /b %errorlevel%
)
echo.

echo 4d. Rendering Warning-Free Word Manuscript...
quarto render r03_manuscript_no_crossref.qmd
if %errorlevel% neq 0 (
    echo [ERROR] Rendering r03_manuscript_no_crossref.qmd failed. Exiting.
    cd ..
    exit /b %errorlevel%
)
echo.

cd ..

echo ========================================================================
echo Success! All scripts executed and dynamic outputs compiled.
echo ========================================================================
