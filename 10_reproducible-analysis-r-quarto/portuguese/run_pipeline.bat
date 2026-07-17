@echo off
rem run_pipeline.bat
rem Script de lote do Windows (batch) para executar toda a pipeline de analise biomedica reprodutivel.

echo ========================================================================
echo EXECUTANDO A PIPELINE DE DADOS E COMPILANDO OS RELATORIOS
echo ========================================================================
echo.

echo 1. Executando s01_clean_data.r...
Rscript scripts\s01_clean_data.r
if %errorlevel% neq 0 (
    echo [ERRO] A limpeza de dados falhou. A terminar.
    exit /b %errorlevel%
)
echo.

echo 2. Executando s02_process_data.r...
Rscript scripts\s02_process_data.r
if %errorlevel% neq 0 (
    echo [ERRO] O processamento de dados falhou. A terminar.
    exit /b %errorlevel%
)
echo.

echo 3. Executando s03_analise_data.r...
Rscript scripts\s03_analise_data.r
if %errorlevel% neq 0 (
    echo [ERRO] A analise estatistica falhou. A terminar.
    exit /b %errorlevel%
)
echo.

echo 4. Compilando os Relatorios Quarto...
echo.

echo 4a. Compilando o Relatorio de Limpeza de Dados (HTML)...
quarto render reports\r01_data_cleaning.qmd
if %errorlevel% neq 0 (
    echo [ERRO] A compilacao de r01_data_cleaning.qmd falhou. A terminar.
    exit /b %errorlevel%
)
echo.

echo 4b. Compilando o Relatorio de Analise Estatistica Epidemiologica (HTML)...
quarto render reports\r02_data_analysis.qmd
if %errorlevel% neq 0 (
    echo [ERRO] A compilacao de r02_data_analysis.qmd falhou. A terminar.
    exit /b %errorlevel%
)
echo.

rem Alterando para o directorio reports para compilar os manuscritos.
rem Isto evita o bug do rmarkdown abs_path ao compilar multiplos formatos.
cd reports

echo 4c. Compilando os Manuscritos (HTML e Word)...
quarto render r03_manuscript.qmd
if %errorlevel% neq 0 (
    echo [ERRO] A compilacao de r03_manuscript.qmd falhou. A terminar.
    cd ..
    exit /b %errorlevel%
)
echo.

echo 4d. Compilando o Manuscrito em Word Livre de Avisos...
quarto render r03_manuscript_no_crossref.qmd
if %errorlevel% neq 0 (
    echo [ERRO] A compilacao de r03_manuscript_no_crossref.qmd falhou. A terminar.
    cd ..
    exit /b %errorlevel%
)
echo.

cd ..

echo ========================================================================
echo Sucesso! Todos os scripts foram executados e os resultados dinamicos foram compilados.
echo ========================================================================
