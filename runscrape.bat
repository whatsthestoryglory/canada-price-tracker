@ECHO OFF 
:: This batch file lists all .R files in the scrapers directory and executes them with Rscript

for %%v in (scrapers/*.R) do start "" "c:\Program Files\R\R-4.0.5\bin\Rscript.exe" "scrapers/%%v"