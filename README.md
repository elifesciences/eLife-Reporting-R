eLife-Reporting-R
=================

R code for reporting

These three scripts take a csv output from the database in eLife-Reporting-SQL as input and are run in the order manuscript_data > summary_data > manuscript_charts. The output is a new directory containing summary data csv files and charts.

Limitations:
- File input path is hardcoded
- File output path is hardcoded
- Dates are hardcoded and must be changed manually to change the date range of the output

In it's current state this is *quite* janky and could use some tidying up.

