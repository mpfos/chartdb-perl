# chartdb-perl
Perl Module to download NOAA charts in ENC or RNC formats and chart corrections.

Running the module as a script:

 ~/chartdb-perl$ perl ChartDB.pm

will execute the run subroutine, and start processing charts with the current 
working directory as reported by Path::Tiny assumed to be the database root.

The CHARTS.TXT manifest file provides the minimum list of charts to process.  
The database directories will be adjusted to accomodate the minimum list.  The 
format of the manifest file is:

cor:16700:
enc:US5GA14M:
rnc:16700:

for corrections, vector charts, and raster charts. This file is updated with 
the whole charts processed list on exit.
