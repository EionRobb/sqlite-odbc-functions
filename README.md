sqlite-odbc-functions
=====================

Adds ODBC-compatible functions to SQLite, eg to use with the ODBC driver at http://www.ch-werner.de/sqliteodbc/
Based on 'extension-functions.c' by Liam Healy from http://www.sqlite.org/contrib/
Code released into the public domain.

See http://msdn.microsoft.com/en-us/library/ms711813(v=vs.85).aspx for ODBC functions

Supported ODBC functions:
=========================

String Functions:
-----------------
  * ASCII
  * CONCAT
  * LCASE
  * LEFT
  * LOCATE
  * LTRIM
  * REPEAT
  * REPLACE
  * RIGHT
  * RTRIM
  * SOUNDEX
  * SPACE
  * SUBSTRING
  * UCASE

Numeric Functions:
------------------
  * ACOS
  * ASIN
  * ATAN
  * ATAN2
  * CEILING
  * COS
  * COT
  * DEGREES
  * DIFFERENCE
  * EXP
  * FLOOR
  * LOG
  * LOG10
  * MOD
  * PI
  * POWER
  * RADIANS
  * ROUND
  * SIGN
  * SIN
  * SQRT
  * TAN

Date and Time Functions:
------------------------
  * CURDATE
  * CURTIME
  * DAYOFMONTH
  * DAYOFWEEK
  * DAYOFYEAR
  * HOUR
  * MINUTE
  * MONTH
  * NOW
  * QUARTER
  * SECOND
  * WEEK
  * YEAR

Missing Functions:
==================
  * BIT_LENGTH (ODBC 3.0)
  * CHAR_LENGTH (ODBC 3.0)
  * CHARACTER_LENGTH (ODBC 3.0)
  * CONVERT
  * CURRENT_DATE (ODBC 3.0)
  * CURRENT_TIME (ODBC 3.0)
  * CURRENT_TIMESTAMP (ODBC 3.0)
  * DATABASE (ODBC 1.0)
  * DAYNAME (ODBC 2.0)
  * EXTRACT (ODBC 3.0)
  * INSERT (ODBC 1.0)
  * MONTHNAME (ODBC 2.0)
  * OCTET_LENGTH (ODBC 3.0)
  * POSITION IN (ODBC 3.0)
  * RAND (ODBC 1.0)
  * TIMESTAMPDIFF (ODBC 2.0)
  * TRUNCATE (ODBC 2.0)
  * USER (ODBC 1.0)

Functions in SQLite3 Core:
--------------------------
  * ABS
  * CHAR
  * IFNULL
  * LENGTH
  
