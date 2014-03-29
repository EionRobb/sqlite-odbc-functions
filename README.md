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
  * BIT_LENGTH
  * CHAR_LENGTH
  * CHARACTER_LENGTH
  * CONCAT
  * INSERT
  * LCASE
  * LEFT
  * LOCATE
  * LTRIM
  * OCTET_LENGTH
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
  * RAND
  * ROUND
  * SIGN
  * SIN
  * SQRT
  * TAN
  * TRUNCATE

Date and Time Functions:
------------------------
  * CURDATE
  * CURRENT_DATE
  * CURRENT_TIME
  * CURRENT_TIMESTAMP
  * CURTIME
  * DAYNAME
  * DAYOFMONTH
  * DAYOFWEEK
  * DAYOFYEAR
  * HOUR
  * MINUTE
  * MONTH
  * MONTHNAME
  * NOW
  * QUARTER
  * SECOND
  * WEEK
  * YEAR
 
System Functions:
=================
  * DATABASE
  * USER

Functions already in SQLite3 Core:
----------------------------------
  * ABS
  * CHAR
  * IFNULL
  * LENGTH

Missing Functions:
==================
  * CONVERT
  * EXTRACT
  * POSITION
  * TIMESTAMPDIFF

Some of the above function names are also SQLite keywords, so should either be called with the {fn ...} ODBC function wrapper, or by quoting the function name, eg "INSERT"(...)
