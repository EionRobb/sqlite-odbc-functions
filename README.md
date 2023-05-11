sqlite-odbc-functions
=====================

Adds ODBC-compatible functions to SQLite, eg to use with the ODBC driver at http://www.ch-werner.de/sqliteodbc/
Includes some code based on on 'extension-functions.c' by Liam Healy from http://www.sqlite.org/contrib/
Code released into the public domain.

Installation
============
On Windows, download the dll's from https://github.com/EionRobb/sqlite-odbc-functions/releases and copy to C:\Windows\System32 (renaming the `_64bit.dll` to just `.dll` if using 64bit Windows)

Usage
=====
Add to the DSN with ";LoadExt=sqlite_odbc_functions", eg "DRIVER={SQLite3 ODBC Driver};Database=:memory:;JDConv=1;LoadExt=sqlite_odbc_functions;OEMCP=1;OEMCPTranslation=1;".  This can also be done at runtime using `load_extension()` https://www.sqlite.org/lang_corefunc.html#load_extension eg `SELECT load_extension('sqlite_odbc_functions');`
Calling standard ODBC functions should be done with the `{fn ...}` syntax, eg `{fn LCASE('Your Text Here')}` to be portable across other ODBC drivers.  The SQLite ODBC driver will remove the `{fn` syntax internally, so you can optionally call the function directly, eg `LCASE('Your Text Here')` however this is less-portable.

Supported ODBC functions:
=========================
See http://msdn.microsoft.com/en-us/library/ms711813(v=vs.85).aspx for ODBC function definitions

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
