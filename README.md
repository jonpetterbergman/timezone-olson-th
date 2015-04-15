# timezone-olson-th
Load Olson timezone files at compile time

For Example:
    myTimeZoneSeries :: TimeZoneSeries
    myTimeZoneSeries = $(loadTZFile "/usr/share/zoneinfo/Europe/Stockholm")

