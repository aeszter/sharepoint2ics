# sharepoint2ics
Read a sharepoint calendar and write an appropriate ics file

Sharepoint can export ics files via owssvr.dll, but this has two disadvantages:

- individual entries are exported, but there is no support for whole calendars -- you have to loop over possible IDs until the export fails
- recurrence is not supported, an event that is supposed to run from 10 to 11 every day in June will end up starting at 10 o'clock on June 1st and last until 11 on June 30th

This project tries to remedy the situation by using Sharepoint's SOAP API to fetch events and building the ics file itself.

## Shortcomings
Ada's web library (AWS) does not support NTLM, the authentication mechanism employed by sharepoint. 
Therefore, sharepoint2ics uses a hand-crafted XML file and wget to perform the SOAP call.
In the current, early versions, this has to be done externally (e.g. by a shell script). 
The input file is supplied as part of the project, and the _seminar_ binary will convert the received XML file to ics.

In future versions, sharepoint2ics is expected to call wget on its own, and the user just has to supply a config file containing Sharepoint URL and credentials.
