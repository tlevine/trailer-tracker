Trailer Tracker
=====
This website collects information about peoples' trailers.

The homepage has the map.

You click something to add to the map. Within this view, you first are asked
for your trailer's information and, optionally, for your email address and
phone number. After this view, more questions appear.


## Outline
All of the dynamic stuff happens on the server; we only use Javascript for
making the map. Unless otherwise specified, all pages return HTML.

    GET    /
    GET    /login
    POST   /login
    GET    /logout
    POST   /logout
    GET    /me                   ->  View information about your user account.
    POST   /me                   ->  Edit information about your user account.

    GET    /questionnaires       ->  View a JSON list of all questionnaire data.
    GET    /questionnaire/:uuid  ->  View an questionnaire.
    PUT    /questionnaire/:uuid  ->  Answer more questionnaire questions.
    POST   /questionnaire/:uuid  ->  Create an questionnaire.

    GET    /observations         ->  View a JSON list of all observation data.
    GET    /observation/:uuid    ->  View an observation.
    PUT    /observation/:uuid    ->  Edit an observation.
    POST   /observation/:uuid    ->  Create an observation.

Some of these respond differently depending on whether you're logged in.
Also, it's not entirely set.

## Notes
text for the start page?
Welcome to Trailer Tracker! 
This website provides consumer information relating 
to the indoor air quality issues of mobile or modular homes.

The baseline data comes from the federal government
 but we need users like you to add  your own knowledge 
 for the benefit of the community. 

     Then below it, and flanked by two sets of thin parallel lines was the following text in slightly larger font:

     To get started:
      click on “TRACKER” in the menu 
      at the top of the screen.

