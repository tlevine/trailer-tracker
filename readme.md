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
    GET    /me                 ->  View information about your user account.
    POST   /me                 ->  Edit information about your user account.
    GET    /observations       ->  View a JSON list of all observation data.
    GET    /observation/:uuid  ->  View an observation.
    PUT    /observation/:uuid  ->  Edit an observation.
    POST   /observation/:uuid  ->  Create an observation.
    GET    /trailer            ->  View a form that lets you add a trailer observation.
    GET    /questionnaire      ->  View another question on the questionnaire.
    POST   /questionnaire      ->  Answer another question on the questionnaire.

Some of these respond differently depending on whether you're logged in.
Also, it's not entirely set.
