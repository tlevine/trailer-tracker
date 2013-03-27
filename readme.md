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

## Integration tests to be written

* If I submit valid information to the trailer form,
  * the page submits
  * I see whatever I expect
  * I can go to the page again and see the information.
* If I submit invalid information to the trailer form, I get an appropriate error.
* The homepage loads leaflet and seems to make a map. (The test probably won't actually run the JavaScript though.)
* If I POST a new trailer, it is reflected in the /trailers map data.
* If I submit a form but the session is expired or otherwise bad, I am allowed to log in or register and then see the data again.
    (This situation might not come up because of how little you actually need to be authenticate for.)

## Ideas from talking with Nick on March 27
* Use IP address
  * Hmm maybe cookies
    * Consider cookie policies
    * Display the policy only in European Union
  * He worries about the chemical industry or resellers trying to fuck with us.
  * At least log everything.
* "Track" a trailer
  * After tracking, send to symptoms so there is no interruption
  * or another trailer
* Use the wide format with dummy variables for checkboxes (multiple columns).
* Show _your_ trailer's story _after_ you submit information about the trailer.
  * Make them do all of the trailer questions
  * Make them do some more?
* Link the symptom and trailer questionnaires for one respondent.
* But getting other peoples' trailers is interesting.
  * People are sort of "vigilante".
* Multiple people in the family
* The other data is a baseline for
  * starting the stories
  * starting the map
* Map filtering
  * Movements
  * Sale date
  * Depend on how users use the site?
    * Beta test on mailing lists before writing press releases
  * Highlight different segments
    * Types of sales: auctions, resales, &c.
    * Time
    * On indian reservations, oil fields, &c.
    * Whether people's health diminished
    * A single trailer, staging area, &c.
  * Refund program (duplicated VIN in spreadsheet)
  * Press release specific to New Orleans

* About variatons symptoms
  * On one sheet of plywood, there can be a three-fold difference in emissions depending on where you test.
  * Variability within components
  * Nobody's done it by plant + model + time
  * Jacking up with cinderblocks
  * Personal data

This is more than FEMA trailers.
  * Mobile homes are the fastest-growing housing market in America.

Immediate gratification
  * Track your trailer
    * Mitigation strategies
  * Symptoms
    * Is my trailer a FEMA trailer?
    * Consumer products
  * Nick offered free testing for FEMA trailers, and this is how his name got around.

Tracking anything serialized
  * Weapons
    * Bullets
    * Guns
    * Black market
