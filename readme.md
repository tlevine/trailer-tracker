Trailer Tracker
=====
This website collects information about peoples' trailers.

The homepage has the map.

You click something to add to the map. Within this view, you first are asked
for your trailer's information and, optionally, for your email address and
phone number. After this view, more questions appear.


## Backend

`GET/POST/PATCH /observations/:uuid` is the endpoint for a trailer observation. It looks
something like this.

    {
      "id":"hoen-uohonu-oho-eohoe-oh",
      "uid":2841,
      "date_created":"2013-04-13 11:28:05"
      "questions": {
        "name":["2013-04-13 11:29:31","Thomas Levine"],
        "zipcode":[...,...],
        ...
      }
    }

The `id` functions as a password, so people can edit their observations
later even if they don't make an account. If they do make an account, they
can also edit by logging in. The `questions` dictionary contains tuples
of `(date edited,value)`. The `uid` refers to a table with documents like this.

    {
        "id":2841,
        "name":"Thomas Levine",
        "email":"foo@bar.baz",
        "shadow":"Hu8eonRQk7GRvmuoNH729723hnoeurg23"
    }

All edits (`POST` and `PATCH` requests) are saved, so the data can be played
back.
