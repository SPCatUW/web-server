# Web server

Code for web server that accepts print jobs, relays jobs to print stations, and provides information on Cooperative.

## The server software
Implemented in Common Lisp.  Depends on [Hunchentoot](https://edicl.github.io/hunchentoot), [CL-WHO](https://edicl.github.io/cl-who/).
Tested on [SBCL](sbcl.org).

## Backend
Currently, the server only supports jobs submitted by paid members ("users").
Support for jobs submitted by non-paying users will be added in the near future.

All URIs for the backend start with "/backend".  All requests to such a URI
must be authenticated using [HTTP Basic](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) [authentication](http://tools.ietf.org/html/7617)
(connections will be encrypted prior to deployment).  Clients who supply
incorrect credentials will receive an HTTP 401 (Authorization Required)
response.

### Getting user queues
Clients may obtain a user's list of pending jobs by sending am HTTP GET request
to the following URI:

  </backend/user/queue>

The user's username and password are to be supplied as the GET parameters
"username" and "password", respectively (no quotation marks; again, connections
will be encrypted).  These user credentials are separate from the client
credentials used for the HTTP Basic Authentication of all /backend requests.
If the supplied user credentials match those stored on the server, then the
server will respond with a JSON dictionary of the following form:

   { "username" : <username>,
     "jobs" : [[<job-number-0>, <job-filename-0>],
               [<job-number-1>, <job-filename-1>],
               .
               .
               .
               [<job-number-n>, <job-filename-n>]]}

<username> and <job-filename-*> are strings; <job-number-*> is a number
that functions as each job's unique identifier.

An example:  ;; comments preceded by semicolons

  ;; Get user "starfighter"'s print-queue.  In addition to supplying
  ;; starfighter's username and password as GET parameters, the print
  ;; station client must supply its own username/password via HTTP
  ;; Basic Authentication.

  GET /backend/user/queue?username=starfighter&password=kjohnson
  ;; [Omitted HTTP Basic Authentication back-and-forth]
  
  ;; Response
  {"username" : "starfighter", "jobs" : [[0, "MLA-README"], [1, "final-paper.pdf"]]}


### Getting actual files for the print job
Each print job has a file associated with it that the user submitted for
printing.  One can download the file by sending an authenticated GET request
to the following URI:

  </backend/file>

The job's number and the username and password of the user who submitted the
job must be sent as the GET parameters "job-number", "username", and "password",
respectively.  If the job exists, the user credentials are valid match that of
the user who actually submitted the job, then the file is transmitted to the
client.  Nonexistent jobs will return a 404 error; improper user credentials
will return a 403 (Forbidden) error.

An example:  ;; Download the file for job number 0, user "starfighter"

  GET /backend/file?job-number=0&username=starfighter&password=kjohnson
  ;; Credentials are correct.  Job's file will be transmitted if it exists.
  [file]
