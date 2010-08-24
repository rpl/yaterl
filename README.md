YATErl - an Erlang binding for YATE VOIP Application Server
==========================================================

## DESCRIPTION

YATErl is a simple Erlang binding for YATE-based VOIP applications,
YATE is a VOIP Application Server developed by Null Team (http://yate.null.ro).

YATErl is designed as a standard OTP application so you can create an OTP release
for your application which include and use it to implement a VOIP application.

## DOWNLOAD AND INSTALL

TBD

## EXAMPLE USE

TBD

## FUTURE WORKS

TBD

## ARCHITECTURE

YATErl is composed by a small number of registered processes and a couple of
"almost" side-effects-free modules:

### Non-Functional Components

* yaterl_app: manage start/stop yaterl supervised tree
* yaterl_sup: supervise yaterl registered processes

### Functional Components

* yate\_stdio\_connection: 
  * connect to yate on stdio 
  * register to a yate\_control\_srv
  * send binary data to yate on stdout
  * receive binary data from yate on stdin
  * route binary data to the registered yate\_control\_srv
* yate\_control\_srv:
  * manage a yate connection
  * send / receive binary data from an active connection
  * encode / decode erlang term <-> binary yate event format
* yate\_event\_mgr:
  * register configured autoregistering yate event handler modules
  * send / receive yate events decoded as erlang records
  * register / recall processes which waits for a reply
  * route watched yate event to registerent yate event handler modules
  * send a wait for reply installed yate event to registered yate event handler modules

### API modules

* yate
  * High Level abstraction API
  * manual watch/unwatch/install/uninstall commands
  * utility commands (setlocal, output)
  * message exchanging commands (send_event \[sync or async\], wait\_for\_event)
* yate_event
  * High Level and Side Effect Free yate event manipulation
  * create new yate events commands
  * yate event type checker commands
  * yate event attributes setters/getters
* yate_message
  * High Level and Side Effect Free yate message manipulation
  * create new yate events commands
  * yate event type checker commands
  * yate event attributes setters/getters

### Internal Modules

* yate_encode: encode yate event from erlang record to binary data
* yate_decode: decode yate event from binary data to erlang record
* yate\_event\_handler: mnesia backend for yate event handlers registering

### OTP Prerequisites

* sasl: error and progress logging
* mnesia: registered yate event handlers modules backend
* kernel / stdlib
