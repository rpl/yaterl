---
title: YATErl
layout: default
---

YATErl <br/> <small><i>bring Erlang back to telephony applications</i></small>
==============================================================================

**YATErl** is a simple Erlang library developed by 
[Alca Società Cooperativa](http://alcacoop.it) to create 
YATE-based VOIP applications using the powerful feature of 
the Erlang language and OTP framework.

**YATErl** is designed as a standard OTP application so you can create an OTP release
for your application which include and use it to implement a VOIP application.

**YATE** is a VOIP Application Server developed by [Null Team](http://yate.null.ro),
coded in C++ using message-passing techniques, and actually scriptable in PHP
and Python.

* [Quickstart](quickstart.html)
* [Examples](examples.html)
* [APIDOC](apidoc/index.html)
* [GitHub source repository](http://github.com/alcacoop/yaterl)

Why I should care?
------------------

Erlang was designed to improve the development of telephony applications...
so why don't **"Render unto Caesar the things which are Caesar’s"**.

YATE-based applications should receive and send a lot of asynchronous events
related to different concurrent VOIP users and Erlang has a lot of native 
features to simplify this kind of tasks:
* pattern matching
* lightweight processes 
* supervising trees
* hot code upgrades

_Do you need more reasons?_



