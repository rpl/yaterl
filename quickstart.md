---
title: YATErl - Quickstart
layout: default
---

Quickstart
==========

* [Download and install](#download_and_install)
* [Create a new project skeleton](#create_a_new_project_skeleton)
* [Pack and load it](#pack_and_load_it)
* [Module Code Review](#module_code_review)

Download and install
--------------------

**YATErl 0.0.1** is a technology preview so there isn't any pre-build distribution and
you have to download and build it from sources:

<pre>
$ git clone http://github.com/alcacoop/yaterl.git
</pre>

**YATErl** git repository include two different Erlang applications:

* **yaterl**: a simple Erlang library application, it depends only on 
  standard Erlang OTP libraries;
* **yaterl_devtool**: a simple Erlang command line application, it 
  depends on [getopt](http://github.com/jcomellas/getopt) and 
  [erlydtl](http://github.com/evanmiller/erlydtl) at build time, and it's packaged 
  as an escript executable archive (which will contain yater_devtool and 
  its dependecies beam files)
  
We're using [rebar](http://github.com/basho/rebar) (already included into the source tree)
and make as build tools:

<pre>
$ make
make -C apps/yaterl compile
...
Installing: yaterl-0.0.1 to ../../builds/
...
make -C apps/yaterl-devtool compile
...
SUCCESS: Wrote yaterl-devtool into ../../builds/yaterl-devtool
...
make -C examples/route compile
...
SUCCESS: packed script to '../../builds/examples/route.escript'
...
make -C examples/resolver compile
...
SUCCESS: packed script to '../../builds/examples/resolver.escript'
...
make -C examples/registration compile
...
SUCCESS: packed script to '../../builds/examples/registration.escript'
...
$
</pre>

If all building tasks will be completed successful, now you should have a new *builds*
directory like this:

<pre>
$ ls builds
examples  yaterl-0.0.1  yaterl-devtool
</pre>

Now you can add yaterl-0.0.1 to your erlang libraries load paths and yaterl\_devtool
to your application path, as example:

<pre>
$ sudo cp -rf builds/yaterl-0.0.1 /usr/lib/erlang/lib/
...
$ sudo cp builds/yaterl-devtool /usr/local/bin/
...
</pre>

Create a new project skeleton
-----------------------------

You can now create a new YATE module using yaterl\_devtool:

<pre>
$ yaterl-devtool generate my_first_yate_module
</pre>

**generate** yaterl\_devtool command will create a base *yaterl\_gen\_mod* project
skeleton:

<pre>
$ ls my_first_yate_module
ebin src include
$ ls my_first_yate_module/src
my_first_yate_module.app.src my_first_yate_module.erl
</pre>

You can now build it with rebar:

<pre>
$ cd my_first_yate_module
$ ../build-tools/rebar compile
==> my_first_yate_module (compile)
Compiled src/my_first_yate_module.erl
$ ls -l ebin
my_first_yate_module.app  my_first_yate_module.beam
</pre>

Pack and load it
----------------

We can now use **pack** yaterl\_devtool command to package the module into a convenient
escript executable archive:

<pre>
$ ../builds/yaterl-devtool pack my_first_yate_module.escript my_first_yate_module
SUCCESS: packed script to 'my_first_yate_module.escript'
</pre>

*my\_first\_yate\_module.escript* is a valid yate extmodule, that will do nothing other
than connect to yate using stdio:

<pre>
$ ./my_first_yate_module.escript

READ: 

READ: 
123
READ: 123
</pre>

Module Code Review
------------------

In pure Erlang spirit, **YATErl** create an OTP-based infrastructure so you can write
pure sequential code in your *yaterl\_gen\_mod behaviour* implementation, or you can
hook to your own OTP-based infrastructure using the same callbacks (in case of more 
complex applications).

*yaterl\_gen\_mod* callbacks:
* **connection\_available**
* **subscribe\_config / subscribe\_error**
* **handle\_install\_message** / **handle\_watch\_message**

If your module will be packaged as an escript executable archive you will need
a main callback, which will configure and start your node environment:

<pre>
config() ->
    LogDir = "/tmp/",
    BaseLogFileName = [LogDir,atom_to_list(?MODULE),"_",os:getpid()],
    yaterl_config:log_files(BaseLogFileName++".log", 
                            BaseLogFileName++"_sasl.log"),
                            
    %%% Configure yaterl to use this module as yaterl_gen_mod implementation
    yaterl_config:yaterl_custom_module_name(?MODULE),
    ok.

main(_) ->
    ok = config(),
    ok = application:start(sasl),
    ok = application:start(yaterl),
    timer:sleep(infinity).
</pre>

When **YATErl** connection module will be ready, your *connection\_available*
callback will be called and you can ask **YATErl** to start a subscribe sequence
or do nothing returning a *start\_subscribe\_sequence* or *do\_nothing* atom:

<pre>
connection_available() ->
    start_subscribe_sequence. % or do_nothing
</pre>

If you return *start\_subscribe\_sequence*, **YATErl** will call *subscribe\_config*
callback to get a list of subscribe requests:

<pre>
subscribe_config() ->
    error_logger:error_msg("SUBSCRIBE CONFIG"),
    [{"call.route", install, 80}].
     {"engine.status", install, 80, {filters, [{"module", "conference"}]}}].
</pre>

If during subscribing **YATErl** recognites any errors your *subscribe\_error* will
be called:

<pre>
subscribe_error(_LastResponse, _LastRequest) ->
    error_logger:error_msg("SUBSCRIBE ERROR... EXITING"),
    init:stop(1).
</pre>

On any yate event of type *message*, your *handle\_install\_message* or
*handle\_watch\_message* callbacks will be called:

* *handle\_install\_message*, if you haven't completed any yate message subscribing
  or you have subscribe its message name with install level.
* *handle\_watch\_message* , if you have subscribe its message name with watch level.

*handle\_install\_message* have to return a valid reply using *yaterl\_gen\_mod:reply*
or *yaterl\_gen\_mod:ack* helpers, whereas  *handle\_watch\_message* return value will 
be ignored:

<pre>
handle_install_message(YateMessage) ->
    case {yate_message:name(YateMessage),
          yate_event:direction(YateMessage)} of
        {"call.route", incoming} ->
            log(YateMessage);
        _AnyOther -> log(YateMessage)
    end.

log(YateMessage) ->
    error_logger:info_msg("IGNORED Msg: ~p~n", [YateMessage]),
    yaterl_gen_mod:ack(YateMessage).
</pre>

More info on *yaterl\_gen\_mod* from [apidoc](apidoc/yaterl_gen_mod.html)
