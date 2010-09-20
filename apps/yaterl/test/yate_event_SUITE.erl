-module(yate_event_SUITE).

-compile(export_all).

-include_lib("ct.hrl").
-include("../include/yate.hrl").
-include("local_test_helpers.hrl").

all() ->
    [
     create_yate_event_without_attributes,
     create_yate_event_with_attributes,
     check_yate_event_type_helpers,
     get_yate_event_attributes,
     get_yate_event_attribute_value,
     change_all_yate_event_attributes,
     change_yate_event_attribute_value
    ].

create_yate_event_without_attributes(_Config) ->
    ExpectedYateEvent = #yate_event{
      type=install,
      direction=outgoing,
      attrs=[],
      params=[]
     },
    ExpectedYateEvent = yate_event:new(install).

create_yate_event_with_attributes(_Config) ->
    ExpectedYateEvent = #yate_event{
      type=watch,
      direction=outgoing,
      attrs=[{name, "engine.status"}],
      params=[]
     },
    ExpectedYateEvent = yate_event:new(watch, [{name, "engine.status"}]).
    
check_yate_event_type_helpers(_Config) ->
    yate_event:is_watch(yate_event:new(watch)),
    yate_event:is_install(yate_event:new(install)),
    yate_event:is_unwatch(yate_event:new(unwatch)),
    yate_event:is_uninstall(yate_event:new(uninstall)),
    yate_event:is_setlocal(yate_event:new(setlocal)),
    yate_event:is_message(yate_event:new(message)),
    yate_event:is_error(yate_event:new(error)).

get_yate_event_attributes(_Config) ->
    ExpectedAttributes = [{name, "engine.status"}],
    ExpectedAttributes = yate_event:attrs(yate_event:new(watch, ExpectedAttributes)).

get_yate_event_attribute_value(_Config) ->
    YateEvent = yate_event:new(watch, [{name, "engine.status"}]),
    "engine.status" = yate_event:attr(name, YateEvent).

change_all_yate_event_attributes(_Config) ->
    OriginalYateEvent = yate_event:new(watch, [{name, "engine.status"}]),
    ExpectedChangedYateEvent = yate_event:new(watch, [{name, "engine.status.changed"}]),
    ExpectedChangedYateEvent = yate_event:attrs([{name, "engine.status.changed"}],
                                                OriginalYateEvent).

change_yate_event_attribute_value(_Config) ->
    OriginalYateEvent = yate_event:new(watch, [{name, "engine.status"}]),
    ExpectedChangedYateEvent = yate_event:new(watch, [{name, "engine.status.changed"}]),
    ExpectedChangedYateEvent = yate_event:attr(name, "engine.status.changed",
                                               OriginalYateEvent).
    
