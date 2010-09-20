-define(LOG_PROCS, 
        ct:pal("LOG_PROCS: ~p~n", [lists:map(fun(I) ->
                                                     erlang:process_info(I)
                                             end, erlang:processes())])).

-define(CT_LOG(Value), ct:pal("CT_LOG: ~p~n", [Value])).

-define(GET_SRV_STATUS(Name), (fun (Name) ->
                                 {status, _Pid, _ModInfo, 
                                  [_PDict, _SysState, _Parent, 
                                   _Dbg, Misc]} = sys:get_status(Name),
                                 [_ModuleName, State | _ ] = Misc,
                                 State
                         end)(Name)).
