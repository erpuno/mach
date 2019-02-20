-module(mach).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1]).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/n2o.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).

money(Double) -> wf:f("~.2f",[Double]).

log_function()   -> application:get_env(mach, errors, {?MODULE,log}).
mock_services()  -> application:get_env(mach, mock, modules()).
dev()            -> application:set_env(mach, mock, modules()).
ops()            -> application:set_env(mach, mock, []).
modules()        -> [mach_ivr].

service(Stack,A,Time,Acc) ->
    Res  = try case pipe(1,Stack,A,Time,Acc) of
                   {error,Data,NewAcc} -> {error,Data,{Stack,NewAcc}};
                   {warning,Data,NewAcc} -> {warning,Data,{Stack,NewAcc}};
                   {refusal,Data,NewAcc} -> {refusal,Data,{Stack,NewAcc}};
                   {errorCode,Data,NewAcc} -> {errorCode,Data,{Stack,NewAcc}};
                   {ok,Data,NewAcc} -> {ok,Data,{Stack,NewAcc}};
                   Unknown -> Unknown end
           catch Error:Reason -> fatal(Error,Reason,{Stack,Acc}) end,
    Res.

pipe(_,{_,[]},A,_,Acc)         -> {ok,A,Acc};
pipe(Retry,Stack,A,Time,Acc)   -> pack(Retry,Stack,A,Time,Acc).

pack(_,{{P,RA},[{Mock,Type,Method,M,{F,G},Retry,Addr}|T]}=Stack,A,Time,Acc) ->
       try case M:F(A,Acc) of
               {ok,Ret,NewAcc} -> call(Retry,Stack,Ret,Time,[{F,A}|NewAcc]);
       {warning,Reason,NewAcc} -> {warning,Reason,NewAcc};
         {error,Reason,NewAcc} -> {error,Reason,NewAcc} end
       catch E:R -> fatal(E,R,{Stack,[{call,{<<"not yet formed">>,R,os:timestamp()}}|Acc]}) end.

call(Retry,{{_,RA},[{Mock,Type,Method,M,{F,G},A,Addr}|T]}=Stack,Req,Time,Acc) when Retry < 1 -> {error,{attempts,A},Acc};
call(Retry,Stack,Req,Time,[{call,{_,_,_}}|Acc]) -> call(Retry,Stack,Req,Time,Acc);
call(Retry,{{P,RA},[{Mock,Type,Method,M,{F,G},_,Addr}|T]}=Stack,Req,Time,Acc) ->
       case is_atom(Mock) of
            true -> skip;
            false -> erlang:put(mock,Mock) end,
       Start = os:timestamp(),
       try  case req_ret(request(P,M,[ Method,
                                addr(RA,Addr,Acc),
                                P:headers([{req,wf:to_binary(Req)},
                                           {method,string:to_upper(wf:to_list(Method))},
                                           {type,type_app(Type)}|Acc]),
                                type_content(Type),
                                Req])) of
                   {ok,Answer} -> unpack(Retry,Stack,Answer,Time,[{call,{Req,Answer,{Start,os:timestamp()}}}|Acc]);
                {error,{requestError,{error,socket_closed_remotely}}} -> case T of
                                      [] -> call(Retry-1,Stack,Req,Time,Acc);
                                       _ -> pipe(Retry,{{P,RA},T},Req,Time,[{G,Req}|Acc]) end;
                {error,Reason} -> {error,Reason,[{call,{Req,Reason,{Start,os:timestamp()}}}|Acc]} end
                     catch E:R -> fatal(E,R,{Stack,[{call,{Req,R,Start}}|Acc]}) end.

unpack(Retry,{{P,RA},[{_,_,_,M,{F,G},_,_}|T]}=Stack,A,Time,Acc) ->
    {call,{Req,_,_}} = hd(Acc),
    try case M:G(A,Acc) of
            {ok,Ret,NewAcc} ->
                pipe(Retry,{{P,RA},T},Ret,Time,[{G,A}|NewAcc]);
            {State,Reason,NewAcc} when State==warning ; State==error ; State==errorCode ; State==refusal ->
                {State,Reason,NewAcc};
            {repeat,NewAcc} -> timer:sleep(Time),
                wf:info(?MODULE, "Retry ~p: ~p~n",[Retry, hd(Acc)]),
                call(Retry-1,Stack,Req,Time,Acc) end catch E:R -> fatal(E,R,{Stack,Acc}) end.


% for internal protocol

strip({ok,Ret,_}) -> {ok,Ret};
strip({error,Ret,_}) -> {error,Ret};
strip({errorCode,Ret,_}) -> {errorCode,Ret};
strip({refusal,Ret,_}) -> {refusal,Ret};
strip({warning,Ret,_}) -> {warning,Ret};
strip({repeat,_}) -> {repeat,[]};
strip(X) -> X.

% for ACT and WEB services
ret(X) -> X.

req_ret({ok,{{_,Code,_},_,Answer}}) when Code>=200, Code<300 -> {ok,Answer};
req_ret({ok,{Status,_,Body}}) -> {error,{httpError,Status,Body}};
req_ret(Error)             -> {error,{requestError,Error}}.

log(_,{error,Error})     -> wf:error(?MODULE, "Error Module: ~n~p", [Error]);
log(_,Result)            -> Result.

mock(undefined) -> 1;
mock(M)         -> M.

http_request(P,M,Mock,[Method,Addr,Headers,Type,Data]=Args) -> mock_request(P,M,Mock,Args).

mock_request(P,M,true,[Method,Addr,Headers,Type,Data]=Args) ->
    M:mock(mock(get(mock)),Args);
mock_request(P,M,false,[get,Address,Headers,Type,Data]) ->
     io:format("HTTP ~p ~p ~p~n",[Type,get,Address]),
    httpc:request(get,{Address, Headers},http_options(P,exported(P,http_options,0)), options(P,exported(P,options,0)));
mock_request(P,M,false,[Method,Address,Headers,Type,Data]) ->
     io:format("HTTP ~p ~p ~p~n",[Type,Method,Address]),
    httpc:request(Method,{Address, Headers, Type, Data},http_options(P,exported(P,http_options,0)),
                                                             options(P,exported(P,options,0))).

mock_replace(Chain,Mocks) ->
     { element(1,Chain), [setelement(6,setelement(1,C,M),1)
                      || {C,M} <- lists:zip(element(2,Chain),Mocks) ]}.

type_content(xml)     -> "text/xml;charset='utf-8';";
type_content(xform)   -> "application/x-www-form-urlencoded";
type_content(stream)  -> "application/octet-stream";
type_content(json)    -> "application/json".
type_app(xml)         -> "application/xml";
type_app(xform)       -> "text/xml";
type_app(stream)      -> "application/octet-stream";
type_app(json)        -> "application/json".
xml()                 -> "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>".
xml(Data)             -> wf:to_binary(lists:flatten(xmerl:export_simple(Data, xmerl_xml, [{prolog, xml()}]))).
fatal(E,R,ReqParams)  -> {fatal, n2o_error:stack(E,R), ReqParams}.
addr(RA,{params,Fun},Acc) -> Fun(wf:to_list(mach:bank()),Acc);
addr(RA,Addr,Acc)     -> lists:concat([RA,Addr,wf:to_list(proplists:get_value(resource,Acc,[]))]).
request(P,M,Args)     -> http_request(P,M,lists:member(P,mock_services()),Args).
protocol(X) when is_tuple(X) -> lists:member(element(1,X),[ok,warning,error,errorCode,refusal,repeat]);
protocol(_) -> false.
test()                -> L2 = lists:flatten([ begin erlang:put(mock,undefined),
                                               Service:test()
                                           end || Service <- mock_services() ]),
                         {Passed,Failed} = lists:partition(fun({}) -> true;
                                           (Y) ->
                                           io:format("Passed: ~p~n",[Y]),
                                           protocol(Y) end,lists:flatten([ strip(X) || X <- L2 ])),
                        if Failed /= [] -> io:format("Failed: ~p~n",[Failed]); true -> skip end,
                        [{passed,length(Passed)},
                         {failed,length(Failed)}].
exported(M,F,A)       -> erlang:function_exported(M,F,A).
http_options(M,true)  -> M:http_options();
http_options(M,false) -> [].
options(M,true)       -> M:options();
options(M,false)      -> [].
host(M)               -> M:host(wf:to_list(bank())).
bank()                -> wf:to_binary(wf:config(n2o, bank, n2o_error)).
ref()                 -> ref("mach").
ref(Name)             -> {Date,Time} = calendar:local_time(),
                         wf:to_binary(lists:concat([Name,
                              wf:to_list(mach:dateToInt(Date)),
                              wf:to_list(mach:timeToInt(Time)),
                              wf:to_list(crypto:rand_uniform(10000,99999))])).

dump() -> {{srv,length(servers()),servers()},
           {mnt,length(endpoints()),endpoints()}}.

servers() -> [ {Service:mocks(),Service,host(Service)} || Service <- modules() ].
endpoints() -> [ {Retry,Address,Type,Method} ||
                 {_,Type,Method,Module,{_,_},Retry,Address} <-
                     lists:flatten([ Links || {{_,_},Links} <-
                           lists:flatten([ Service:pipe() || Service <-
                                modules() ]) ]), Address /= [] ].

all() -> lists:flatten([ {Service,Service:pipe()} || Service <- modules() ]).

start()    -> spawn(fun() -> [ supervisor:start_child(mach,Spec) || Spec <- supervision() ] end).
start(_,_) -> supervisor:start_link({local,mach},mach,[]).
stop(_)    -> ok.
init([])   -> application:stop(services_bank), mach:ops(), {ok, {{one_for_one, 5, 10}, supervision() }}.

supervision() -> [].

till(Now,TTL) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + TTL).

