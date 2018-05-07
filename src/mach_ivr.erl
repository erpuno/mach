-module(mach_ivr).
-include("api.hrl").
-export(?API).
-export([ivr/1]).
-compile(export_all).

mocks()        -> 1.
host(_)     -> "https://ns.synrc.com".
headers(Acc)   ->
    Protocol = proplists:get_value(protocol, Acc),
    case Protocol of
        post ->
            [
                {"content-type", "application/json"},
                {"accept", "application/json"},
                {"connection", "keep-alive"},
                {"content-Length", size(proplists:get_value(req,Acc))}
            ];
        get ->
            [
                {"accept", "application/json"},
                {"Cache-Control","no-cache"}
            ]
    end.

http_options() -> [{timeout,5000}, {connect_timeout,2000}].
options()      -> [{sync,true}, {body_format,binary}, {full_result,true}].
pipe()         -> {{?MODULE, mach:host(?MODULE)},[{1,json, post, ?MODULE, {req1, ans1}, 1, "/register/"},
                                                {1,json, get, ?MODULE,  {req2, ans2}, 300, "/check/"}]}.

ivr(Args = [Phone,Lang,Event]) ->
    mach:ret(mach:service(pipe(), Args, 2000, [])).

test() -> [fun (Mock) -> erlang:put(mock,Mock),
   ivr([<<"+380910001122">>, <<"ua">>, 'EXT_SITE']) end (Mock) || Mock <- lists:seq(1,mocks())].

req1([Phone, Lang, Event], Acc) ->
    io:format("REQ1~n"),
    JSON = jsone:encode({
        [
            {<<"sys">>,<<"depsber">>},
            {<<"phone">>,Phone},
            {<<"evt">>,Event},
            {<<"lang">>,Lang}

        ]}),
    {ok, JSON, [{resource,<<"HASH">>},{protocol,get}|Acc]}.

ans1(AnswerJSON, Acc) ->
    io:format("ANS1~n"),
    {ok,{Resp},_} = jsone:try_decode(unicode:characters_to_binary(AnswerJSON)),
    case proplists:get_value(<<"code">>, Resp) of
         <<"200">> -> {ok, proplists:get_value(<<"ref">>, Resp), Acc};
         BadCode -> {error, BadCode, Acc} end.

req2(Ref, Acc) ->
    io:format("REQ2~n"),
    {ok, <<"">>, [{resource, wf:to_list(Ref)}|proplists:delete(resource, Acc)]}.

ans2(AnswerJSON, Acc) ->
    io:format("ANS2~n"),
    {ok,{Resp},_} = jsone:try_decode(unicode:characters_to_binary(AnswerJSON)),
    case {proplists:get_value(<<"code">>, Resp), proplists:get_value(<<"status">>, Resp)} of
         {<<"200">>,<<"wait">>} -> {repeat, Acc};
         {<<"200">>,<<"ok">>} -> {ok, enter, Acc};
         _Other -> {error, _Other, Acc} end.

mock(1,[Method,Address,Headers,Type,Data]) ->
    Response =  <<"{\"code\":\"200\",\"ref\":\"151125154319VP02CLOY6M\"}">>,
    {ok,{{[],200,"OK"},[], Response }};

mock(2,Ref) ->
    Response =  <<"{\"code\":\"200\",\"status\":\"ok\",\"answer\":\"K1\",\"phone\":\"+380923058337\"}">>,
    {ok,{{[],200,"OK"},[], Response }}.
