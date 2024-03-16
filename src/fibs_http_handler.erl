-module(fibs_http_handler).

-behaviour(cowboy_rest).

% cowboy REST callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2
]).

-export([
    handle_from_json/2
]).


init(Req, State) ->
    {cowboy_rest, Req, State}.


allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


content_types_provided(Req, State) ->
    {
        [
            {{<<"application">>, <<"json">>, []}, handle_from_json}
        ],
        Req,
        State
    }.


handle_from_json(Req, State) ->
    Cont = [1, 2],
    Result = #{
        <<"numbers">> => [1, 2, 3],
        <<"continuation">> => fibs_util:continuation_encode(Cont)
    },
    {fibs_util:to_json(Result), Req, State}.

