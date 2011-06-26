-module(jsonld_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    jsonld_sup:start_link().

stop(_State) ->
    ok.

json_to_triples(Doc) ->
    json_reader:json_to_triples(Doc).

