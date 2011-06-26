
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

-module(jsonld).

%% Application callbacks
-export([start/0, stop/0]).

%% API
-export([json_to_triples/1, triples_to_ntriples/1, json_to_ntriples/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(jsonld).

stop() ->
    application:stop(jsonld).

json_to_triples(Doc) ->
    jsonld_reader:json_to_triples(Doc).

triples_to_ntriples(Triples) ->
    jsonld_writer:triples_to_ntriples(Triples).

json_to_ntriples(Doc) ->
    Triples = jsonld_reader:json_to_triples(Doc),
    jsonld_writer:triples_to_ntriples(Triples).
