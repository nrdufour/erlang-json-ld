
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

-module(jsonld_2ntriples).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([start/0]).

-include("jsonld.hrl").

start() ->
    case init:get_plain_arguments() of
        [JsonFile] -> parse_json_to_ntriples(JsonFile);
        _ -> io:format("Wrong usage: jsonfile~n", [])
    end.

parse_json_to_ntriples(JsonFile) ->
    Json = readlines(JsonFile),
    NTriples = jsonld:json_to_ntriples(Json),
    lists:foreach(
        fun(NTriple) ->
            io:format("~s~n", [binary_to_list(NTriple)])
        end,
        NTriples
    ),
    ok.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_all_lines(Device, Accum ++ [Line])
    end.
