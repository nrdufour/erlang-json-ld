% Copyright 2011 Nicolas R Dufour
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(jsonld_2ntriples).
-author("Nicolas R Dufour <nrdufour@gmail.com>").

-export([start/0]).


start() ->
    case init:get_plain_arguments() of
        [JsonFile] -> parse_json_to_ntriples(JsonFile);
        _ -> io:format("Wrong usage: jsonfile~n", [])
    end.

parse_json_to_ntriples(JsonFile) ->
    Json = readlines(JsonFile),
    Triples = jsonld_proc:to_triples(Json),
    io:format("JSON: ~p~n", [Json]),
    io:format("Triples: ~p~n", [Triples]),
    ok.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_all_lines(Device, Accum ++ [Line])
    end.

