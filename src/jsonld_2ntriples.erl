
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

-module(jsonld_2ntriples).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([start/0]).

-include("jsonld.hrl").

-define(BNODE_PATTERN, "^_\\:\\w+$").

start() ->
    case init:get_plain_arguments() of
        [JsonFile] -> parse_json_to_ntriples(JsonFile);
        _ -> io:format("Wrong usage: jsonfile~n", [])
    end.

parse_json_to_ntriples(JsonFile) ->
    Json = readlines(JsonFile),
    Triples = jsonld_reader:json_to_triples(Json),
    lists:foreach(
        fun(Element) ->
            NTriple = triple_to_ntriple(Element),
            io:format("~s~n", [binary_to_list(NTriple)])
        end,
        Triples
    ),
    ok.

triple_to_ntriple(Triple) ->
    TripleSubject = Triple#triple.subject,
    TripleProperty = Triple#triple.property,
    TripleObject = Triple#triple.object,
    TripleType = Triple#triple.type,
    Start = <<"<">>,
    End = <<">">>,
    Space = <<" ">>,
    Quote = <<"\"">>,

    %% subject
    TripleSubject = Triple#triple.subject,
    Subject = case re:run(TripleSubject, ?BNODE_PATTERN) of
        {match, _} -> TripleSubject;
        _ -> <<Start/binary, TripleSubject/binary, End/binary>>
    end,

    %% property
    Property = <<Start/binary, TripleProperty/binary, End/binary>>,

    %% object
    Object = case re:run(TripleObject, ?BNODE_PATTERN) of
        {match, _} -> TripleObject;
        _ ->
            case TripleType of
                resource ->
                    <<Start/binary, TripleObject/binary, End/binary>>;
                literal ->
                    %% TODO missing support for lang and datatype
                    <<Quote/binary, TripleObject/binary, Quote/binary>>
            end
    end,

    %% N triple
    <<Subject/binary, Space/binary, Property/binary, Space/binary, Object/binary>>.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_all_lines(Device, Accum ++ [Line])
    end.
