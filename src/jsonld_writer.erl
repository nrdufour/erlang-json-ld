
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

-module(jsonld_writer).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([triples_to_ntriples/1]).

-include("jsonld.hrl").

triples_to_ntriples(Triples) ->
    Fun = fun(T) ->
        triple_to_ntriple(T)
    end,
    lists:map(Fun, Triples).

% INTERNAL API

-define(BNODE_PATTERN, "^_\\:\\w+$").

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
