
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

%
% JSON-LD Processor based on Bradley Allen work
%

-module(jsonld_context).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([create/0]).

create() ->
    Default = [
        {<<"rdf">>, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#">>},
        {<<"xsd">>, <<"http://www.w3.org/2001/XMLSchema#">>},
        {<<"dc">>, <<"http://purl.org/dc/terms/">>},
        {<<"skos">>, <<"http://www.w3.org/2004/02/skos/core#">>},
        {<<"foaf">>, <<"http://xmlns.com/foaf/0.1/">>},
        {<<"sioc">>, <<"http://rdfs.org/sioc/ns#">>},
        {<<"cc">>, <<"http://creativecommons.org/ns#">>},
        {<<"geo">>, <<"http://www.w3.org/2003/01/geo/wgs84_pos#">>},
        {<<"vcard">>, <<"http://www.w3.org/2006/vcard/ns#">>},
        {<<"cal">>, <<"http://www.w3.org/2002/12/cal/ical#">>},
        {<<"doap">>, <<"http://usefulinc.com/ns/doap#">>},
        {<<"Person">>, <<"http://xmlns.com/foaf/0.1/Person">>},
        {<<"name">>, <<"http://xmlns.com/foaf/0.1/name">>},
        {<<"homepage">>, <<"http://xmlns.com/foaf/0.1/homepage">>}
    ],
    InitialDict = dict:new(),
    lists:foldl(
        fun({Key, Value}, Dict) ->
            dict:store(Key, Value, Dict)
        end,
        InitialDict,
        Default).
