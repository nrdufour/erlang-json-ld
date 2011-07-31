
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

%
% JSON-LD Processor based on Bradley Allen work
%

-module(jsonld_context).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-include("jsonld.hrl").

-export([create_default/0, merge/2, has_namespace/2, get_namespace/2, get_base/1, get_vocab/1]).

-record(context, {
        namespaces,
        base = undefined,
        vocab = undefined,
        coerce
    }
).

create_default() ->
    DefaultContext = create_default_context(),
    DefaultCoerce = create_default_coerce(),
    #context{ namespaces = DefaultContext, coerce = DefaultCoerce }.

merge(Context, NewNamespaces) ->
    Fun = fun({Key, Value}, Ctx) ->
        case Key of
            ?VOCAB_KEY  -> Ctx#context{ vocab = Value };
            ?BASE_KEY   -> Ctx#context{ base  = Value };
            ?COERCE_KEY ->
                % TODO
                Ctx;
            _ ->
                UpdatedNamespaces = dict:store(Key, Value, Ctx#context.namespaces),
                Ctx#context{ namespaces = UpdatedNamespaces }
        end
    end,
    lists:foldl(Fun, Context, NewNamespaces).

has_namespace(Context, Prefix) ->
    dict:is_key(Prefix, Context#context.namespaces).

get_namespace(Context, Prefix) ->
    dict:fetch(Prefix, Context#context.namespaces).

get_base(Context) ->
    Context#context.base.

get_vocab(Context) ->
    Context#context.vocab.

%
% Internal API
%

create_default_context() ->
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

create_default_coerce() ->
    dict:new().
