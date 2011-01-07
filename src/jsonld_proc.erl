%% Copyright 2011 Nicolas R Dufour
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

%%
%% JSON-LD Processor based on Bradley Allen work
%%

-module(jsonld_proc).
-author("Nicolas R Dufour <nrdufour@gmail.com>").

-compile(export_all).

-include("jsonld.hrl").

%% Patterns
-define(IRI_PATTERN, "^<?(?<iri>(?<prefix>\\w+)\\:(?<iri_starter>/?)(/?)(?<name>[^>\\s]+))>?$").
-define(BNODE_PATTERN, "^_\\:\\w+$").
-define(CURIE_PATTERN, "^(?<prefix>\\w+)\\:(?<reference>\\w+)$").
-define(ABSOLUTE_PATTERN, "^(?<iri>(\\w+)\\:(/?)(/?)([^>\\s]+))$").
-define(WRAPPED_ABSOLUTE_IRI_PATTERN, "^<(?<iri>(\\w+)\:(/?)(/?)([^>\\s]+))>$").
-define(WRAPPED_RELATIVE_IRI_PATTERN, "^<(?<iri>[^\\:>\\s]+)>$").
-define(LANG_PATTERN, "^(?<literal>.+)@(?<lang>[a-zA-Z][a-zA-Z0-9\\-]+)$").
-define(TYPED_LITERAL_PATTERN, "^(?<literal>.+)\\^\\^(?<datatype>.+)$").
-define(DATETIME_PATTERN, "^(?<year>\\d\\d\\d\\d)([-])?(?<month>\\d\\d)([-])?(?<day>\\d\\d)((T|\\s+)(?<hour>\\d\\d)(([:])?(?<minute>\\d\\d)(([:])?(?<second>\\d\\d)(([.])?(?<fraction>\\d+))?)?)?)?((?<tzzulu>Z)|(?<tzoffset>[-+])(?<tzhour>\\d\\d)([:])?(?<tzminute>\\d\\d))?$").

-record(state, {
    context,
    subject,
    triples = []
}).

to_triples(Doc) ->
    DefaultContext = build_default_context(),
    JsonItem = mochijson2:decode(Doc),
    InitialState = #state{context = DefaultContext},
    FinalState = triples(JsonItem, InitialState),
    FinalState#state.triples.

%%
%% Internal API
%%

triples({struct, Props}, InitialState) ->
    ProcessingState = extract_processing_state(InitialState, Props),
    lists:foldl(
        fun({Key, Value}, State) ->
            ExtractedTriples = case Key of
                <<"#">> -> [];
                <<"@">> -> [];
                _ ->
                    Property = case Key of
                        <<"a">> -> <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>;
                        _ -> process_property(Key, State#state.context)
                    end,
                    case Value of
                        {struct, _V} ->
                            triples(Value, ProcessingState);
                        _ ->
                            [triple(State#state.subject, Property, Value, State#state.context)]
                    end
            end,
            NewTriples = lists:append(State#state.triples, ExtractedTriples),
            State#state{triples = NewTriples}
        end,
        ProcessingState,
        Props);

triples([H|T], InitialState) ->
    [triples(H, InitialState) | triples(T, InitialState)];

triples([], InitialState) ->
    InitialState#state.triples.

is_resource(_Subject, _Property, Object, ContextDict) ->
    dict:is_key(Object, ContextDict)
    or not(nomatch == re:run(Object, ?BNODE_PATTERN))
    or not(nomatch == re:run(Object, ?CURIE_PATTERN))
    or not(nomatch == re:run(Object, ?WRAPPED_ABSOLUTE_IRI_PATTERN))
    or not(nomatch == re:run(Object, ?WRAPPED_RELATIVE_IRI_PATTERN)).

triple(Subject, Property, Object, ContextDict) ->
    case is_resource(Subject, Property, Object, ContextDict) of
        true  -> resource_valued_triple(Subject, Property, Object, ContextDict);
        false -> literal_valued_triple(Subject, Property, Object, ContextDict)
    end.

resource_valued_triple(Subject, Property, Object, ContextDict) ->
    #triple{type = resource, subject = Subject, property = Property, object = resource(Object, ContextDict)}.

literal_valued_triple(Subject, Property, Object, ContextDict) ->
    #triple{type = literal, subject = Subject, property = Property, object = Object}.

resource(Object, ContextDict) ->
    WrappedAbsoluteIri = re:run(Object, ?WRAPPED_ABSOLUTE_IRI_PATTERN, [{capture, ['iri'], binary}]),
    WrappedRelativeIri = re:run(Object, ?WRAPPED_RELATIVE_IRI_PATTERN, [{capture, ['iri'], binary}]),
    Curie = re:run(Object, ?CURIE_PATTERN, [{capture, ['prefix', 'reference'], binary}]),
    BNode = re:run(Object, ?BNODE_PATTERN),
    case dict:is_key(Object, ContextDict) of
        true -> dict:fetch(Object, ContextDict);
        false ->
            case {BNode, Curie, WrappedAbsoluteIri, WrappedRelativeIri} of
                %% BNode
                {{match, _}, _, _, _} -> Object;
                %% Curie
                {_, {match, [Prefix, Reference]}, _, _} ->
                    case dict:is_key(Prefix, ContextDict) of
                        true -> 
                            PrefixNamespace = dict:fetch(Prefix, ContextDict),
                            <<PrefixNamespace/binary, Reference/binary>>;
                        false ->
                          case dict:is_key(Reference, ContextDict) of
                              true ->
                                  dict:fetch(Reference, ContextDict);
                              false ->
                                  throw({wrong_curie_resource, Object})
                          end
                    end;
                %% WrappedAbsoluteIri
                {_, _, {match, [IRI]}, _} ->
                    Base = case dict:is_key(<<"#base">>, ContextDict) of
                        true -> dict:fetch(<<"#base">>, ContextDict);
                        false -> <<"">>
                    end,
                    %% TODO need something for url parsing with #base rather than just concatenate it!
                    <<Base/binary, IRI/binary>>;
                %% WrappedRelativeIri
                {_, _, _, {match, [IRI]}} ->
                    case dict:is_key(<<"#base">>, ContextDict) of
                        true ->
                            Base = dict:fetch(<<"#base">>, ContextDict),
                            %% TODO need something for url parsing with #base rather than just concatenate it!
                            <<Base/binary, IRI/binary>>;
                        false ->
                            throw({wrong_relative_iri_resource, Object})
                    end;
                %% Everything else
                _ ->
                    throw({wrong_resource, Object})
            end
    end.

process_property(Key, ContextDict) ->
    case re:run(Key, ?IRI_PATTERN, [{capture, ['iri', 'prefix', 'iri_starter', 'name'], binary}]) of
        {match, [_IRI, _Prefix, <<"/">>, _Name]} -> Key;
        {match, [_IRI, <<"_">>, _IRI_Starter, _Name]} -> Key;
        {match, [IRI, Prefix, _IRI_Starter, Name]} ->
            case dict:is_key(Prefix, ContextDict) of
                true ->
                    URI = dict:fetch(Prefix, ContextDict),
                    <<URI/binary, Name/binary>>;
                false -> IRI
            end;
        _ ->
            case dict:is_key(Key, ContextDict) of
                true -> dict:fetch(Key, ContextDict);
                false ->
                    case dict:is_key(<<"#vocab">>, ContextDict) of
                        true ->
                            Vocab = dict:fetch(<<"#vocab">>, ContextDict),
                            <<Vocab/binary, Key/binary>>;
                        false -> throw({bad_property, Key})
                    end
            end
    end.

build_default_context() ->
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

extract_processing_state(InitialState, Props) ->
    lists:foldl(
        fun(Element, State) ->
            case Element of
                {<<"#">>, {struct, LocalContext}} ->
                    NewContext = merge_contexts(State#state.context, LocalContext),
                    State#state{context = NewContext};
                {<<"@">>, Subject} ->
                    case Subject of
                        {struct, _} ->
                            Triples = triples(Subject, State),
                            %% TODO need to set the subject with the "@" in the Subject structure
                            State#state{triples = [Triples|State#state.triples]};
                        List when is_list(List) ->
                            Triples = triples(List, State),
                            %% TODO need to generate the subject with a uuid here
                            State#state{triples = [Triples|State#state.triples]};
                        _ ->
                            State#state{subject = resource(Subject, State#state.context)}
                    end;
                _ -> State
            end
        end,
        InitialState,
        Props).

merge_contexts(InitialContext, LocalContext) ->
  lists:foldl(
      fun({Key, Value}, Dict) -> dict:store(Key, Value, Dict) end,
      InitialContext,
      LocalContext).
