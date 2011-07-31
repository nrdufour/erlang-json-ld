
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

-module(jsonld_reader).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([json_to_triples/1]).

-include("jsonld.hrl").

-record(state, {
    context,
    subject,
    triples = []
}).

json_to_triples(Doc) ->
    DefaultContext = build_default_context(),

    io:format("about to decode json ~p~n", [Doc]),

    JsonItem = jsx:json_to_term(Doc),

    io:format("Terms: ~p~n", [JsonItem]),

    InitialState = #state{context = DefaultContext},
    FinalState = triples(JsonItem, InitialState),
    FinalState#state.triples.

%
% Internal API
%

% Patterns
-define(IRI_PATTERN, "^<?(?<iri>(?<prefix>\\w+)\\:(?<iri_starter>/?)(/?)(?<name>[^>\\s]+))>?$").
-define(BNODE_PATTERN, "^_\\:\\w+$").
-define(CURIE_PATTERN, "^(?<prefix>\\w+)\\:(?<reference>\\w+)$").
-define(ABSOLUTE_PATTERN, "^(?<iri>(\\w+)\\:(/?)(/?)([^>\\s]+))$").
-define(WRAPPED_ABSOLUTE_IRI_PATTERN, "^<(?<iri>(\\w+)\:(/?)(/?)([^>\\s]+))>$").
-define(WRAPPED_RELATIVE_IRI_PATTERN, "^<(?<iri>[^\\:>\\s]+)>$").
-define(LANG_PATTERN, "^(?<literal>.+)@(?<lang>[a-zA-Z][a-zA-Z0-9\\-]+)$").
-define(TYPED_LITERAL_PATTERN, "^(?<literal>.+)\\^\\^(?<datatype>.+)$").
-define(DATETIME_PATTERN, "^(?<year>\\d\\d\\d\\d)([-])?(?<month>\\d\\d)([-])?(?<day>\\d\\d)((T|\\s+)(?<hour>\\d\\d)(([:])?(?<minute>\\d\\d)(([:])?(?<second>\\d\\d)(([.])?(?<fraction>\\d+))?)?)?)?((?<tzzulu>Z)|(?<tzoffset>[-+])(?<tzhour>\\d\\d)([:])?(?<tzminute>\\d\\d))?$").

process_local_context(JsonObject, InitialState) ->
    % Local Context: merge if exists
    LocalContextProp = ?HAS_VALUE(JsonObject, ?LOCAL_CONTEXT_KEY),
    case LocalContextProp of
        false -> InitialState;
        {_, Value} ->
            NewContext = merge_contexts(InitialState#state.context, Value),
            InitialState#state{context = NewContext}
    end.

process_subject(JsonObject, StateWithLocalContext) ->
    % Subject
    LocalSubjectProp = ?HAS_VALUE(JsonObject, ?SUBJECT_KEY),
    case LocalSubjectProp of
        {_, SubjectValue} -> process_subject_value(SubjectValue, StateWithLocalContext);
        false ->
            Uuid = uuid:to_string(uuid:v4()),
            CurrentSubject = list_to_binary(io_lib:format("_:~s", [Uuid])),
            StateWithLocalContext#state{ subject = CurrentSubject }
    end.

% The subject value can be an object, an array or a simple literal (to confirm here...)

process_subject_value(SubjectValue, StateWithLocalContext) when is_list(SubjectValue) ->
    case ?IS_OBJECT(SubjectValue) of
        % this is an object
        true  ->
            TriplesFromObject = triples(SubjectValue, StateWithLocalContext),
            CurrentSubject = ?HAS_VALUE(SubjectValue, ?SUBJECT_KEY),
            StateWithLocalContext#state{
             subject = CurrentSubject,
              triples = lists:append(TriplesFromObject, StateWithLocalContext#state.triples)
            };
        % this is an array
        false ->
            TriplesFromList = triples(SubjectValue, StateWithLocalContext),
            Uuid = uuid:to_string(uuid:v4()),
            CurrentSubject = list_to_binary(io_lib:format("_:~s", [Uuid])),
            StateWithLocalContext#state{
                subject = CurrentSubject,
                triples = lists:append(TriplesFromList, StateWithLocalContext#state.triples)
            }
    end;

% the subject value is neither an object or an array
process_subject_value(SubjectValue, StateWithLocalContext) ->
    CurrentSubject = process_resource(SubjectValue, StateWithLocalContext#state.context),
    StateWithLocalContext#state{ subject = CurrentSubject }.

% ---

process_other(JsonObject, StateWithSubject) ->
    lists:foldl(
        fun({Key, Value}, State) ->
            case Key of
                ?LOCAL_CONTEXT_KEY -> State;
                ?SUBJECT_KEY -> State;
                _ ->
                    Property = case Key of
                        ?TYPE_KEY -> <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>;
                        _ -> process_property(Key, State#state.context)
                    end,
                    case is_list(Value) of
                        true  ->
                            case ?IS_OBJECT(Value) of
                                true  ->
                                    NewState = triples(Value, State),
                                    LinkedTripleObject = case ?HAS_VALUE(Value, ?SUBJECT_KEY) of
                                        {_, SubjectValue} -> SubjectValue;
                                        false ->
                                            Uuid = uuid:to_string(uuid:v4()),
                                            list_to_binary(io_lib:format("_:~s", [Uuid]))
                                    end,
                                    LinkedTriple = triple(State#state.subject, Property, LinkedTripleObject, State#state.context),
                                    NewState#state{triples = lists:append(NewState#state.triples, [LinkedTriple])};
                                false ->
                                    % TODO Need to process array item here
                                    State
                            end;
                        false ->
                            Triple = triple(State#state.subject, Property, Value, State#state.context),
                            State#state{triples = lists:append(State#state.triples, [Triple])}
                    end
            end
        end,
        StateWithSubject,
        JsonObject).

triples(Item, InitialState) ->
    case ?IS_OBJECT(Item) of
        % it's an object
        true ->
            io:format("It's an object!~n", []),
            % Local Context: merge if exists
            StateWithLocalContext = process_local_context(Item, InitialState),
            % Subject
            StateWithSubject = process_subject(Item, StateWithLocalContext),
            % Everything else
            process_other(Item, StateWithSubject);
        % it's an array
        false ->
            io:format("It's an array!~n", []),
            AllTriples = lists:foldl(
                fun(Element, Acc) ->
                    CurrentState = triples(Element, InitialState),
                    CurrentTriples = CurrentState#state.triples,
                    Acc ++ CurrentTriples
                end,
                InitialState#state.triples,
                Item
            ),
            InitialState#state{triples = AllTriples}
    end.

is_resource(_Subject, _Property, Object, ContextDict) ->
    dict:is_key(Object, ContextDict)
    or not(nomatch == re:run(Object, ?BNODE_PATTERN))
    or not(nomatch == re:run(Object, ?CURIE_PATTERN))
    or not(nomatch == re:run(Object, ?WRAPPED_ABSOLUTE_IRI_PATTERN))
    or not(nomatch == re:run(Object, ?WRAPPED_RELATIVE_IRI_PATTERN)).

triple(Subject, Property, Object, ContextDict) ->
    case is_resource(Subject, Property, Object, ContextDict) of
        true  -> process_resource_valued_triple(Subject, Property, Object, ContextDict);
        false -> process_literal_valued_triple(Subject, Property, Object, ContextDict)
    end.

process_resource_valued_triple(Subject, Property, Object, ContextDict) ->
    #triple{type = resource, subject = Subject, property = Property, object = process_resource(Object, ContextDict)}.

process_literal_valued_triple(Subject, Property, Object, _ContextDict) ->
    #triple{type = literal, subject = Subject, property = Property, object = Object}.

process_resource(Object, ContextDict) ->
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
    InitialDict = dict:new(),
    Fun = fun({Key, Value}, Dict) -> dict:store(Key, Value, Dict) end,
    lists:foldl(Fun, InitialDict, ?DEFAULT_CONTEXT).

merge_contexts(InitialContext, LocalContext) ->
    Fun = fun({Key, Value}, Dict) -> dict:store(Key, Value, Dict) end,
    lists:foldl(Fun, InitialContext, LocalContext).
