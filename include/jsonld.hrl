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

% Keywords
-define(LOCAL_CONTEXT_KEY, <<"@context">>).
-define(IRI_BASE_KEY, <<"@base">>).
-define(REMOTE_CONTEXT_KEY, <<"@profile">>).
-define(VOCAB_KEY, <<"@vocab">>).
-define(COERCE_KEY, <<"@coerce">>).
-define(LITERAL_KEY, <<"@literal">>).
-define(IRI_KEY, <<"@iri">>).
-define(LANGUAGE_KEY, <<"@language">>).
-define(DATATYPE_KEY, <<"@datatype">>).
-define(SUBJECT_KEY, <<"@subject">>).
-define(TYPE_KEY, <<"a">>).

-define(DEFAULT_CONTEXT,
[
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
]
).

-define(IS_OBJECT(Obj), jsonld_utils:is_proplist(Obj)).

-define(HAS_VALUE(Proplist, Key), lists:keyfind(Key, 1, Proplist)).

-record(triple, {
    type     :: resource | literal,
    subject  :: binary(),
    object   :: binary(),
    property :: binary()
}).

-type triple() :: #triple{}.
