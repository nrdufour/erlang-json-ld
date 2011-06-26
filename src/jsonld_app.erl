
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

-module(jsonld_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    jsonld_sup:start_link().

stop(_State) ->
    ok.
