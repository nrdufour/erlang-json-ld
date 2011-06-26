#! /usr/bin/env escript
%%! -pa ./test/ -pa ./ebin/
% This file is part of erlang-json-ld released under the MIT license.
% See the LICENSE file for more information.

main([]) ->
    test_util:run(length(modules()), fun() -> test() end).

modules() ->
    [jsonld_proc, uuid, jsonld_2ntriples].

test() ->
    lists:foreach(fun(Mod) ->
        Mesg = atom_to_list(Mod) ++ " module loaded",
        etap:loaded_ok(Mod, Mesg)
    end, modules()).
