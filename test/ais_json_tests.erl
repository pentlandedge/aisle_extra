%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2018 Pentland Edge Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations
%% under the License.
%%

-module(ais_json_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator function to run all the tests.
ais_json_test_() ->
    [ais_checks(), cnb_checks()].

ais_checks() ->
    Json = ais_json:ais_to_json(dummy),
    [?_assertEqual(ok, Json)].

cnb_checks() ->
    [].
