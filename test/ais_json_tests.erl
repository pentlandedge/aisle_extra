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
    Sentences = sample_ais(),
    AisRecs = lists:map(fun aisle:decode/1, Sentences),
    Json = ais_json:tagged_ais_to_json(AisRecs),
    [?_assert(is_binary(Json))].

cnb_checks() ->
    [].

sample_ais() ->
    ["!AIVDM,1,1,,A,18IsCb0Oj1wl2L4P78w3B2q600Rr,0*4D",
     "!AIVDM,1,1,,B,18IsCb0Oj0wl2klP791SIRwN08Ki,0*08",
     "!AIVDM,1,1,,B,13M2?H02Aowlt20P84HrM`MV0<0O,0*4D",
     "!AIVDM,1,1,,A,18IsCb0Oj0wl35tP78w3NS1j0<0V,0*4F",
     "!AIVDM,1,1,,A,13M2?H021owlsiRP84ErN8L20L0O,0*43",
     "!AIVDM,1,1,,B,18IsCb0Oiwwl3I`P78r3SS6>060l,0*1B",
     "!AIVDM,1,1,,B,13M2?H02AowlsO>P84CJKpLF0D0O,0*75",
     "!AIVDM,1,1,,A,18IsCb0Oiwwl3a@P78m3`S:N0H9h,0*33",
     "!AIVDM,1,1,,A,13M2?H021nwls<jP84?rM`N`0<0N,0*2F",
     "!AIVDM,1,1,,B,18IsCb0Oiwwl3ttP78c3eS<j0<0V,0*7B",
     "!AIVDM,1,1,,B,13M2?H0vinwlrr8P84;:O8Nv060l,0*05",
     "!AIVDM,1,1,,A,18IsCb001wwl4@DP78NSjS?40<0V,0*43"].

