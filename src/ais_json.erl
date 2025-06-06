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

-module(ais_json).

-export([tagged_ais_to_json/1, tagged_ais_to_json2/1, mmsi_cnb_map_to_json/1]).

%% Function to convert a list of decoded AIS records to JSON. Consumes the 
%% tagged form e.g. {ok, #ais{}} and steps over any sentences that 
%% failed to decode.
tagged_ais_to_json(AisRecs) ->
    AisCnbList = aisle:extract_cnb_records(AisRecs),
    ais_cnb_to_geojson(AisCnbList).

tagged_ais_to_json2(AisRecs) ->
    GoodRecs = extract_valid_recs(AisRecs),
    ais_to_geojson(GoodRecs).

extract_valid_recs(AisRecs) ->
    F = fun(Rec, Acc) ->
            case tagged_ais_valid(Rec) of
                true ->
                    {ok, Ais} = Rec,
                    [Ais|Acc];
                false ->
                    Acc
            end
        end,
    Rev = lists:foldl(F, [], AisRecs),
    lists:reverse(Rev).

tagged_ais_valid({ok, A}) ->
    aisle:is_ais_rec(A);
tagged_ais_valid(_)            -> false.

%% Convert a map containing MMSI => CNB pairs to GeoJSON.
mmsi_cnb_map_to_json(CnbMap) when is_map(CnbMap) ->
    CnbList = maps:values(CnbMap),
    cnb_to_geojson(CnbList).

%% Function to convert a list of AIS records containing CNB records to GeoJSON. 
ais_cnb_to_geojson(AisCnbList) when is_list(AisCnbList)  ->
    PrepList = lists:map(fun ais_cnb_prep/1, AisCnbList),
    jsx:encode([{<<"data">>, PrepList}]).

%% Function to convert a list of AIS records containing CNB records to GeoJSON. 
cnb_to_geojson(CnbList) when is_list(CnbList)  ->
    FeatureList = lists:map(fun cnb_prep/1, CnbList),
    jsx:encode([{<<"type">>,<<"FeatureCollection">>},
                {<<"features">>, FeatureList}]).

%% Collect the relevant data into a structure suitable for encoding using
%% the jsx library.
ais_cnb_prep(AISrec) -> 
    CNB = aisle:get_data(AISrec),
    cnb_prep(CNB).

%% Convert the CNB record to a form suitable for JSON conversion.
cnb_prep(CNB) ->
    Timestamp = aisle:get_timestamp(CNB),
    Lat = aisle:get_latitude(CNB),
    Lon = aisle:get_longitude(CNB),
    MMSI = aisle:get_mmsi(CNB),
    gen_cnb_geojson(Timestamp, Lat, Lon, MMSI).

%% Generate the GeoJSON for a CNB record.
gen_cnb_geojson(TimeUtc, Lat, Lon, MMSI) ->
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"timestamp">>, TimeUtc},
                         {<<"type">>, <<"CNB">>},
                         {<<"mmsi">>, MMSI}]},
     {<<"geometry">>, [{<<"type">>, <<"Point">>},
                       {<<"coordinates">>, [Lon, Lat]}]}].

ais_bsr_prep(AisRec) ->
    BSR = aisle:get_data(AisRec),
    bsr_prep(BSR).

bsr_prep(BSR) ->
    DateTime = aisle:get_bsr_datetime(BSR), 
    MMSI = aisle:get_bsr_mmsi(BSR), 
    RI = aisle:repeat_indicator_to_list(aisle:get_bsr_repeat_indicator(BSR)),
    Lat = aisle:get_bsr_latitude(BSR),
    Lon = aisle:get_bsr_longitude(BSR),
    FixType = aisle:epfd_fix_type_to_list(aisle:get_bsr_type_of_epfd(BSR)),
    Raim = aisle:raim_to_list(aisle:get_bsr_raim_flag(BSR)),
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"timestamp">>, datetime_to_timestamp(DateTime)},
                         {<<"type">>, <<"BSR">>},
                         {<<"repeat_indicator">>,  RI},
                         {<<"mmsi">>, MMSI},
                         {<<"epfd_fix_type">>, FixType},
                         {<<"raim_flag">>, Raim}]},
     {<<"geometry">>, [{<<"type">>, <<"Point">>},
                       {<<"coordinates">>, [Lon, Lat]}]}].

datetime_to_timestamp({{Y,M,D},{H,M,S}}) ->
    lists:flatten(io_lib:format("~p-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, M, S])).

ais_to_feature(AisRec) ->
    case aisle:get_msg_id(AisRec) of
        1 ->
            ais_cnb_prep(AisRec);
        2 ->
            ais_cnb_prep(AisRec);
        3 ->
            ais_cnb_prep(AisRec);
        4 ->
            ais_bsr_prep(AisRec);
        _ ->
            [{<<"type">>, <<"Feature">>},
             {<<"properties">>, []},
             {<<"geometry">>, []}]
    end.


ais_to_geojson(AisRecs) ->
    FeatureList = lists:map(fun ais_to_feature/1, AisRecs),
    jsx:encode([{<<"type">>,<<"FeatureCollection">>},
                {<<"features">>, FeatureList}]).
