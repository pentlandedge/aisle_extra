-module(ais_time).

-export([extract_datetime/1, extract_time_info/1, construct_timeline/1]).

%% Function to extract a datetime from an AIS record if one is available.
extract_datetime({ok, A} = AisRec) ->
    MessageType = aisle:get_payload_type(AisRec),
    Data = aisle:get_data(A), 
    case MessageType of
        cnb     -> 
            {seconds, aisle:get_timestamp(Data)};  
        base_sr ->
            aisle:get_bsr_datetime(Data);
        _       -> 
            undefined
    end.

extract_time_info(AisRecs) when is_list(AisRecs) ->
    F = fun(AIS, Acc) ->
            case extract_datetime(AIS) of
                {{_,_,_},{_,_,_}} = DateTime -> 
                    [DateTime|Acc];
                _ -> 
                    Acc
            end
        end,
    RevTime = lists:foldl(F, [], AisRecs),
    lists:reverse(RevTime).

%% @doc Maps a list of tagged AisRecs to a list of tuples {TimeStamp, AisRec} 
%% where it is possible to work out a UTC time for the record. This requires 
%% base station reports to be interspersed with the CNB data. This is 
%% generally most useful for recovering a timeline from recorded data: live 
%% data can be timestamped as it arrives. Where a record cannot be 
%% unambiguously placed on a timeline, the atom undefined is set.
%% The code assumes all sentences are in chronological order.
%% Only sections of data enclosed within base station reports separated by 
%% less than one minute can be unambiguously placed on a timeline. 
construct_timeline(AisRecs) when is_list(AisRecs) ->
    F = fun(AisRec, {BsrTime, PendingRecs, Acc}) ->
            case extract_bsrtime(AisRec) of
                {ok, DateTime} -> 
                    io:format("DateTime ~p~n", [DateTime]),
                    Diff = bsr_time_delta(DateTime, BsrTime), 
                    io:format("Diff ~p~n", [Diff]),
                    {DateTime, [AisRec|PendingRecs], Acc};
                undefined ->
                    {BsrTime, [AisRec|PendingRecs], Acc}
            end
        end,
    % This ignores the last set of records which have no BSR.
    {_LastBsrTime, _RemRecs, RevRecs} = lists:foldl(F, {undefined, [], []}, AisRecs),
    lists:reverse(RevRecs).

%% @doc Convenience wrapper to fetch a base station report time. Returns 
%% undefined if it is not a BSR.
extract_bsrtime(AisRec) ->
    case extract_datetime(AisRec) of
        {{_,_,_},{_,_,_}} = DateTime -> 
            {ok, DateTime};
        _ -> 
            undefined 
    end.

bsr_time_delta(undefined, _) -> 
    undefined;
bsr_time_delta(_, undefined) -> 
    undefined;
bsr_time_delta(DT1, DT2) -> 
    S1 = calendar:datetime_to_gregorian_seconds(DT1),
    S2 = calendar:datetime_to_gregorian_seconds(DT2),
    S1 - S2.


