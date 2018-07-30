-module(ais_time).

-export([extract_datetime/1, extract_time_info/1, construct_timeline/1]).

-export([secs_diff/2]).

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
                    Delta = bsr_time_delta(DateTime, BsrTime), 
                    NewRecs = map_pending_recs(BsrTime, Delta, [AisRec|PendingRecs]),
                    NewAcc = NewRecs ++ Acc,
                    {DateTime, [], NewAcc};
                undefined ->
                    {BsrTime, [AisRec|PendingRecs], Acc}
            end
        end,
    {_LastBsrTime, RemRecs, RevRecs} = lists:foldl(F, {undefined, [], []}, AisRecs),
    % Map the remaining records which we cannot fix in time.
    MappedRemRecs = map_pending_recs(undefined, undefined, RemRecs),
    FullList = MappedRemRecs ++ RevRecs,
    lists:reverse(FullList).

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

%% @doc Convert a list of pending records to a list of time-tagged tuples.
map_pending_recs(BsrTime, BsrDelta, PendingRecs) when 
        is_integer(BsrDelta),  BsrDelta > 0, BsrDelta < 60 ->
    F = fun(Rec) ->
            time_tagged_tuple(BsrTime, Rec)
        end,
    lists:filtermap(F, PendingRecs); 
map_pending_recs(_, _, PendingRecs) ->
    F = fun(Rec) ->
            time_tagged_tuple(undefined, Rec)
        end,
    lists:filtermap(F, PendingRecs).

%% Helper function to use with filtermap to build a list of time-tagged 
%% tuples, skipping over records that have not decoded properly.
time_tagged_tuple(undefined, {ok, AisRec}) ->
    {true, {undefined, AisRec}};
time_tagged_tuple(BsrTime, {ok, AisRec}) ->
    TimeTag = calc_time_tag(BsrTime, {ok, AisRec}),    
    {true, {TimeTag, AisRec}};
time_tagged_tuple(_, _) ->
    false.

calc_time_tag(BsrTime, AisRec) ->
    S1 = calendar:datetime_to_gregorian_seconds(BsrTime),
    case extract_datetime(AisRec) of
        {{_,_,_},{_,_,_}} = DateTime -> 
            DateTime;
        {seconds, Secs} ->
            {{_,_,_},{_,_,BsrSecs}} = BsrTime,
            SecsDiff = secs_diff(Secs, BsrSecs),
            S2 = S1 + SecsDiff,
            calendar:gregorian_seconds_to_datetime(S2);
        undefined ->
            undefined
    end.

%% Calculate the difference in seconds between 2 seconds fields (from a 
%% datetime, assuming that the datetimes are separated by less than one 
%% minute).
secs_diff(S1, S2) when S1 >= S2 ->
    S1 - S2;
secs_diff(S1, S2) ->
    60 + S1 - S2.

