-module(data_string_unsafe@foreign).
-export([charAt/2, char/1]).

%% Formally, in Purescript a Char corresponds to a single UTF-16 code unit,
%% and a Char should not hold any code point above 0xFFFF. Note that since
%% we use UTF-8, we also get a runtime exception if the given index does
%% not point to a valid byte sequence.

charAt(I, S) ->
    <<_:I/binary,S1/binary>> = S,
    case string:next_codepoint(S1) of
        [C | _Rest] when C =< 16#FFFF -> C;
        _ -> error(badarg)
    end.

char(S) ->
    case string:next_codepoint(S) of
        [C | <<>>] when C =< 16#FFFF -> C;
        _ -> error(badarg)
    end.
