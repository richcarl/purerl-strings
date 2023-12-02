-module(data_string_codePoints@foreign).
-export(['_unsafeCodePointAt0'/1
       , '_codePointAt'/6
       , '_fromCodePointArray'/2
       , '_singleton'/1
       , '_take'/1
       , '_toCodePointArray'/1]).

'_unsafeCodePointAt0'(_Fallback) -> fun (Str) ->
    case string:next_codepoint(Str) of
        [CP|_Rest] -> CP;
        _ -> error(badarg)  % malformed utf-8
    end
end.

'_codePointAt'(_Fallback, Just, Nothing, _unsafeCodePointAt0, Index, Str) ->
    if is_integer(Index), Index >= 0, Index < byte_size(Str) ->
            <<_:Index/binary,S/binary>> = Str,
            case string:next_codepoint(S) of
                [CP | _Rest] -> Just(CP);
                _ -> Nothing  % malformed utf-8
            end;
       true -> Nothing
    end.

'_fromCodePointArray'(_Fallback, Array) ->
  List = array:to_list(Array),
  unicode:characters_to_binary(List, utf8).

'_singleton'(_Fallback) -> fun (CP) ->
    unicode:characters_to_binary([CP], utf8)
end.

'_take'(_Fallback) ->
    fun (N) ->
        fun (S) -> take(N, S, <<>>) end
    end.

take(N, _S, Cs) when N =< 0 -> Cs;
take(_N, <<>>, _Cs) -> <<>>;  % trying to take too many yields empty string
take(N, S, Cs) ->
    %% note that right-appending to a binary in a loop is efficient
    case string:next_codepoint(S) of
        [CP | Rest] -> take(N-1, Rest, <<Cs/binary, CP/utf8>>);
        _ -> error(badarg)  % malformed utf-8
    end.

'_toCodePointArray'(_Fallback) -> fun (_UnsafeCodePointAt0) ->
    fun (Str) ->
        array:from_list(unicode:characters_to_list(Str, utf8))
    end
end.
