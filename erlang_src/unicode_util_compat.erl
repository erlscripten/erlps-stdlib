%% THIS IS VERY VERY VERY SIMPLIFIED BUT SHOULD WORK FINE FOR ASCII

-module(unicode_util_compat).
-export([cp/1, gc/1]).
-export([whitespace/0, is_whitespace/1]).
-export([uppercase/1, lowercase/1, titlecase/1, casefold/1]).

-inline([class/1]).
-compile(nowarn_unused_vars).
-type gc() :: char()|[char()].

-spec uppercase(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).
uppercase(Str0) ->
    case cp(Str0) of
        [CP|Str] = Str1 ->
            case case_table(CP) of
                {Upper,_} -> [Upper|Str];
                {Upper,_,_,_} -> [Upper|Str]
            end;
        [] -> [];
        {error,Err} -> error({badarg, Err})
    end.

-spec lowercase(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).
lowercase(Str0) ->
    case cp(Str0) of
        [CP|Str] = Str1 ->
            case case_table(CP) of
                {_,Lower} -> [Lower|Str];
                {_,Lower,_,_} -> [Lower|Str]
            end;
        [] -> [];
        {error,Err} -> error({badarg, Err})
    end.

-spec titlecase(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).
titlecase(Str0) ->
    case cp(Str0) of
        [CP|Str] = Str1 ->
            case case_table(CP) of
                {_,_,Title,_} -> [Title|Str];
                {Upper,_} -> [Upper|Str]
            end;
        [] -> [];
        {error,Err} -> error({badarg, Err})
    end.

-spec casefold(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).
casefold(Str0) ->
    case cp(Str0) of
        [CP|Str] = Str1 ->
            case case_table(CP) of
                {_,_,_,Fold} -> [Fold|Str];
                {_,Lower} -> [Lower|Str]
            end;
        [] -> [];
        {error,Err} -> error({badarg, Err})
    end.

%% Useful non-breakable whitespace chars
%% defined as Pattern White Space in Unicode Standard Annex #31
-spec whitespace() -> [gc()].
whitespace() -> [[13,10],9,10,11,12,13,32,133,8206,8207,8232,8233].

-spec is_whitespace(gc()) -> boolean().
is_whitespace([13,10]) -> true;
is_whitespace(9) -> true;
is_whitespace(10) -> true;
is_whitespace(11) -> true;
is_whitespace(12) -> true;
is_whitespace(13) -> true;
is_whitespace(32) -> true;
is_whitespace(133) -> true;
is_whitespace(8206) -> true;
is_whitespace(8207) -> true;
is_whitespace(8232) -> true;
is_whitespace(8233) -> true;
is_whitespace(_) -> false.

-spec cp(String::unicode:chardata()) -> maybe_improper_list() | {error, unicode:chardata()}.
cp([C|_]=L) when is_integer(C) -> L;
cp([List]) -> cp(List);
cp([List|R]) -> cpl(List, R);
cp([]) -> [];
cp(<<C/utf8, R/binary>>) -> [C|R];
cp(<<>>) -> [];
cp(<<R/binary>>) -> {error,R}.

cpl([C], R) when is_integer(C) -> [C|cpl_1_cont(R)];
cpl([C|T], R) when is_integer(C) -> [C|cpl_cont(T, R)];
cpl([List], R) -> cpl(List, R);
cpl([List|T], R) -> cpl(List, [T|R]);
cpl([], R) -> cp(R);
cpl(<<C/utf8, T/binary>>, R) -> [C,T|R];
cpl(<<>>, R) -> cp(R);
cpl(<<B/binary>>, R) -> {error,[B|R]}.

%%%

cpl_cont([C|T], R) when is_integer(C) -> [C|cpl_cont2(T, R)];
cpl_cont([L], R) -> cpl_cont(L, R);
cpl_cont([L|T], R) -> cpl_cont(L, [T|R]);
cpl_cont([], R) -> cpl_1_cont(R);
cpl_cont(T, R) -> [T|R].

cpl_cont2([C|T], R) when is_integer(C) -> [C|cpl_cont3(T, R)];
cpl_cont2([L], R) -> cpl_cont2(L, R);
cpl_cont2([L|T], R) -> cpl_cont2(L, [T|R]);
cpl_cont2([], R) -> cpl_1_cont2(R);
cpl_cont2(T, R) -> [T|R].

cpl_cont3([C], R) when is_integer(C) -> [C|R];
cpl_cont3([C|T], R) when is_integer(C) -> [C,T|R];
cpl_cont3([L], R) -> cpl_cont3(L, R);
cpl_cont3([L|T], R) -> cpl_cont3(L, [T|R]);
cpl_cont3([], R) -> cpl_1_cont3(R);
cpl_cont3(T, R) -> [T|R].

%%%

cpl_1_cont([C|T]) when is_integer(C) -> [C|cpl_1_cont2(T)];
cpl_1_cont([L]) -> cpl_1_cont(L);
cpl_1_cont([L|T]) -> cpl_cont(L, T);
cpl_1_cont(T) -> T.

cpl_1_cont2([C|T]) when is_integer(C) -> [C|cpl_1_cont3(T)];
cpl_1_cont2([L]) -> cpl_1_cont2(L);
cpl_1_cont2([L|T]) -> cpl_cont2(L, T);
cpl_1_cont2(T) -> T.

cpl_1_cont3([C|_]=T) when is_integer(C) -> T;
cpl_1_cont3([L]) -> cpl_1_cont3(L);
cpl_1_cont3([L|T]) -> cpl_cont3(L, T);
cpl_1_cont3(T) -> T.

%%%

cp_no_bin([C|_]=L) when is_integer(C) -> L;
cp_no_bin([List]) -> cp_no_bin(List);
cp_no_bin([List|R]) -> cp_no_binl(List, R);
cp_no_bin([]) -> [];
cp_no_bin(_) -> binary_found.

cp_no_binl([C], R) when is_integer(C) -> [C|cpl_1_cont(R)];
cp_no_binl([C|T], R) when is_integer(C) -> [C|cpl_cont(T, R)];
cp_no_binl([List], R) -> cp_no_binl(List, R);
cp_no_binl([List|T], R) -> cp_no_binl(List, [T|R]);
cp_no_binl([], R) -> cp_no_bin(R);
cp_no_binl(_, _) -> binary_found.

-spec gc(String::unicode:chardata()) -> maybe_improper_list() | {error, unicode:chardata()}.
gc([]=R) -> R;
gc([CP]=R) when is_integer(CP) -> R;
gc([$\r=CP|R0]) ->
    case cp(R0) of % Don't break CRLF
        [$\n|R1] -> [[$\r,$\n]|R1];
        T -> [CP|T]
    end;
gc([CP1|T1]=T) when CP1 < 256 ->
    case T1 of
        [CP2|_] when CP2 < 256 -> T; %% Ascii Fast path
        _ -> %% Keep the tail binary.
            case cp_no_bin(T1) of
                [CP2|_]=T3 when CP2 < 256 -> [CP1|T3]; %% Asciii Fast path
                binary_found -> gc_1(T);
                T4 -> gc_1([CP1|T4])
            end
    end;
gc(<<>>) -> [];
gc(<<CP1/utf8, Rest/binary>>) ->
    if CP1 < 256, CP1 =/= $\r ->
           case Rest of
               <<CP2/utf8, _/binary>> when CP2 < 256 -> %% Ascii Fast path
                   [CP1|Rest];
               _ -> gc_1([CP1|Rest])
           end;
      true -> gc_1([CP1|Rest])
    end;
gc([CP|_]=T) when is_integer(CP) -> gc_1(T);
gc(Str) ->
    case cp(Str) of
        {error,_}=Error -> Error;
        CPs -> gc(CPs)
    end.

gc_1([$\r|R0] = R) ->
    case cp(R0) of % Don't break CRLF
        [$\n|R1] -> [[$\r,$\n]|R1];
        _ -> R
    end;

%% Handle control
gc_1([0=CP|R1]=R0) -> R0;
gc_1([1=CP|R1]=R0) -> R0;
gc_1([2=CP|R1]=R0) -> R0;
gc_1([3=CP|R1]=R0) -> R0;
gc_1([4=CP|R1]=R0) -> R0;
gc_1([5=CP|R1]=R0) -> R0;
gc_1([6=CP|R1]=R0) -> R0;
gc_1([7=CP|R1]=R0) -> R0;
gc_1([8=CP|R1]=R0) -> R0;
gc_1([9=CP|R1]=R0) -> R0;
gc_1([10=CP|R1]=R0) -> R0;
gc_1([11=CP|R1]=R0) -> R0;
gc_1([12=CP|R1]=R0) -> R0;
gc_1([14=CP|R1]=R0) -> R0;
gc_1([15=CP|R1]=R0) -> R0;
gc_1([16=CP|R1]=R0) -> R0;
gc_1([17=CP|R1]=R0) -> R0;
gc_1([18=CP|R1]=R0) -> R0;
gc_1([19=CP|R1]=R0) -> R0;
gc_1([20=CP|R1]=R0) -> R0;
gc_1([21=CP|R1]=R0) -> R0;
gc_1([22=CP|R1]=R0) -> R0;
gc_1([23=CP|R1]=R0) -> R0;
gc_1([24=CP|R1]=R0) -> R0;
gc_1([25=CP|R1]=R0) -> R0;
gc_1([26=CP|R1]=R0) -> R0;
gc_1([27=CP|R1]=R0) -> R0;
gc_1([28=CP|R1]=R0) -> R0;
gc_1([29=CP|R1]=R0) -> R0;
gc_1([30=CP|R1]=R0) -> R0;
gc_1([31=CP|R1]=R0) -> R0;
gc_1([127=CP|R1]=R0) -> R0;
gc_1([128=CP|R1]=R0) -> R0;
gc_1([129=CP|R1]=R0) -> R0;
gc_1([130=CP|R1]=R0) -> R0;
gc_1([131=CP|R1]=R0) -> R0;
gc_1([132=CP|R1]=R0) -> R0;
gc_1([133=CP|R1]=R0) -> R0;
gc_1([134=CP|R1]=R0) -> R0;
gc_1([135=CP|R1]=R0) -> R0;
gc_1([136=CP|R1]=R0) -> R0;
gc_1([137=CP|R1]=R0) -> R0;
gc_1([138=CP|R1]=R0) -> R0;
gc_1([139=CP|R1]=R0) -> R0;
gc_1([140=CP|R1]=R0) -> R0;
gc_1([141=CP|R1]=R0) -> R0;
gc_1([142=CP|R1]=R0) -> R0;
gc_1([143=CP|R1]=R0) -> R0;
gc_1([144=CP|R1]=R0) -> R0;
gc_1([145=CP|R1]=R0) -> R0;
gc_1([146=CP|R1]=R0) -> R0;
gc_1([147=CP|R1]=R0) -> R0;
gc_1([148=CP|R1]=R0) -> R0;
gc_1([149=CP|R1]=R0) -> R0;
gc_1([150=CP|R1]=R0) -> R0;
gc_1([151=CP|R1]=R0) -> R0;
gc_1([152=CP|R1]=R0) -> R0;
gc_1([153=CP|R1]=R0) -> R0;
gc_1([154=CP|R1]=R0) -> R0;
gc_1([155=CP|R1]=R0) -> R0;
gc_1([156=CP|R1]=R0) -> R0;
gc_1([157=CP|R1]=R0) -> R0;
gc_1([158=CP|R1]=R0) -> R0;
gc_1([159=CP|R1]=R0) -> R0;
gc_1([173=CP|R1]=R0) -> R0;

%% Optimize Latin-1
gc_1([169=CP|R1]=R0) -> gc_ext_pict(R1,[CP]);
gc_1([174=CP|R1]=R0) -> gc_ext_pict(R1,[CP]);
gc_1([CP|R]=R0) when CP < 256 ->
    case R of
        [CP2|_] when CP2 < 256 -> R0;
        _ -> gc_extend(cp(R), R, CP)
    end;

%% Continue control
gc_1([1564=CP|R1]=R0) -> R0;
gc_1([6158=CP|R1]=R0) -> R0;
gc_1([8203=CP|R1]=R0) -> R0;
gc_1([CP|R1]=R0) when 8206 =< CP, CP =< 8207 -> R0;
gc_1([CP|R1]=R0) when 8232 =< CP, CP =< 8238 -> R0;
gc_1([CP|R1]=R0) when 8288 =< CP, CP =< 8303 -> R0;
gc_1([65279=CP|R1]=R0) -> R0;
gc_1([CP|R1]=R0) when 65520 =< CP, CP =< 65531 -> R0;
gc_1([CP|R1]=R0) when 78896 =< CP, CP =< 78904 -> R0;
gc_1([CP|R1]=R0) when 113824 =< CP, CP =< 113827 -> R0;
gc_1([CP|R1]=R0) when 119155 =< CP, CP =< 119162 -> R0;
gc_1([CP|R1]=R0) when 917504 =< CP, CP =< 917535 -> R0;
gc_1([CP|R1]=R0) when 917632 =< CP, CP =< 917759 -> R0;
gc_1([CP|R1]=R0) when 918000 =< CP, CP =< 921599 -> R0.

%% Handle Extend
%% To simplify binary handling in libraries the tail should be kept binary
%% and not a lookahead CP
gc_extend([CP|T], T0, CP0) ->
    case false of
        false -> [CP0|T0]; % losing work done on T
        _TrueOrZWJ -> gc_extend2(cp(T), T, [CP,CP0])
    end;
gc_extend([], _, CP) -> [CP];
gc_extend({error,R}, _, CP) -> [CP|R].

gc_extend2([CP|T], T0, Acc) ->
    case false of
        false -> [lists:reverse(Acc)|T0]; % losing work done on T
        _TrueOrZWJ -> gc_extend2(cp(T), T, [CP|Acc])
    end;
gc_extend2([], _, Acc) ->
    [lists:reverse(Acc)];
gc_extend2({error,R}, _, Acc) ->
    [lists:reverse(Acc)] ++ [R].

gc_ext_pict(T, Acc) ->
    gc_ext_pict(cp(T), T, Acc).

gc_ext_pict([CP|R1], T0, Acc) ->
    case false of
        zwj -> gc_ext_pict_zwj(cp(R1), R1, [CP|Acc]);
        true -> gc_ext_pict(R1, [CP|Acc]);
        false ->
            case Acc of
                [A] -> [A|T0];
                _ -> [lists:reverse(Acc)|T0]
            end
    end;
gc_ext_pict([], _T0, Acc) ->
    case Acc of
        [A] -> [A];
        _ -> [lists:reverse(Acc)]
    end;
gc_ext_pict({error,R}, T, Acc) ->
    gc_ext_pict([], T, Acc) ++ [R].

gc_ext_pict_zwj([CP|R1], T0, Acc) ->
    case is_ext_pict(CP) of
        true -> gc_ext_pict(R1, [CP|Acc]);
        false ->
            case Acc of
                [A] -> [A|T0];
                _ -> [lists:reverse(Acc)|T0]
            end
    end;
gc_ext_pict_zwj([], _, Acc) ->
    case Acc of
        [A] -> [A];
        _ -> [lists:reverse(Acc)]
    end;
gc_ext_pict_zwj({error,R}, T, Acc) ->
    gc_ext_pict_zwj([], T, Acc) ++ [R].

is_ext_pict(169) -> true;
is_ext_pict(174) -> true;
is_ext_pict(8252) -> true;
is_ext_pict(8265) -> true;
is_ext_pict(8482) -> true;
is_ext_pict(8505) -> true;
is_ext_pict(9000) -> true;
is_ext_pict(9096) -> true;
is_ext_pict(9167) -> true;
is_ext_pict(9410) -> true;
is_ext_pict(9654) -> true;
is_ext_pict(9664) -> true;
is_ext_pict(10004) -> true;
is_ext_pict(10006) -> true;
is_ext_pict(10013) -> true;
is_ext_pict(10017) -> true;
is_ext_pict(10024) -> true;
is_ext_pict(10052) -> true;
is_ext_pict(10055) -> true;
is_ext_pict(10060) -> true;
is_ext_pict(10062) -> true;
is_ext_pict(10071) -> true;
is_ext_pict(10145) -> true;
is_ext_pict(10160) -> true;
is_ext_pict(10175) -> true;
is_ext_pict(11088) -> true;
is_ext_pict(11093) -> true;
is_ext_pict(12336) -> true;
is_ext_pict(12349) -> true;
is_ext_pict(12951) -> true;
is_ext_pict(12953) -> true;
is_ext_pict(127279) -> true;
is_ext_pict(127374) -> true;
is_ext_pict(127514) -> true;
is_ext_pict(127535) -> true;
is_ext_pict(CP) when 127340 =< CP, CP =< 127345 -> true;
is_ext_pict(CP) when 9872 =< CP, CP =< 9989 -> true;
is_ext_pict(CP) when 9642 =< CP, CP =< 9643 -> true;
is_ext_pict(CP) when 8986 =< CP, CP =< 8987 -> true;
is_ext_pict(CP) when 8596 =< CP, CP =< 8601 -> true;
is_ext_pict(CP) when 8617 =< CP, CP =< 8618 -> true;
is_ext_pict(CP) when 9193 =< CP, CP =< 9203 -> true;
is_ext_pict(CP) when 9208 =< CP, CP =< 9210 -> true;
is_ext_pict(CP) when 9735 =< CP, CP =< 9746 -> true;
is_ext_pict(CP) when 9723 =< CP, CP =< 9726 -> true;
is_ext_pict(CP) when 9728 =< CP, CP =< 9733 -> true;
is_ext_pict(CP) when 9748 =< CP, CP =< 9861 -> true;
is_ext_pict(CP) when 10548 =< CP, CP =< 10549 -> true;
is_ext_pict(CP) when 10067 =< CP, CP =< 10069 -> true;
is_ext_pict(CP) when 9992 =< CP, CP =< 10002 -> true;
is_ext_pict(CP) when 10035 =< CP, CP =< 10036 -> true;
is_ext_pict(CP) when 10083 =< CP, CP =< 10087 -> true;
is_ext_pict(CP) when 10133 =< CP, CP =< 10135 -> true;
is_ext_pict(CP) when 126976 =< CP, CP =< 127231 -> true;
is_ext_pict(CP) when 11013 =< CP, CP =< 11015 -> true;
is_ext_pict(CP) when 11035 =< CP, CP =< 11036 -> true;
is_ext_pict(CP) when 127245 =< CP, CP =< 127247 -> true;
is_ext_pict(CP) when 128884 =< CP, CP =< 128895 -> true;
is_ext_pict(CP) when 127548 =< CP, CP =< 127551 -> true;
is_ext_pict(CP) when 127405 =< CP, CP =< 127461 -> true;
is_ext_pict(CP) when 127358 =< CP, CP =< 127359 -> true;
is_ext_pict(CP) when 127377 =< CP, CP =< 127386 -> true;
is_ext_pict(CP) when 127489 =< CP, CP =< 127503 -> true;
is_ext_pict(CP) when 127538 =< CP, CP =< 127546 -> true;
is_ext_pict(CP) when 128326 =< CP, CP =< 128591 -> true;
is_ext_pict(CP) when 127561 =< CP, CP =< 127994 -> true;
is_ext_pict(CP) when 128000 =< CP, CP =< 128317 -> true;
is_ext_pict(CP) when 128640 =< CP, CP =< 128767 -> true;
is_ext_pict(CP) when 129198 =< CP, CP =< 129279 -> true;
is_ext_pict(CP) when 129096 =< CP, CP =< 129103 -> true;
is_ext_pict(CP) when 128981 =< CP, CP =< 129023 -> true;
is_ext_pict(CP) when 129036 =< CP, CP =< 129039 -> true;
is_ext_pict(CP) when 129114 =< CP, CP =< 129119 -> true;
is_ext_pict(CP) when 129160 =< CP, CP =< 129167 -> true;
is_ext_pict(CP) when 129351 =< CP, CP =< 129791 -> true;
is_ext_pict(CP) when 129292 =< CP, CP =< 129338 -> true;
is_ext_pict(CP) when 129340 =< CP, CP =< 129349 -> true;
is_ext_pict(CP) when 130048 =< CP, CP =< 131069 -> true;
is_ext_pict(_) -> false.

case_table(65) -> {65,97};
case_table(66) -> {66,98};
case_table(67) -> {67,99};
case_table(68) -> {68,100};
case_table(69) -> {69,101};
case_table(70) -> {70,102};
case_table(71) -> {71,103};
case_table(72) -> {72,104};
case_table(73) -> {73,105};
case_table(74) -> {74,106};
case_table(75) -> {75,107};
case_table(76) -> {76,108};
case_table(77) -> {77,109};
case_table(78) -> {78,110};
case_table(79) -> {79,111};
case_table(80) -> {80,112};
case_table(81) -> {81,113};
case_table(82) -> {82,114};
case_table(83) -> {83,115};
case_table(84) -> {84,116};
case_table(85) -> {85,117};
case_table(86) -> {86,118};
case_table(87) -> {87,119};
case_table(88) -> {88,120};
case_table(89) -> {89,121};
case_table(90) -> {90,122};
case_table(97) -> {65,97};
case_table(98) -> {66,98};
case_table(99) -> {67,99};
case_table(100) -> {68,100};
case_table(101) -> {69,101};
case_table(102) -> {70,102};
case_table(103) -> {71,103};
case_table(104) -> {72,104};
case_table(105) -> {73,105};
case_table(106) -> {74,106};
case_table(107) -> {75,107};
case_table(108) -> {76,108};
case_table(109) -> {77,109};
case_table(110) -> {78,110};
case_table(111) -> {79,111};
case_table(112) -> {80,112};
case_table(113) -> {81,113};
case_table(114) -> {82,114};
case_table(115) -> {83,115};
case_table(116) -> {84,116};
case_table(117) -> {85,117};
case_table(118) -> {86,118};
case_table(119) -> {87,119};
case_table(120) -> {88,120};
case_table(121) -> {89,121};
case_table(122) -> {90,122};
case_table(181) -> {924,181,924,956};
case_table(192) -> {192,224};
case_table(193) -> {193,225};
case_table(194) -> {194,226};
case_table(195) -> {195,227};
case_table(196) -> {196,228};
case_table(197) -> {197,229};
case_table(198) -> {198,230};
case_table(199) -> {199,231};
case_table(200) -> {200,232};
case_table(201) -> {201,233};
case_table(202) -> {202,234};
case_table(203) -> {203,235};
case_table(204) -> {204,236};
case_table(205) -> {205,237};
case_table(206) -> {206,238};
case_table(207) -> {207,239};
case_table(208) -> {208,240};
case_table(209) -> {209,241};
case_table(210) -> {210,242};
case_table(211) -> {211,243};
case_table(212) -> {212,244};
case_table(213) -> {213,245};
case_table(214) -> {214,246};
case_table(216) -> {216,248};
case_table(217) -> {217,249};
case_table(218) -> {218,250};
case_table(219) -> {219,251};
case_table(220) -> {220,252};
case_table(221) -> {221,253};
case_table(222) -> {222,254};
case_table(223) -> {[83,83],223,[83,115],[115,115]};
case_table(224) -> {192,224};
case_table(225) -> {193,225};
case_table(226) -> {194,226};
case_table(227) -> {195,227};
case_table(228) -> {196,228};
case_table(229) -> {197,229};
case_table(230) -> {198,230};
case_table(231) -> {199,231};
case_table(232) -> {200,232};
case_table(233) -> {201,233};
case_table(234) -> {202,234};
case_table(235) -> {203,235};
case_table(236) -> {204,236};
case_table(237) -> {205,237};
case_table(238) -> {206,238};
case_table(239) -> {207,239};
case_table(240) -> {208,240};
case_table(241) -> {209,241};
case_table(242) -> {210,242};
case_table(243) -> {211,243};
case_table(244) -> {212,244};
case_table(245) -> {213,245};
case_table(246) -> {214,246};
case_table(248) -> {216,248};
case_table(249) -> {217,249};
case_table(250) -> {218,250};
case_table(251) -> {219,251};
case_table(252) -> {220,252};
case_table(253) -> {221,253};
case_table(254) -> {222,254};
case_table(255) -> {376,255}.
