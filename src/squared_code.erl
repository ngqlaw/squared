%%%-------------------------------------------------------------------
%%% @author ngq
%%% @copyright (C) 2023, <ngq_scut@126.com>
%%% @doc
%%%     九宫格生成代码模块
%%% @end
%%% Created : 22. 5月 2023 下午12:55
%%%-------------------------------------------------------------------
-module(squared_code).
-author("ngq").

-include("squared.hrl").

%% API
-export([gen/2, get_neighbours/5, get_point/5]).

-define(MOD_NAME(ModPreFix, Index), lists:concat([ModPreFix, Index])).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 生成地图模块
gen(Path, ModPreFix) when is_binary(Path) ->
    {ok, List} = file:list_dir(Path),
    FullPath = squared_util:adjust_path(Path),
    do_gen(List, FullPath, ModPreFix, []);
gen(Path, ModPreFix) when is_list(Path) ->
    gen(list_to_binary(Path), ModPreFix).


%% @doc 查询相邻格子
get_neighbours(Mod, 1, _Diff, X, Y) ->
    case Mod:get({1, 0, X, Y}) of
        #point{neighbours = L} -> L;
        undefined -> []
    end;
get_neighbours(Mod, Layer, Diff, X, Y) ->
    case Mod:get({Layer, Diff, X, Y}) of
        #point{neighbours = L} -> L;
        undefined -> []
    end.

%% @doc 查询点信息
get_point(Mod, 1, _Diff, X, Y) ->
    case Mod:get({1, 0, X, Y}) of
        #point{} = Point -> Point;
        undefined -> undefined
    end;
get_point(Mod, Layer, Diff, X, Y) ->
    case Mod:get({Layer, Diff, X, Y}) of
        #point{} = Point -> Point;
        undefined -> undefined
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_gen([F|T], Path, ModPreFix, Res) ->
    FilePath = filename:join(Path, F),
    case filelib:is_dir(FilePath) of
        true ->
            case gen(FilePath, ModPreFix) of
                {ok, Add} ->
                    do_gen(T, Path, ModPreFix, Add ++ Res);
                Error ->
                    {error, F, Error}
            end;
        false ->
            case filelib:is_regular(FilePath) of
                true ->
                    [Map] = jsx:consult(FilePath),
                    {[X0, Y0], PosMap} = maps:take(<<"pos">>, Map),
                    [{IndexBin, PosList}] = maps:to_list(PosMap),
                    Index = binary_to_integer(IndexBin),
                    Module = gen_map(ModPreFix, Index, X0, Y0, PosList),
                    ok = gen_path(Module, Index),
                    do_gen(T, Path, ModPreFix, [{Index, Module}|Res]);
                false ->
                    {error, F, not_regular_file}
            end
    end;
do_gen([], _Path, _ModPreFix, Res) ->
    {ok, Res}.

%% 生成简单地图配置
gen_map(ModPreFix, Index, X0, Y0, PosList) ->
    {MaxX, MaxY, AllPos} = count_all_pos(PosList, X0, X0, Y0, []),

    Module = list_to_atom(?MOD_NAME(ModPreFix, Index)),

    XRangeStr = lists:concat(["get_x_range() -> {", X0, ", ", MaxX, "}."]),
    YRangeStr = lists:concat(["get_y_range() -> {", Y0, ", ", MaxY, "}."]),

    {Keys, Get} = gen_get_pos(AllPos, [], ["get(_) -> 0."]),
    AllBody = lists:flatten(lists:join(",", Keys)),
    AllStr = lists:flatten(["get_all() -> [", AllBody, "]."]),
    GetStr = lists:flatten(lists:join(";\n", Get)),

    TokenList = [begin {ok, Token, _} = erl_scan:string(S), Token end || S <- [
        lists:concat(["-module(", Module, ")."]),
        "-export([get_x_range/0, get_y_range/0, get_all/0, get/1]).",
        XRangeStr, YRangeStr, AllStr, GetStr
    ]],
    gen_module(Module, TokenList),
    Module.

count_all_pos([H|T], X0, MaxX, Y, Res) ->
    {X1, Add} = count_x_pos(H, X0, Y, []),
    count_all_pos(T, X0, max(MaxX, X1), Y + 1, Add ++ Res);
count_all_pos([], _, X, Y, Res) ->
    {X, Y, Res}.

count_x_pos([0|T], X, Y, Res) ->
    count_x_pos(T, X + 1, Y, Res);
count_x_pos([1|T], X, Y, Res) ->
    count_x_pos(T, X + 1, Y, [{X, Y}|Res]);
count_x_pos([], X, _Y, Res) ->
    {X, Res}.

gen_get_pos([Pos|T], Keys, Get) ->
    ValStr = io_lib:format("~p", [Pos]),
    GetStr = lists:flatten(["get(", ValStr, ") -> 1"]),
    gen_get_pos(T, [ValStr|Keys], [GetStr|Get]);
gen_get_pos([], Keys, Get) ->
    {Keys, Get}.


%% 生成地图寻路配置
gen_path(MapMod, MapId) ->
    Tab = squared_create:init(MapMod, MapId),
    {MaxLayer, PosList, InfoList} = do_gen_path(ets:first(Tab), Tab, 1,
        ["get_pos(_) -> 0."], ["get(_) -> undefined."]),

    ets:delete(Tab),

    LayerStr = lists:concat(["get_layer() -> ", MaxLayer, "."]),

    GetPosStr = lists:flatten(lists:join(";\n", PosList)),
    GetInfoStr = lists:flatten(lists:join(";\n", InfoList)),

    TokenList = [begin {ok, Token, _} = erl_scan:string(S), Token end || S <- [
        lists:concat(["-module(", MapMod, ")."]),
        "-export([get_layer/0, get_pos/1, get/1]).",
        LayerStr, GetPosStr, GetInfoStr
    ]],
    gen_module(MapMod, TokenList),
    ok.

do_gen_path('$end_of_table', _Tab, MaxLayer, PosList, InfoList) ->
    {MaxLayer, PosList, InfoList};
do_gen_path(Key0, Tab, MaxLayer, PosList, InfoList) ->
    [#point{id = {Layer, Diff, X, Y}} = Info] = ets:lookup(Tab, Key0),
    PosList1 =
        case Layer == 1 of
            true ->
                PosStr = io_lib:format("~p", [{X, Y}]),
                GetPosStr = lists:flatten(["get_pos(", PosStr, ") -> 1"]),
                [GetPosStr|PosList];
            false -> PosList
        end,
    KeyStr = io_lib:format("~p", [{Layer, Diff, X, Y}]),
    InfoStr = io_lib:format("~w", [Info]),
    GetStr = lists:flatten(["get(", KeyStr, ") -> ", InfoStr]),
    Key1 = ets:next(Tab, Key0),
    do_gen_path(Key1, Tab, max(Layer, MaxLayer),
        PosList1, [GetStr|InfoList]).


gen_module(Module, TokenList) ->
    AbsFormList = [begin {ok, A} = erl_parse:parse_form(T), A end || T <- TokenList],
    {ok, Module, Binary} = compile:forms(AbsFormList),
    code:purge(Module),
    Filename = lists:concat([Module, ".erl"]),
    {module, Module} = code:load_binary(Module, Filename, Binary),
    ok.
