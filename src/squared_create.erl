%%%-------------------------------------------------------------------
%%% @author ngq
%%% @copyright (C) 2022, <ngq_scut@126.com>
%%% @doc
%%%     九宫格地图索引
%%% @end
%%% Created : 25. 10月 2022 18:16
%%%-------------------------------------------------------------------
-module(squared_create).
-author("ngq").

-include("squared.hrl").

%% API
-export([init/2, clean/1]).
-export([get_neighbours/5, get_point/5]).

-define(MAP_NAME(MapId), lists:concat(["map_layer_", MapId])).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 初始指定地图索引
-spec(init(MapMod::atom(), MapId::integer()) -> Tab::ets:tab()).
init(MapMod, MapId) ->
    Tab = ets:new(list_to_atom(?MAP_NAME(MapId)), [
        named_table, set, {keypos, #point.id}
    ]),
    All = MapMod:get_all(),
    gen_first_layer(All, MapMod, Tab),
    {MinX, MaxX} = MapMod:get_x_range(),
    {MinY, MaxY} = MapMod:get_y_range(),
    gen_layer(?SQUARED_LAYER_N, 0, MinX, MaxX, MinY, MaxY, Tab),
    gen_layer(?SQUARED_LAYER_N, 1, MinX, MaxX + 1, MinY, MaxY, Tab),
    gen_layer(?SQUARED_LAYER_N, 2, MinX, MaxX + 2, MinY, MaxY, Tab),
    Tab.

%% @doc 清除指定地图索引
-spec(clean(MapId::integer()) -> ok).
clean(MapId) ->
    Tab = list_to_existing_atom(?MAP_NAME(MapId)),
    ets:delete(Tab),
    ok.

%% @doc 查询相邻格子
get_neighbours(Tab, 1, _Diff, X, Y) ->
    case ets:lookup(Tab, {1, 0, X, Y}) of
        [#point{neighbours = L}] -> L;
        [] -> []
    end;
get_neighbours(Tab, Layer, Diff, X, Y) ->
    case ets:lookup(Tab, {Layer, Diff, X, Y}) of
        [#point{neighbours = L}] -> L;
        [] -> []
    end.

%% @doc 查询点信息
get_point(Tab, 1, _Diff, X, Y) ->
    case ets:lookup(Tab, {1, 0, X, Y}) of
        [#point{} = Point] -> Point;
        [] -> undefined
    end;
get_point(Tab, Layer, Diff, X, Y) ->
    case ets:lookup(Tab, {Layer, Diff, X, Y}) of
        [#point{} = Point] -> Point;
        [] -> undefined
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 底层索引
gen_first_layer([{X, Y}|T], Map, Tab) ->
    Neighbours0 = gen_neighbours(X, Y),
    {OutPass, Neighbours1} = lists:foldl(
        fun({X0, Y0, DX, DY}, {Acc1, Acc2}) ->
            case Map:get({X0, Y0}) == 1 of
                true ->
                    FaceTo = squared_util:turn_to({X, Y}, {X0, Y0}, ?SQUARED_CENTER),
                    {[{FaceTo, [{X, Y}]}|Acc1], [{{X0, Y0}, DX, DY}|Acc2]};
                false ->
                    {Acc1, Acc2}
            end
        end, {[], []}, Neighbours0
    ),
    InPass = direction(?SCENE_ALL_DIRECTION, []),
    ets:insert(Tab, #point{
        id = {1, 0, X, Y},
        out_pass = OutPass,
        in_pass = InPass,
        neighbours = Neighbours1
    }),
    gen_first_layer(T, Map, Tab);
gen_first_layer([], _Map, _Tab) -> ok.

%% 相邻点列表
gen_neighbours(X0, Y0) ->
    X1 = X0 - 1,
    X2 = X0 + 1,
    Y1 = Y0 - 1,
    Y2 = Y0 + 1,
    [
        {X1, Y2, 1, 1}, {X0, Y2, 0, 1}, {X2, Y2, 1, 1},
        {X1, Y0, 1, 0}, {X2, Y0, 1, 0},
        {X1, Y1, 1, 1}, {X0, Y1, 0, 1}, {X2, Y1, 1, 1}
    ].

%% 所有方向组合列表
direction([H|T], Res) ->
    Add = [{{H, Last}, []} || Last <- T],
    direction(T, Add ++ Res);
direction([], Res) -> Res.

%% 层九宫
gen_layer(Layer, Diff, MinX0, MaxX0, MinY0, MaxY0, Tab) ->
    MinX1 = get_d(MinX0) - 1,
    MaxX1 = get_d(MaxX0) - 1,

    MinY1 = get_d(MinY0) - 1,
    MaxY1 = get_d(MaxY0) - 1,

    L = [{X, Z} || X <- lists:seq(MinX1, MaxX1), Z <- lists:seq(MinY1, MaxY1)],
    gen_box(L, Tab, Layer, Layer div ?SQUARED_LAYER_N, Diff),

    D = max(MaxX1 - MinX1 + 1, MaxY1 - MinY1 + 1),
    case D > ?SQUARED_LAYER_N of
        true ->
            NextLayer = erlang:trunc(Layer * ?SQUARED_LAYER_N),
            gen_layer(NextLayer, Diff, MinX1, MaxX1, MinY1, MaxY1, Tab);
        false -> ok
    end.

%% 转换比例
get_d(Value) ->
    Value1 = Value + 1,
    D = Value1 div ?SQUARED_LAYER_N,
    case (Value1 rem ?SQUARED_LAYER_N) == 0 of
        true -> D;
        false -> D + 1
    end.

%% 生成九宫格信息
gen_box([{X, Y}|T], Tab, Layer, PreLayer, Diff) ->
    PreDiff = Diff div PreLayer,
    LayerX0 = erlang:trunc(X * ?SQUARED_LAYER_N) - PreDiff,
    LayerY0 = erlang:trunc(Y * ?SQUARED_LAYER_N),
    LayerX1 = LayerX0 + 1,
    LayerX2 = LayerX0 + 2,
    LayerY1 = LayerY0 + 1,
    LayerY2 = LayerY0 + 2,
    Box0 = [
        {?SQUARED_DOWN_LEFT, {LayerX0, LayerY0}},
        {?SQUARED_DOWN, {LayerX1, LayerY0}},
        {?SQUARED_DOWN_RIGHT, {LayerX2, LayerY0}},
        {?SQUARED_LEFT, {LayerX0, LayerY1}},
        {?SQUARED_CENTER, {LayerX1, LayerY1}},
        {?SQUARED_RIGHT, {LayerX2, LayerY1}},
        {?SQUARED_UP_LEFT, {LayerX0, LayerY2}},
        {?SQUARED_UP, {LayerX1, LayerY2}},
        {?SQUARED_UP_RIGHT, {LayerX2, LayerY2}}
    ],
    Box1 = lists:filter(
        fun({_, {PX0, PY0}}) ->
            case PreLayer == 1 of
                true ->
                    ets:member(Tab, {1, 0, PX0, PY0});
                false ->
                    ets:member(Tab, {PreLayer, Diff, PX0, PY0})
            end
        end, Box0),
    case Box1 == [] of
        true -> skip;
        false ->
            %% 相邻外格子和方向外连接信息
            {Neighbours, OutPass} = lists:foldl(
                fun(D, {Acc1, Acc2}) ->
                    OPList = layer_out_pass(D, Tab, PreLayer, Diff, Box1),
                    case OPList == [] of
                        true -> {Acc1, Acc2};
                        false -> {[neighbour(D, {X, Y})|Acc1], OPList ++ Acc2}
                    end
                end, {[], []}, ?SCENE_OUT_DIRECTION),
            %% 内部连接情况
            InPass = layer_in_pass(?SCENE_ALL_DIRECTION, Tab, PreLayer, Diff, Box1, []),
            ets:insert(Tab, #point{
                id = {Layer, Diff, X, Y},
                out_pass = OutPass,
                in_pass = InPass,
                neighbours = Neighbours
            })
    end,
    gen_box(T, Tab, Layer, PreLayer, Diff);
gen_box([], _Tab, _Layer, _PreLayer, _Diff) -> ok.

%% 指定方向相邻格子信息
neighbour(?SQUARED_UP_LEFT, {X, Y}) -> {{X - 1, Y + 1}, 1, 1};
neighbour(?SQUARED_UP, {X, Y}) -> {{X, Y + 1}, 0, 1};
neighbour(?SQUARED_UP_RIGHT, {X, Y}) -> {{X + 1, Y + 1}, 1, 1};
neighbour(?SQUARED_LEFT, {X, Y}) -> {{X - 1, Y}, 1, 0};
neighbour(?SQUARED_RIGHT, {X, Y}) -> {{X + 1, Y}, 1, 0};
neighbour(?SQUARED_DOWN_LEFT, {X, Y}) -> {{X - 1, Y - 1}, 1, 1};
neighbour(?SQUARED_DOWN, {X, Y}) -> {{X, Y - 1}, 0, 1};
neighbour(?SQUARED_DOWN_RIGHT, {X, Y}) -> {{X + 1, Y - 1}, 1, 1}.

%% 九宫格外连接点
layer_out_pass(?SQUARED_DOWN, Tab, Layer, Diff, Box) ->
    Add1 =
        case lists:keyfind(?SQUARED_DOWN, 1, Box) of
            {_, {X1, Y1}} ->
                case check_out_pass(?SQUARED_DOWN, Tab, Layer, Diff, X1, Y1) of
                    true -> [{X1, Y1}];
                    false -> []
                end;
            false -> []
        end,
    Add2 =
        case lists:keyfind(?SQUARED_DOWN_LEFT, 1, Box) of
            {_, {X2, Y2}} ->
                case check_out_pass(?SQUARED_DOWN, Tab, Layer, Diff, X2, Y2) of
                    true -> [{X2, Y2}];
                    false -> []
                end;
            false -> []
        end,
    Add3 =
        case lists:keyfind(?SQUARED_DOWN_RIGHT, 1, Box) of
            {_, {X3, Y3}} ->
                case check_out_pass(?SQUARED_DOWN, Tab, Layer, Diff, X3, Y3) of
                    true -> [{X3, Y3}];
                    false -> []
                end;
            false -> []
        end,
    PointList = Add1 ++ Add2 ++ Add3,
    case PointList == [] of
        true -> [];
        false -> [{?SQUARED_DOWN, PointList}]
    end;
layer_out_pass(?SQUARED_DOWN_LEFT, Tab, Layer, Diff, Box) ->
    case lists:keyfind(?SQUARED_DOWN_LEFT, 1, Box) of
        {_, {X, Y}} ->
            case check_out_pass(?SQUARED_DOWN_LEFT, Tab, Layer, Diff, X, Y) of
                true -> [{?SQUARED_DOWN_LEFT, [{X, Y}]}];
                false -> []
            end;
        false -> []
    end;
layer_out_pass(?SQUARED_DOWN_RIGHT, Tab, Layer, Diff, Box) ->
    case lists:keyfind(?SQUARED_DOWN_RIGHT, 1, Box) of
        {_, {X, Y}} ->
            case check_out_pass(?SQUARED_DOWN_RIGHT, Tab, Layer, Diff, X, Y) of
                true -> [{?SQUARED_DOWN_RIGHT, [{X, Y}]}];
                false -> []
            end;
        false -> []
    end;
layer_out_pass(?SQUARED_UP, Tab, Layer, Diff, Box) ->
    Add1 =
        case lists:keyfind(?SQUARED_UP, 1, Box) of
            {_, {X1, Y1}} ->
                case check_out_pass(?SQUARED_UP, Tab, Layer, Diff, X1, Y1) of
                    true -> [{X1, Y1}];
                    false -> []
                end;
            false -> []
        end,
    Add2 =
        case lists:keyfind(?SQUARED_UP_LEFT, 1, Box) of
            {_, {X2, Y2}} ->
                case check_out_pass(?SQUARED_UP, Tab, Layer, Diff, X2, Y2) of
                    true -> [{X2, Y2}];
                    false -> []
                end;
            false -> []
        end,
    Add3 =
        case lists:keyfind(?SQUARED_UP_RIGHT, 1, Box) of
            {_, {X3, Y3}} ->
                case check_out_pass(?SQUARED_UP, Tab, Layer, Diff, X3, Y3) of
                    true -> [{X3, Y3}];
                    false -> []
                end;
            false -> []
        end,
    PointList = Add1 ++ Add2 ++ Add3,
    case PointList == [] of
        true -> [];
        false -> [{?SQUARED_UP, PointList}]
    end;
layer_out_pass(?SQUARED_UP_LEFT, Tab, Layer, Diff, Box) ->
    case lists:keyfind(?SQUARED_UP_LEFT, 1, Box) of
        {_, {X, Y}} ->
            case check_out_pass(?SQUARED_UP_LEFT, Tab, Layer, Diff, X, Y) of
                true -> [{?SQUARED_UP_LEFT, [{X, Y}]}];
                false -> []
            end;
        false -> []
    end;
layer_out_pass(?SQUARED_UP_RIGHT, Tab, Layer, Diff, Box) ->
    case lists:keyfind(?SQUARED_UP_RIGHT, 1, Box) of
        {_, {X, Y}} ->
            case check_out_pass(?SQUARED_UP_RIGHT, Tab, Layer, Diff, X, Y) of
                true -> [{?SQUARED_UP_RIGHT, [{X, Y}]}];
                false -> []
            end;
        false -> []
    end;
layer_out_pass(?SQUARED_LEFT, Tab, Layer, Diff, Box) ->
    Add1 =
        case lists:keyfind(?SQUARED_LEFT, 1, Box) of
            {_, {X1, Y1}} ->
                case check_out_pass(?SQUARED_LEFT, Tab, Layer, Diff, X1, Y1) of
                    true -> [{X1, Y1}];
                    false -> []
                end;
            false -> []
        end,
    Add2 =
        case lists:keyfind(?SQUARED_UP_LEFT, 1, Box) of
            {_, {X2, Y2}} ->
                case check_out_pass(?SQUARED_LEFT, Tab, Layer, Diff, X2, Y2) of
                    true -> [{X2, Y2}];
                    false -> []
                end;
            false -> []
        end,
    Add3 =
        case lists:keyfind(?SQUARED_DOWN_LEFT, 1, Box) of
            {_, {X3, Y3}} ->
                case check_out_pass(?SQUARED_LEFT, Tab, Layer, Diff, X3, Y3) of
                    true -> [{X3, Y3}];
                    false -> []
                end;
            false -> []
        end,
    PointList = Add1 ++ Add2 ++ Add3,
    case PointList == [] of
        true -> [];
        false -> [{?SQUARED_LEFT, PointList}]
    end;
layer_out_pass(?SQUARED_RIGHT, Tab, Layer, Diff, Box) ->
    Add1 =
        case lists:keyfind(?SQUARED_RIGHT, 1, Box) of
            {_, {X1, Y1}} ->
                case check_out_pass(?SQUARED_RIGHT, Tab, Layer, Diff, X1, Y1) of
                    true -> [{X1, Y1}];
                    false -> []
                end;
            false -> []
        end,
    Add2 =
        case lists:keyfind(?SQUARED_UP_RIGHT, 1, Box) of
            {_, {X2, Y2}} ->
                case check_out_pass(?SQUARED_RIGHT, Tab, Layer, Diff, X2, Y2) of
                    true -> [{X2, Y2}];
                    false -> []
                end;
            false -> []
        end,
    Add3 =
        case lists:keyfind(?SQUARED_DOWN_RIGHT, 1, Box) of
            {_, {X3, Y3}} ->
                case check_out_pass(?SQUARED_RIGHT, Tab, Layer, Diff, X3, Y3) of
                    true -> [{X3, Y3}];
                    false -> []
                end;
            false -> []
        end,
    PointList = Add1 ++ Add2 ++ Add3,
    case PointList == [] of
        true -> [];
        false -> [{?SQUARED_RIGHT, PointList}]
    end.

%% 检查方向点外联通情况
check_out_pass(?SQUARED_DOWN, Tab, Layer, Diff, X0, Y0) ->
    Y1 = Y0 - 1,
    L = [{X0 - 1, Y1}, {X0, Y1}, {X0 + 1, Y1}],
    List = get_neighbours(Tab, Layer, Diff, X0, Y0),
    lists:any(fun(P) -> lists:keymember(P, 1, List) end, L);
check_out_pass(?SQUARED_DOWN_LEFT, Tab, Layer, Diff, X, Y) ->
    List = get_neighbours(Tab, Layer, Diff, X, Y),
    lists:keymember({X - 1, Y - 1}, 1, List);
check_out_pass(?SQUARED_DOWN_RIGHT, Tab, Layer, Diff, X, Y) ->
    List = get_neighbours(Tab, Layer, Diff, X, Y),
    lists:keymember({X + 1, Y - 1}, 1, List);
check_out_pass(?SQUARED_UP, Tab, Layer, Diff, X0, Y0) ->
    Y1 = Y0 + 1,
    L = [{X0 - 1, Y1}, {X0, Y1}, {X0 + 1, Y1}],
    List = get_neighbours(Tab, Layer, Diff, X0, Y0),
    lists:any(fun(P) -> lists:keymember(P, 1, List) end, L);
check_out_pass(?SQUARED_UP_LEFT, Tab, Layer, Diff, X, Y) ->
    List = get_neighbours(Tab, Layer, Diff, X, Y),
    lists:keymember({X - 1, Y + 1}, 1, List);
check_out_pass(?SQUARED_UP_RIGHT, Tab, Layer, Diff, X, Y) ->
    List = get_neighbours(Tab, Layer, Diff, X, Y),
    lists:keymember({X + 1, Y + 1}, 1, List);
check_out_pass(?SQUARED_LEFT, Tab, Layer, Diff, X0, Y0) ->
    X1 = X0 - 1,
    L = [{X1, Y0 - 1}, {X1, Y0}, {X1, Y0 + 1}],
    List = get_neighbours(Tab, Layer, Diff, X0, Y0),
    lists:any(fun(P) -> lists:keymember(P, 1, List) end, L);
check_out_pass(?SQUARED_RIGHT, Tab, Layer, Diff, X0, Y0) ->
    X1 = X0 + 1,
    L = [{X1, Y0 - 1}, {X1, Y0}, {X1, Y0 + 1}],
    List = get_neighbours(Tab, Layer, Diff, X0, Y0),
    lists:any(fun(P) -> lists:keymember(P, 1, List) end, L).

%% 九宫格内部连接
layer_in_pass([D0 | Tail], Tab, Layer, Diff, Box, Res) ->
    Add = lists:foldl(
        fun(D1, Acc) ->
            case is_d_pass(Tab, Layer, Diff, Box, D0, D1) of
                {ok, L} -> [{{D0, D1}, L}|Acc];
                false -> Acc
            end
        end, [], Tail
    ),
    layer_in_pass(Tail, Tab, Layer, Diff, Box, Add ++ Res);
layer_in_pass([], _Tab, _Layer, _Diff, _Box, Res) -> Res.

%% 九宫格内部连接检查
is_d_pass(Tab, Layer, Diff, Box, D0, D1) ->
    case lists:keyfind(D0, 1, Box) of
        {_, {X0, Y0}} ->
            case lists:keyfind(D1, 1, Box) of
                {_, {X1, Y1}} ->
                    L0 = path_exist({X0, Y0}, {X1, Y1}, Tab, Layer, Diff, Box, 0, 0, 0, [], []),
                    L1 = get_element(L0, []),
                    case L1 == [] of
                        true -> false;
                        false ->
                            [Best|_] = _L2 = lists:sort(L1),
                            {ok, [Best]}
                    end;
                false -> false
            end;
        false -> false
    end.

%% 查询连接路径
path_exist(P, P, _Tab, _Layer, _Diff, _Box, Sum, XSum, YSum, List, _) ->
    [#path{
        sum = Sum, x_sum = XSum, y_sum = YSum,
        list = lists:reverse([P|List])
    }];
path_exist({X, Y} = P, Target, Tab, Layer, Diff, Box, Sum, XSum, YSum, List, Done) ->
    case get_point(Tab, Layer, Diff, X, Y) of
        #point{neighbours = Neighbours0} ->
            {Neighbours1, Done1} = lists:foldl(
                fun({P0, _, _} = E, {Acc1, Acc2}) ->
                    case lists:member(P0, List) orelse lists:member(P0, Done) orelse
                        (not lists:keymember(P0, 2, Box)) of
                        true -> {Acc1, Acc2};
                        false ->
                            {[E|Acc1], [P0|Acc2]}
                    end
                end, {[], Done}, Neighbours0
            ),
            [path_exist(P1, Target, Tab, Layer, Diff, Box, Sum + 1, XSum + DX, YSum + DY,
                [P|List], Done1) || {P1, DX, DY} <- Neighbours1];
        undefined -> []
    end.

get_element([H|T], Res) when is_list(H) ->
    Add = get_element(H, []),
    get_element(T, Add ++ Res);
get_element([#path{list = []}|T], Res) ->
    get_element(T, Res);
get_element([#path{} = E|T], Res) ->
    get_element(T, [E|Res]);
get_element([], Res) ->
    Res.
