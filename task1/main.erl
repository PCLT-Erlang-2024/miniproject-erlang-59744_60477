-module(main).
-export([start/2, factory/2, conveyor_belt/2, truck/1]).
-define(TRUCK_CAPACITY, 8).

factory(NumberPackages, Belts) ->
  %% Send packages conveyor belts
  lists:foreach(
    fun(Id) ->
      BeltIndex = (Id rem length(Belts)) + 1,
      {Belt, BeltId} = lists:nth(BeltIndex, Belts), % Select a belt (round-robin)
      io:format("[Factory] Package ~p sent to belt ~p~n", [Id, BeltId]),
      Belt ! {package, Id, 1}
    end,
    lists:seq(1, NumberPackages)
  ),

  %End
  lists:foreach(fun({Belt, _}) -> Belt ! {terminate} end, Belts),
  io:format("[Factory] End of production~n").

%% Conveyor Belt Process
conveyor_belt(BeltId, Truck) ->
  receive
    {terminate} ->
      io:format("[Belt ~p] Turned off~n", [BeltId]),
      Truck ! {terminate};

    {package, Id, Size} ->
      io:format("[Belt ~p] Package ~p received~n", [BeltId, Id]),

      Truck ! {package, Id, Size},
      conveyor_belt(BeltId, Truck)
  end.

%% Truck Process
truck(CurrentLoad) ->
  receive
    {terminate} ->
      handle_termination(CurrentLoad);
    {package, Id, Size} ->
      NewLoad = CurrentLoad + Size,
      handle_package(NewLoad, Id)
  end.

%% Handle termination signal
handle_termination(CurrentLoad) ->
  case CurrentLoad of
    0 -> ok;
    _ -> io:format("[Truck] Sent with capacity ~p/~p~n", [CurrentLoad, ?TRUCK_CAPACITY])
  end,
  io:format("[Truck] End of distribution~n").

%% Handle package
handle_package(NewLoad, Id) when NewLoad == ?TRUCK_CAPACITY ->
  io:format("[Truck] Package ~p received, full, sending truck~n", [Id]),
  io:format("[Truck] Sent with capacity ~p/~p~n", [NewLoad, ?TRUCK_CAPACITY]),
  truck(0);

handle_package(NewLoad, Id) ->
  io:format("[Truck] Package ~p received, capacity ~p/~p~n", [Id, NewLoad, ?TRUCK_CAPACITY]),
  truck(NewLoad).


start(NumberPackages, NumberTrucks) ->

  Trucks = [spawn(?MODULE, truck, [0]) || _ <- lists:seq(1, NumberTrucks)],

  Belts = [{spawn(?MODULE, conveyor_belt, [BeltId, Truck]), BeltId} || {BeltId, Truck} <- lists:zip(lists:seq(1, NumberTrucks), Trucks)],

  spawn(?MODULE, factory, [NumberPackages, Belts]).
