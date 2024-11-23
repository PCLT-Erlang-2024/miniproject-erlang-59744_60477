-module(main).
-export([start/2, factory/2, conveyor_belt/2, truck/2]).
-define(TRUCK_CAPACITY, 8).
-define(SIZE, 1).

factory(NumberPackages, Belts) ->
  %% Send packages conveyor belts
  lists:foreach(
    fun(PackageId) ->
      BeltIndex = ((PackageId -1) rem length(Belts)) + 1,
      {Belt, BeltId} = lists:nth(BeltIndex, Belts), % Select a belt (round-robin)
      io:format("[Factory] Package ~p sent to belt ~p.~n", [PackageId, BeltId]),
      Belt ! {package, PackageId, ?SIZE}
    end,
    lists:seq(1, NumberPackages)
  ),

  %End
  lists:foreach(fun({Belt, _}) -> Belt ! terminate end, Belts),
  io:format("[Factory] End of production.~n").

%% Conveyor Belt Process
conveyor_belt(BeltId, Truck) ->
  receive
    {package, PackageId, Size} ->
      io:format("[Belt ~p] Package ~p received and sent to Truck ~p.~n", [BeltId, PackageId, BeltId]),
      Truck ! {package, PackageId, Size},
      conveyor_belt(BeltId, Truck);

    terminate ->
      io:format("[Belt ~p] Turned off.~n", [BeltId]),
      Truck ! terminate
  end.

%% Truck Process
truck(CurrentLoad, TruckId) ->
  receive
    {package, PackageId, Size} ->
      NewLoad = CurrentLoad + Size,
      handle_package(NewLoad, PackageId, TruckId);

    terminate ->
      handle_termination(CurrentLoad, TruckId)
  end.

%% Handle termination signal
handle_termination(CurrentLoad, TruckId) ->
  case CurrentLoad of
    0 -> ok;
    _ -> io:format("[Truck ~p] Sent with capacity ~p/~p.~n", [TruckId, CurrentLoad, ?TRUCK_CAPACITY])
  end,
  io:format("[Truck ~p] End of distribution.~n", [TruckId]).

%% Handle package
handle_package(NewLoad, PackageId, TruckId) when NewLoad == ?TRUCK_CAPACITY ->
  io:format("[Truck ~p] Package ~p received, full, sending truck.~n", [TruckId, PackageId]),
  io:format("[Truck ~p] Sent with capacity ~p/~p.~n", [TruckId, NewLoad, ?TRUCK_CAPACITY]),
  truck(0, TruckId);

handle_package(NewLoad, PackageId, TruckId) ->
  io:format("[Truck ~p] Package ~p received, capacity ~p/~p.~n", [TruckId, PackageId, NewLoad, ?TRUCK_CAPACITY]),
  truck(NewLoad, TruckId).

start(NumberPackages, NumberTrucks) ->

  Trucks = [spawn(?MODULE, truck, [0, TruckId]) || TruckId <- lists:seq(1, NumberTrucks)],

  Belts = [{spawn(?MODULE, conveyor_belt, [BeltId, Truck]), BeltId} || {BeltId, Truck} <- lists:zip(lists:seq(1, NumberTrucks), Trucks)],

  spawn(?MODULE, factory, [NumberPackages, Belts]).
