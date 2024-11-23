-module(main).
-export([start/3, factory/2, conveyor_belt/1, truck/2]).

factory(NumberPackages, Belts) ->
  %% Send packages conveyor belts
  lists:foreach(
    fun(Id) ->
      Belt = lists:nth((Id rem length(Belts)) + 1, Belts), % Select a belt (round-robin)
      io:format("[Factory] Package ~p sent to belt ~p~n", [Id, Belt]),
      Belt ! {package, Id, 1}
    end,
    lists:seq(1, NumberPackages)
  ),

  %End
  lists:foreach(fun(Belt) -> Belt ! {terminate} end, Belts),
  io:format("[Factory] End of production~n").

%% Conveyor Belt Process
conveyor_belt(Truck) ->
  receive
    {terminate} ->
      %% Termination signal received
      io:format("[Conveyor Belt] Turned off~n"),
      Truck ! {terminate};
    {package, Id, Size} ->
      %% Forward package to truck
      io:format("[Conveyor Belt] Package ~p received~n", [Id]),
      Truck ! {package, Id, Size},
      conveyor_belt(Truck)
  end.

%% Truck Process
truck(TruckCapacity, CurrentLoad) ->
  receive
    {terminate} ->
      %% Termination signal received
      if
        CurrentLoad > 0 ->
          io:format("[Truck] Sent with capacity ~p/~p~n", [CurrentLoad, TruckCapacity]);
        true -> ok
      end,
      io:format("[Truck] End of distribution~n");
    {package, Id, Size} ->
      NewLoad = CurrentLoad + Size,
      if
        NewLoad > TruckCapacity ->
          %% Truck is over capacity, send it and start fresh
          io:format("[Truck] Sent with capacity ~p/~p~n", [CurrentLoad, TruckCapacity]),
          truck(TruckCapacity, Size);
        NewLoad == TruckCapacity ->
          %% Truck is full, send it
          io:format("[Truck] Package ~p received, full, sending truck~n", [Id]),
          io:format("[Truck] Sent with capacity ~p/~p~n", [NewLoad, TruckCapacity]),
          truck(TruckCapacity, 0);
        true ->
          %% Truck is still loading
          io:format("[Truck] Package ~p received, capacity ~p/~p~n", [Id, NewLoad, TruckCapacity]),
          truck(TruckCapacity, NewLoad)
      end
  end.

start(NumberPackages, NumberTrucks, TruckCapacity) ->

  Trucks = [spawn(?MODULE, truck, [TruckCapacity, 0]) || _ <- lists:seq(1, NumberTrucks)],

  Belts = [spawn(?MODULE, conveyor_belt, [Truck]) || Truck <- Trucks],

  spawn(?MODULE, factory, [NumberPackages, Belts]).
