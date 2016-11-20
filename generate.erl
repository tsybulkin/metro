%
% This module contains generative model
%

-module(generate).
-export([init_plan/2,
		modify_plan/3
		]).

%% LOT CONSTRAINS
-define(MAX_FLOOR_NBR, 7).

%% bulding constrains
-define(LIFT_AFTER_N_FLOOR, 4).
-define(MIN_FLOORS_WITH_ONE_STAIR, 4).
-define(ONE_STAIR_AREA, 100). %sq ft
-define(LIFT_AREA, 20). %sq ft
-define(ATTEMPTS, 10).

%% unit area constrains
-define(MAX_APPT_SIZE, 1500).
-define(MIN_BEDROOM, 80).
-define(MIN_LIVINGROOM, 150).
-define(MIN_KITCHEN, 60).
-define(MIN_BATHROOM, 48).
-define(MIN_APPT_SIZE, ?MIN_BATHROOM + ?MIN_KITCHEN + ?MIN_LIVINGROOM).


%%%%%%%%%%%%%%%%% Plan structure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plan = {Lift, Floor_area, Floors}, where
% Lift:  true/false
% Floor_area: the area of one floor in sq ft
% Floors = [Floor|_]; 
% Floor = [{Kitchen_size,Bathroom_size,Livingroom_size,Bedroom_sizes}|Appartments]
%

init_plan(X,Y) -> 
	Floors_nbr = rand:uniform(?MAX_FLOOR_NBR),
	Lift = sample_lift(Floors_nbr),
	Floor_area = case Lift of
		true -> X*Y - ?LIFT_AREA - ?ONE_STAIR_AREA;
		false-> X*Y - ?ONE_STAIR_AREA
	end,
	{Lift, Floor_area, sample_floors(Floor_area, Floors_nbr)}.





modify_plan(X,Y,Plan) -> init_plan(X,Y).




sample_lift(Floors_nbr) ->
	case Floors_nbr > ?LIFT_AFTER_N_FLOOR of
		true -> true;
		false-> utils:coin((Floors_nbr-1)/?LIFT_AFTER_N_FLOOR)
	end.



sample_floors(Area,Floors_nbr) ->
	Floors = [ generate_floor(Area) || _ <- lists:seq(1,Floors_nbr)],
	case cost:appts_proportion_fits(Floors) of
		true -> Floors;
		false-> 
			io:format("appartments proportion in the building violates constrains~n"),
			sample_floors(Area,Floors_nbr)
	end.



generate_floor(Area) -> generate_floor(Area,[]).

generate_floor(Area, Appts) ->
	Appt_area = round(?MIN_APPT_SIZE * ( abs(rand:normal()) + 1 )),			
	if 
		Area - Appt_area =< ?MIN_APPT_SIZE -> [generate_appt(Area) | Appts];
		true -> generate_floor(Area-Appt_area, [generate_appt(Appt_area) | Appts])
	end.



generate_appt(Area) when Area =< ?MIN_APPT_SIZE + ?MIN_BEDROOM -> generate_studio(Area,?ATTEMPTS);
generate_appt(Area) when Area =< ?MIN_APPT_SIZE + 2*?MIN_BEDROOM -> 
	case utils:coin(0.5) of
		true -> generate_studio(Area,?ATTEMPTS);
		false-> generate_one_bd(Area,?ATTEMPTS)
	end;
generate_appt(Area) when Area =< ?MIN_APPT_SIZE + 3*?MIN_BEDROOM -> 
	case utils:coin(0.3) of
		true -> generate_studio(Area,?ATTEMPTS);
		false-> 
			case utils:coin(0.3) of
				true -> generate_one_bd(Area,?ATTEMPTS);
				false-> generate_two_bd(Area,?ATTEMPTS)
			end
	end;
generate_appt(Area) -> 
	case utils:coin(0.2) of
		true -> generate_studio(Area,?ATTEMPTS);
		false-> 
			case utils:coin(0.2) of
				true -> generate_one_bd(Area,?ATTEMPTS);
				false-> 
					case utils:coin(0.3) of
						true -> generate_two_bd(Area,?ATTEMPTS);
						false-> generate_three_bd(Area,?ATTEMPTS)
					end
			end
	end.


generate_studio(Area,0) -> 
	LR = Area - ?MIN_KITCHEN - ?MIN_BATHROOM,
	{?MIN_KITCHEN,?MIN_BATHROOM,LR,[]};
generate_studio(Area,N) ->
	Kitchen_size = round(?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN/5),
	Bathroom_size = round(?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM/5),
	Livingroom_size = Area - Kitchen_size - Bathroom_size,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, []};
		false-> 
			io:format("failed to generate studio. Area: ~p~n",[Area]), 
			generate_studio(Area,N-1)
	end.

generate_one_bd(Area,0) -> 
	LR = Area - ?MIN_KITCHEN - ?MIN_BATHROOM - ?MIN_BEDROOM,
	{?MIN_KITCHEN,?MIN_BATHROOM,LR,[?MIN_BEDROOM]};
generate_one_bd(Area,N) ->
	Kitchen_size = round(?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN/5),
	Bathroom_size = round(?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM/5),
	Bedroom_size = round(?MIN_BEDROOM +abs(rand:normal())*?MIN_BEDROOM/5),
	Livingroom_size = Area - Kitchen_size - Bathroom_size - Bedroom_size,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, [Bedroom_size]};
		false-> 
			io:format("failed to generate 1-bd appt. Area: ~p~n",[Area]), 
			generate_one_bd(Area,N-1)
	end.


generate_two_bd(Area,0) -> 
	LR = Area - ?MIN_KITCHEN - ?MIN_BATHROOM - 2*?MIN_BEDROOM,
	{?MIN_KITCHEN,?MIN_BATHROOM,LR,[?MIN_BEDROOM,?MIN_BEDROOM]};
generate_two_bd(Area,N) ->
	Kitchen_size = round(?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN/5),
	Bathroom_size = round(?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM/5),
	Bedroom_size1 = round(?MIN_BEDROOM + abs(rand:normal())*?MIN_BEDROOM/5),
	Bedroom_size2 = round(?MIN_BEDROOM + abs(rand:normal())*?MIN_BEDROOM/5),
	Livingroom_size = Area - Kitchen_size - Bathroom_size - Bedroom_size1 - Bedroom_size2,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, [Bedroom_size1, Bedroom_size2]};
		false-> 
			io:format("failed to generate 2-bd appt. Area: ~p~n",[Area]), 
			generate_two_bd(Area,N-1)
	end.



generate_three_bd(Area,0) -> 
	LR = Area - ?MIN_KITCHEN - ?MIN_BATHROOM - 3*?MIN_BEDROOM,
	{?MIN_KITCHEN,?MIN_BATHROOM,LR,[?MIN_BEDROOM,?MIN_BEDROOM,?MIN_BEDROOM]};
generate_three_bd(Area,N) ->
	Kitchen_size = round(?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN/5),
	Bathroom_size = round(?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM/5),
	Bedroom_size1 = round(?MIN_BEDROOM +abs(rand:normal())*?MIN_BEDROOM/5),
	Bedroom_size2 = round(?MIN_BEDROOM +abs(rand:normal())*?MIN_BEDROOM/5),
	Bedroom_size3 = round(?MIN_BEDROOM +abs(rand:normal())*?MIN_BEDROOM/5),
	Livingroom_size = Area - Kitchen_size - Bathroom_size - Bedroom_size1 - Bedroom_size2 - Bedroom_size3,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, [Bedroom_size1, Bedroom_size2, Bedroom_size3]};
		false-> 
			io:format("failed to generate 3-bd appt. Area: ~p~n",[Area]), 
			generate_three_bd(Area,N-1)
	end.












