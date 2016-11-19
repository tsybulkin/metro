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

%% unit area constrains
-define(MAX_APPT_SIZE, 1500).
-define(MIN_BEDROOM, 80).
-define(MIN_LIVINGROOM, 150).
-define(MIN_KITCHEN, 60).
-define(MIN_BATHROOM, 48).



%%%%%%%%%%%%%%%%% Plan structure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plan = {Lift, Floor_area, Floors}, where
% Lift:  true/false
% Floor_area: the area of one floor in sq ft
% Floors = [Floor|_]; 
% Floor = [{Kitchen_size,Livingroom_size,Bathroom_size,Bedroom_sizes}|Appartments]
%

init_plan(X,Y) -> 
	Floors_nbr = rand:uniform(?MAX_FLOOR_NBR),
	Lift = sample_lift(Floors_nbr),
	Floor_area = X*Y,
	{Lift, Floor_area, sample_floors(Floor_area, Floors_nbr)}.





modify_plan(X,Y,Plan) -> Plan.




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



generate_floor(Area) ->  generate_floor(Area,[]).

generate_floor(Area, Appts) ->
	MIN_APPT_SIZE = ?MIN_BATHROOM + ?MIN_KITCHEN + ?MIN_LIVINGROOM,
	case Area < 2 * MIN_APPT_SIZE of
		true -> [generate_appt(Area) | Appts];
		false->
			Appt_area = rand:uniform(MIN_APPT_SIZE, ?MAX_APPT_SIZE - MIN_APPT_SIZE),
			generate_floor(Area-Appt_area, [generate_appt(Appt_area) | Appts])
	end.




generate_appt(Area) -> {?MIN_KITCHEN, ?MIN_LIVINGROOM, ?MIN_BATHROOM, []}.

