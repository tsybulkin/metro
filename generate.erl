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
-define(MIN_APPT_SIZE, ?MIN_BATHROOM + ?MIN_KITCHEN + ?MIN_LIVINGROOM).


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
	case Area < 2 * ?MIN_APPT_SIZE of
		true -> [generate_appt(Area) | Appts];
		false->
			Appt_area = ?MIN_APPT_SIZE + rand:uniform(?MAX_APPT_SIZE - 2*?MIN_APPT_SIZE),
			generate_floor(Area-Appt_area, [generate_appt(Appt_area) | Appts])
	end.




generate_appt(Area) when Area < ?MIN_APPT_SIZE + ?MIN_BEDROOM -> generate_studio(Area);
generate_appt(Area) when Area < ?MIN_APPT_SIZE + 2*?MIN_BEDROOM -> 
	case utils:coin(0.5) of
		true -> generate_studio(Area);
		false-> generate_one_bd(Area)
	end;
generate_appt(Area) when Area < ?MIN_APPT_SIZE + 3*?MIN_BEDROOM -> 
	case utils:coin(0.3) of
		true -> generate_studio(Area);
		false-> 
			case utils:coin(0.3) of
				true -> generate_one_bd(Area);
				false-> generate_two_bd(Area)
			end
	end;
generate_appt(Area) -> 
	case utils:coin(0.2) of
		true -> generate_studio(Area);
		false-> 
			case utils:coin(0.2) of
				true -> generate_one_bd(Area);
				false-> 
					case utils:coin(0.3) of
						true -> generate_two_bd(Area);
						false-> generate_three_bd(Area)
					end
			end
	end.



generate_studio(Area) ->
	Kitchen_size = ?MIN_KITCHEN + abs(rand:normal())*?MIN_KITCHEN,
	Bathroom_size = ?MIN_BATHROOM + abs(rand:normal())*?MIN_BATHROOM,
	Livingroom_size = Area - Kitchen_size - Bathroom_size,
	case Livingroom_size >= ?MIN_LIVINGROOM of
		true -> {Kitchen_size, Bathroom_size, Livingroom_size, []};
		false-> generate_studio(Area)
	end.

generate_one_bd(Area) -> generate_studio(Area).


generate_two_bd(Area) -> generate_studio(Area).


generate_three_bd(Area) -> generate_studio(Area).












