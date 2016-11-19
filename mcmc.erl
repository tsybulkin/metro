%
% this module contains the MC Metropolis algorithm
%

-module(mcmc).
-export([metropolis/3]).



metropolis(X,Y,N) -> 
	InitPlan = generate:init_plan(X,Y),
	Profit = cost:estimate_profit(InitPlan),
	metropolis(X,Y,N-1,[{Profit,InitPlan}]).

metropolis(_X,_Y,0,Accepted) -> Accepted;
metropolis(X,Y,N,[{LastProfit,LastPlan}|_]=Accepted) -> 
	NewPlan = generate:modify_plan(X,Y,LastPlan),
	NewProfit = cost:estimate_profit(NewPlan),
	
	case (Delta = NewProfit - LastProfit) >= 0.0 of
		true -> metropolis(X,Y,N-1,[{NewProfit,NewPlan}|Accepted]);
		false->
			case utils:coin(math:exp(Delta/1000)) of
				true -> metropolis(X,Y,N-1,[{NewProfit,NewPlan}|Accepted]);
				false-> metropolis(X,Y,N-1,Accepted)
			end
	end.
