#
# This module contains the entry point in the building 
# generating process. 
# To run: find_bp(lot_points, rules)
# 	lot_points is a list of 3D points representing a given lot
#	QUESTION: how to represent different sides of the lot? [ xyz1,'side',xyz2,'rear', ...]
# 	rules is a function that checks the building plan returning either true or false
#	QUESTION: maybe divide the rules into the groups of rules: (ZC rules, BC rules, Soft rules)
#	attempts is a number of attempts that will be made to find an optimal plan

import numpy as np
import building_plan, finance, ZC
import matplotlib.pyplot as plt

MAX_REJECTS = 25


def find_bp(lot_points, rules, attempts_nbr=500):
	i = 0
	bp = building_plan.BP(lot_points, rules)
	profit = finance.estimate_profit(bp)
	best_plans = [ (profit,bp) ]

	rejects = 0
	while i < attempts_nbr:
		i += 1
		if i%10 == 0: print 'round:',i

		new_bp = bp.get_proposal()
		if not new_bp:  
			print "\tCannot generate new proposal..."
			new_bp = building_plan.BP(lot_points, rules)
	
		new_profit = finance.estimate_profit(new_bp)

		if point_accepted(new_profit, profit):
			best_plans.append( (new_profit,new_bp) )
			bp = new_bp
			profit = new_profit
			rejects = 0
		else:
			rejects += 1
			if rejects > MAX_REJECTS:
				print "\nNew epoch"
				bp = building_plan.BP(lot_points, rules)
				profit = finance.estimate_profit(bp)
	


	return best_plans



def point_accepted(u2,u1):
	if u2 - u1 >= 0.: return True
	else: return np.random.uniform() < np.exp((u2-u1)/5000.)



if __name__ == '__main__':
	lot_points = [ np.array([0.,0.,0.]), 'side',
				np.array([0.,50.,0.]), 'rear',
				np.array([40.,50.,0.]), 'side',
				np.array([40.,0.,0.]), 'front' ]
	
	# QUESTION: view is important! Should we take into account view quality 
	#	for each side of a given lot?
	
	rules = ([ZC.check_setbacks, ZC.check_height],	# ZC
			[lambda :True],		# BC
			[lambda :0])		# Soft rules

	plans = find_bp(lot_points,rules)
	pr = [ p for (p,_) in plans]
	
	profit,opt = max(plans)
	print "\nProfit exceeded: $%.3f mil" % (profit/1e6)
	opt.show_plan()

	plt.plot(pr)
	plt.show()



