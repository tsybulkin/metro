#
# This module contains functions dealing with different financial 
# information as profit estimation etc.
#

import numpy as np

## prices
ONE_BR_APT_PRICE = 55000
TWO_BR_APT_PRICE = 69000
THREE_BR_APT_PRICE = 77000

## costs
ONE_BR_APT_COST = 40000
TWO_BR_APT_COST = 54000
THREE_BR_APT_COST = 65000



def estimate_profit(bp):

	# TODO: develop a function that estimates profit as revenue(bp)-cost(bp)
	
	storey_profit = 100 * bp.L1 * bp.L2
	print 'storey profit:', storey_profit
	print 'storey nbr:',bp.storey_nbr

	if bp.storey_nbr >= 10: return bp.storey_nbr * storey_profit - 700000
	elif bp.storey_nbr >= 5: return bp.storey_nbr * storey_profit - 200000
	else: return bp.storey_nbr * storey_profit

