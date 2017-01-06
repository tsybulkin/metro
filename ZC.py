#
# This module contains Zonning Code rules
#	which return either True or False 
#
import numpy as np

HEIGHT_MAX = 30
SET_BACK = 5


def check_setbacks(L1,L2,Oxy,h,lot_points):
	return all( is_inside(Oxy + np.array(i*L1/2,j*L2/2),lot_points) 
		for i in [-1,1] for j in [-1,1] )



def check_height(L1,L2,Oxy,h,lot_points): return h <= HEIGHT_MAX




def is_inside(xy, points):
	# points go clockwise 
	pairs = [ (points[i-1],points[i]) for i in range(len(points)) ]

	return all( np.dot(xy-p1,right_90(p2-p1)) >= 0 for (p1,p2) in pairs)



def right_90(xy):
	""" turns vector in 90 degrees clockwise
	"""
	return np.array([xy[1],-xy[0]])



