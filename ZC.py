#
# This module contains Zonning Code rules
#	which return either True or False 
#
import numpy as np


HEIGHT_MAX = 45
SET_BACK = 5


def check_setbacks(L1,L2,Oxyz,h,lot_points):
	bld_corners = [ Oxyz + np.array([i*L1/2, j*L2/2, 0.]) 
		for i in [-1,1] for j in [-1,1] ] 
	
	lot_corners = [ lot_points[i] for i in range(len(lot_points)) if i%2==0]
	
	return all( is_inside(c,lot_corners) for c in bld_corners )



def check_height(L1,L2,Oxyz,h,lot_points): return h <= HEIGHT_MAX




def is_inside(xyz, points):
	# points go clockwise 
	pairs = [ (points[i-1],points[i]) for i in range(len(points)) ]

	return all( np.dot(xyz-p1,right_90(p2-p1)) >= 0 for (p1,p2) in pairs)



def right_90(xyz):
	""" turns vector in 90 degrees clockwise
	"""
	return np.array([xyz[1],-xyz[0], xyz[2]])



