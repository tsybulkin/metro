#
# This module contains the functions that sample core of the building
#

import numpy as np



def sample_core(L1,L2,lift_optional):
	"""
	returns tuple (core, partitions)
		core - is an object describing the core
		partitions - the list of rectangles specified by a tuple: (xy1,xy2)
			represented lower left and upper right corners
	"""
	core = Core(L1,L2)

	# TODO:
	# core.xy1 and core.xy2 - core corners
	# divide the rest into partitions and return (core, list_of_partitions)

	return (core, [])




class Core():

	def __init__(self,L1,L2):
		self.xy1 = np.array([0,0])
		self.xy2 = np.array([L1,L2])



