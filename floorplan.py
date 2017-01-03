#
# This module contains floorplan class
#	the floorplan assumes rectangular shape of the building and
#	rectangular shape of all units. Please look at the diagram ...
#	describing all parameters of the layout
#


import numpy as np


class FloorPlan():
	def __init__(self,shape,sizes):
		if shape == 0:
			self.L1 = sizes[0]
			self.L2 = sizes[1]
		else:
			raise "the building types except 0 are not supported"


	def copy(self):
		fp = FloorPlan(0,[self.L1, self.L2])

		return fp
