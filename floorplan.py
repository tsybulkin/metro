#
# This module contains floorplan class
#	the floorplan assumes rectangular shape of the building and
#	rectangular shape of all units. Please look at the diagram ...
#	describing all parameters of the layout
#


import numpy as np


class FloorPlan():
	def __init__(self,L1,L2):
		self.L1 = L1
		self.L2 = L2
	

	def copy(self):
		fp = FloorPlan(self.L1, self.L2)

		return fp
