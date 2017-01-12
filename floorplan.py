#
# This module contains floorplan class
#	the floorplan assumes rectangular shape of the building and
#	rectangular shape of all units. Please look at the diagram ...
#	describing all parameters of the layout
#

import core
import numpy as np


class FloorPlan():
	def __init__(self, L1, L2, BC, lift_optional=True):
		self.L1 = L1
		self.L2 = L2
		self.BC = BC
		self.lift_optional = True
	

	def gen_layout(self,n=10):

		while n > 0:
			(core,partitions) = core.sample_core(self.L1,self.L2,self.lift_optional)
			self.core = core

			apts = []
			for part in partitions:
				apt = apartments.sample(part)
				if apt: apts.append(apt)
				else: 
					n += -1
					continue
			self.apts = apts

			if self.BC(): return True
			n += -1

		return False




	def copy(self):
		fp = FloorPlan(self.L1, self.L2, self.BC)

		return fp
