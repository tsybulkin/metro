#
# This module contains building plan class 
# 
# 

import numpy as np

GROUND_FLOOR_MIN_HEIGHT = 10.
MAX_BUILDING_HEIGHT = 1000.

class BP():
	def __init__(self, lot_points, rules):
		self.rules = rules
		self.lot_points = lot_points
		self.get_init_plan()


	def copy(self):
		bp = BP(self.lot_points, self.rules)
		
		## position and shape
		bp.building_shape = self.building_shape
		bp.building_sizes = self.sizes
		bp.origin = self.origin
		bp.height = self.height

		## building core
		bp.core = self.core

		bp.groundfloor = self.groundfloor
		bp.storey_nbr = self.storey_nbr
		bp.floorplan = self.floorplan[]


	def get_init_plan(self):
		if not self.generate_pos_shape():
			raise "unable to generate initial building plan for a given lot: %s" % str(self.lot)
		
		# TODO: generate other parameters of initial building plan



	def generate_pos_shape(self):
		self.building_shape = 0
		xyz = np.mean(self.lot_points)
		self.origin = xyz
		self.height = GROUND_FLOOR_MIN_HEIGHT / (np.random.uniform() + 1/MAX_BUILDING_HEIGHT)


	def get_proposal(self):
		"""
		This function should generate and return an object which is slightly
		different from self. However, there is a likelyhood not equal to zero
		that the new object will differ substantially. The distribution law
		should be similar or close to Gaussian normal distribution.
		"""
		return self.copy()