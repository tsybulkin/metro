#
# This module contains building plan class 
# 
# 

import numpy as np
import floorplan


GROUND_FLOOR_MIN_HEIGHT = 10.
MAX_BUILDING_HEIGHT = 1000.
STOREY_HEIGHT_MIN = 8.2


class BP():
	def __init__(self, lot_points, rules):
		self.ZC = rules[0]
		self.BC = rules[1]
		self.SR = rules[2]
		# QUESTION: do we have different rules in different towns or 
		#	we have different parameters of the same rules?

		self.lot_points = lot_points
		self.get_init_plan()


	def copy(self):
		bp = BP(self.lot_points, (self.ZC,self.BC,self.SR) )
		
		## position and sizes
		bp.L1 = self.L1
		bp.L2 = self.L2
		bp.origin = self.origin
		
		## height and type
		bp.height = self.height
		bp.building_type = self.building_type

		## building core
		bp.core = self.core

		# TODO: make floorplan an object with method copy() and use it here
		bp.groundfloor = self.groundfloor
		bp.storey_nbr = self.storey_nbr
		bp.floorplan = self.floorplan

		return bp


	def get_init_plan(self):
		trials = 20
		if not self.generate_pos_size(trials):
			raise "unable to generate initial building plan for a given lot: %s" % str(self.lot)
		
		if not self.generate_type_height(trials):
			raise "unable to generate height of the building"

		# TODO: generate core for a rectangular-shaped building
		self.core = None
		
		# TODO: generate ground floor and other floors
		self.groundfloor = {}
		self.storey_nbr = self.set_storey_nbr()
		self.floorplan = floorplan.FloorPlan(self.building_shape ,self.sizes)


	def generate_pos_size(self,gen_attempts):
		
		# TODO: sample sizes for a given lot
		self.L1 = np.random.randint(20,100)
		self.L2 = np.random.randint(25,40)
		
		# TODO: sample position
		xyz = np.mean([ self.lot_points[i] for i in range(len(self.lot_points)) if i%2 == 0])
		self.origin = xyz
		

		# check Zonning rules
		if all( r(self.) for r in self.ZC ): return True
		elif gen_attempts > 0: return generate_pos_shape(gen_attempts-1)
		else: return False


	def generate_type_height(self, gen_attempts):
		self.building_type = 0
		# sample building height
		self.height = GROUND_FLOOR_MIN_HEIGHT / (np.random.uniform() + 1/MAX_BUILDING_HEIGHT)
		
		# TODO: sample ground floor height and other floors height. 
		# refine the total height of the building

		# check ZC
		if all( r(self.L1,self.L2,self.origin,self.height) for r in self.ZC ): return True
		elif gen_attempts > 0: return generate_type_height(gen_attempts-1)
		else: return False



	def set_storey_nbr(self):
		height_wo_groundfloor = self.height - GROUND_FLOOR_MIN_HEIGHT
		n_max = int(height_wo_groundfloor / STOREY_HEIGHT_MIN)
		n_min = int(n_max * 0.7)
		self.storey_nbr = np.random.randint(n_min,n_max+1)



	def get_proposal(self):
		"""
		This function should generate and return an object which is slightly
		different from self. However, there is a likelyhood not equal to zero
		that the new object will differ substantially. The distribution law
		should be similar or close to Gaussian normal distribution.
		"""
		return self.copy()


