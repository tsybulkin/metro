#
# This module contains building plan class 
# 
# 

import numpy as np
import floorplan



GROUND_FLOOR_MIN_HEIGHT = 10.
MAX_BUILDING_HEIGHT = 1000.
STOREY_HEIGHT_MIN = 8.2

## Building dimensions
MIN_L1 = 20
MAX_L1 = 50
MIN_L2 = 20
MAX_L2 = 40

class BP():
	def __init__(self, lot_points, rules):
		self.ZC = rules[0]
		self.BC = rules[1]
		self.SR = rules[2]
		# QUESTION: do we have different rules in different towns or 
		#	we have different parameters of the same rules?

		self.lot_points = lot_points
		self.height = None
		self.set_init_plan()


	def copy(self):
		bp = BP(self.lot_points, (self.ZC,self.BC,self.SR) )
		
		## position and sizes
		bp.L1 = self.L1
		bp.L2 = self.L2
		bp.origin = self.origin.copy()
		
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

	
	def set_zero_plan(self):
		self.origin = np.mean([ self.lot_points[i] 
			for i in range(len(self.lot_points)) if i%2 == 0],axis=0)
		self.L1 = MIN_L1
		self.L2 = MIN_L2
		self.height = GROUND_FLOOR_MIN_HEIGHT
		self.storey_nbr = 1

		if self.check_ZC(): return True
		else: 
			print "unable to generate initial building plan for a given lot: %s" % str(self.lot_points)
			raise 
		


	def set_init_plan(self):
		trials = 20
		if not self.generate_pos_size(trials):
			self.set_zero_plan()

		if not self.generate_type_height(trials):
			self.set_zero_plan()

		
		# TODO: generate core for a rectangular-shaped building
		self.core = None
		
		# TODO: generate ground floor and other floors
		self.groundfloor = {}
		self.set_storey_nbr()
		self.floorplan = floorplan.FloorPlan(self.L1 ,self.L2, self.BC)
		return True


	
	def generate_pos_size(self,gen_attempts):
		
		# TODO: sample sizes for a given lot
		self.L1 = np.random.randint(MIN_L1,MAX_L1)
		self.L2 = np.random.randint(MIN_L2,MAX_L2)
		
		# TODO: sample position
		xyz = np.mean([ self.lot_points[i] for i in range(len(self.lot_points)) if i%2 == 0],
			axis=0)
		dx = np.random.randint(-10,10)
		dy = np.random.randint(-10,10)
		self.origin = xyz + np.array([dx,dy,0])
		
		# QUESTION: Should we always place the building parallel to lot front?


		# check Zonning rules
		if self.check_ZC(): return True
		elif gen_attempts > 0: return self.generate_pos_size(gen_attempts-1)
		else: return False


	
	def generate_type_height(self, gen_attempts):
		self.building_type = 0
		# sample building height
		self.height = GROUND_FLOOR_MIN_HEIGHT + STOREY_HEIGHT_MIN / (np.random.uniform() + STOREY_HEIGHT_MIN/MAX_BUILDING_HEIGHT)
		self.height = max(self.height, GROUND_FLOOR_MIN_HEIGHT)
		# TODO: sample ground floor height and other floors height. 
		# refine the total height of the building

		# check ZC
		if self.check_ZC(): return True
		elif gen_attempts > 0: return self.generate_type_height(gen_attempts-1)
		else: return False



	def set_storey_nbr(self):
		height_wo_groundfloor = self.height - GROUND_FLOOR_MIN_HEIGHT
		n_max = int(height_wo_groundfloor / STOREY_HEIGHT_MIN)+1
		n_min = int(n_max * 0.7)
		self.storey_nbr = np.random.randint(n_min,n_max+1)+1



	def get_proposal(self,n=10):
		"""
		This function should generate and return an object which is slightly
		different from self. However, there is a likelyhood not equal to zero
		that the new object will differ substantially. The distribution law
		should be similar or close to Gaussian normal distribution.
		"""		
		while n > 0:
			bp = self.copy()
		
			dx = round(np.random.randn()*1) / 2
			dy = round(np.random.randn()*1) / 2
			bp.origin += np.array([dx,dy,0])

			dw1 = int(round(np.random.randn()*1))
			dw2 = int(round(np.random.randn()*1))
			w1 = bp.L1 + dw1
			w2 = bp.L2 + dw2
			bp.L1 = max(MIN_L1, min(MAX_L1,w1)) 
			bp.L2 = max(MIN_L2, min(MAX_L2,w2)) 

			dh = int(round(np.random.randn()*3))
			h = bp.height + dh
			bp.height = max(GROUND_FLOOR_MIN_HEIGHT, h) 
			
			if bp.check_ZC(): 
				bp.set_storey_nbr()
				if bp.get_layout(): return bp
				else: n += -1
					
		return False
	


	def get_layout(self):
		"""tries to generate floor layout. Returns either True or False if 
			successful or not.
		"""
		fp = floorplan.FloorPlan(self.L1, self.L2, self.BC)
		if fp.gen_layout(): 
			self.floor_plan = fp
			return True

		return False



	
	def check_ZC(self):
		return all( r(self.L1,self.L2,self.origin,self.height, self.lot_points) 
			for r in self.ZC )


	
	def show_plan(self):
		print "**************"
		print "Building dimensions: %i x %i   Height: %i"%(self.L1,self.L2,int(self.height))
		print "Nbr of stories:", self.storey_nbr
		print "**************"
		