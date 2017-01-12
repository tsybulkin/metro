#
# This module contains sampling apartments
#
import numpy as np


def sample_apt((xy1,xy2)):
	return Appartment(xy2-xy1)



class Appartment():

	def __init__(self, wh):
		self.bedrooms = 2
		w,h = wh
		self.area = w * h

		self.layout = None
 

# TODO: develop sampling an apartment from the list of standard apartments which
#	fit some conditions like dimensions, layout, list of materials, etc.
