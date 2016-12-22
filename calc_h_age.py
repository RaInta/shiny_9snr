#!  /usr/bin/python

##################################################
#                 calc_h_age.py
#
# Simple script to print out the age-based
# upper limit of the gravitational-wave (GW) strain
# expected from a continuous GW source, where we
# know the age and distance to the source.
#
#
# Created: 11 November 2016, Ra Inta
# Last modified: 20161111, RI
#
##################################################

from math import pow
from sys import argv

tSpanCasA = 12*24*3600 # 12 days
hCasA = 1.22e-24
ageCasA = 0.3 # kyr
distCasA = 3.4 # kpc
fCasA = 300 # Hz
costCasA = 3.5*24*3600 # 3.5 days in seconds
livetimeFracCasA = 12.0 / 19.2


Dkpc = float(argv[2])
TAUkyr = float(argv[3])

#print("    <h_age>" + str( hCasA * (distCasA / Dkpc) * pow(TAUkyr / ageCasA, -0.5) ) + "</h_age>")
h_age = hCasA * (distCasA / Dkpc) * pow(TAUkyr / ageCasA, -0.5)
print(argv[1] + " " + argv[2] + " " + argv[3] + str(h_age))

