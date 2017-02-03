"""
Example of how you would call the fortran function.

f2py -c --fcompiler=gnu95 -m sum_mod sum_mod.F90

Creates the library sum_mod.so

"""

from sum_mod import sum_mod

a = sum_mod.sumpy( [ 1.0, 2.0 ] )
b = sum_mod.fumpy( [ 1.0, 2.0 ] )
c = sum_mod.sumpy( numpy.array( [ 1.0, 2.0 ] ))

# Also F90WRAP tool is good for calling fortran from python 
