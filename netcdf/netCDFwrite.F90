int, dimension(NX,NY) :: data

ierr = NF90_CREATE( "example.nc", NF90_CLOBBER, nicd )
data(:,:) = 1 ! entering define mode

ierr = NF90_DEF_DIM( ncid, "x", NX, x_dimid )
ierr = NF90_DEF_DIM( ncid, "y", NY, y_dimid )
ierr = NF90_DEF_VAR( ncid, "data", NF90_INT, [ x_dimid, y_dimid ], &
     &               varid )
ierr = NF90_ENDDEF( ncid ) ! end define mode and enter data mode

ierr = NF90_PUT_VAR( ncid, varid, data) !write data

ierr = NF90_CLOSE( ncid )


