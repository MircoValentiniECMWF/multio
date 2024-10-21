#!/bin/bash

IFS=$'\n'
for i in `cat type_of_section4_param.txt` ; do
  UNAME=`echo $i | awk '{print $1}'`
  LNAME=`echo $i | awk '{print $1}' |sed -e 's/.*/\L&/'`
  FNAME="grib2_section4_${LNAME}_mod.F90"
  echo $LNAME
  echo $UNAME
  echo $FNAME
  rm -f $FNAME
  cat grib2_section4_XXX_mod.F90 | sed "s/@XXX@/${UNAME}/g" | sed "s/@YYY@/${UNAME}/g" | sed "s/@ZZZ@/${LNAME}/g" > ./$FNAME
done

#IFS=$'\n'
#for i in `cat type_of_level.txt` ; do
#  ID=`echo $i | awk '{print $1}'`
#  LNAME=`echo $i | awk '{print $2}'`
#  UNAME=`echo $i | awk '{print $3}'`
#  echo "  USE :: GRIB2_LEVEL_CONFIGURATOR_${ID}_MOD, ONLY: GRIB2_LEVEL_CONFIGURATOR_${ID}_T"
#done


#IFS=$'\n'
#for i in `cat type_of_level.txt` ; do
#  ID=`echo $i | awk '{print $1}'`
#  LNAME=`echo $i | awk '{print $2}'`
#  UNAME=`echo $i | awk '{print $3}'`
#  II=`echo ${ID} | sed -e 's/^0*//'`
#  echo "  CASE (${II})"
#  echo " "
#  echo "    ALLOCATE( GRIB2_LEVEL_CONFIGURATOR_${ID}_T::GRIB2_LEVEL_CONFIGURATOR, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )"
#  echo "    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )"
#  echo " "
#done



# IFS=$'\n'
# for i in `cat type_of_level.txt` ; do
#   ID=`echo $i | awk '{print $1}'`
#   LNAME=`echo $i | awk '{print $2}'`
#   UNAME=`echo $i | awk '{print $3}'`
#   II=`echo ${ID} | sed -e 's/^0*//'`
#   echo "      CASE ('${LNAME}')"
#   echo "        LEVEL_CONFIGURATOR_TYPE = ${II}_JPIB_K"
# done


#IFS=$'\n'
#for i in `cat type_of_level.txt` ; do
#  ID=`echo $i | awk '{print $1}'`
#  echo "  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_${ID}_mod.F90"
#done
#echo "  grib/grib2/grib2_section4/grib2_level_configurator/grib2_level_configurator_factory_mod.F90"


#IFS=$'\n'
#for i in `cat type_of_level.txt` ; do
#  ID=`echo $i | awk '{print $1}'`
#  LNAME=`echo $i | awk '{print $2}'`
#  UNAME=`echo $i | awk '{print $3}'`
#  echo "!>   - @dependency [TYPE] GRIB2_LEVEL_CONFIGURATOR_${ID}_MOD::GRIB2_LEVEL_CONFIGURATOR_${ID}_T"
#done
