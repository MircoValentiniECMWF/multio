#!/bin/bash

# Save the current Internal Field Separator
OLD_IF=$IFS;

# Geenrate files for different section2
IFS=$'\n'
echo "Automatic generation of section2 templates"
for i in `cat type_of_section2.txt` ; do
  XXX="${i}";
  tmp=`echo "${i}" | sed -e 's/^0*//'`;
  YYY=`printf "%d" "${tmp}"`;
  S2NAME="grib2_section2_${XXX}_mod.F90";
  echo "${XXX}", "${YYY}", "grib2_section2_${XXX}_mod.F90"
  cat "./grib2_section2_XXX_mod.F90" | sed -e '{s/@XXX@/'${XXX}'/};{s/@YYY@/'${YYY}'/}' > "./${S2NAME}";
done

# Generate all the include files
IFS=$'\n'
for i in `cat type_of_section2.txt` ; do
  ID="${i}";
  echo "  USE :: GRIB2_SECTION2_${ID}_MOD, ONLY: GRIB2_SECTION2_${ID}_T"
done > "./grib2_section2_use.F90.in";


IFS=$'\n'
for i in `cat type_of_section2.txt` ; do
  ID="${i}";
  tmp=`echo "${i}" | sed -e 's/^0*//'`;
  II=`printf "%d" "${tmp}"`;
  echo "  CASE (${II})"
  echo ""
  echo "    ALLOCATE( GRIB2_SECTION2_${ID}_T::GRIB2_SECTION2, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )"
  echo "    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )"
  echo ""
done > "./grib2_section2_allocate.F90.in";



# IFS=$'\n'
# for i in `cat type_of_level.txt` ; do
#   ID=`echo $i | awk '{print $1}'`
#   LNAME=`echo $i | awk '{print $2}'`
#   UNAME=`echo $i | awk '{print $3}'`
#   II=`echo ${ID} | sed -e 's/^0*//'`
#   echo "      CASE ('${LNAME}')"
#   echo "        SECTION2_TYPE = ${II}_JPIB_K"
# done


#IFS=$'\n'
#for i in `cat type_of_level.txt` ; do
#  ID=`echo $i | awk '{print $1}'`
#  echo "  grib/grib2/grib2_section4/grib2_section2/grib2_section2_${ID}_mod.F90"
#done
#echo "  grib/grib2/grib2_section4/grib2_section2/grib2_section2_factory_mod.F90"


#IFS=$'\n'
#for i in `cat type_of_level.txt` ; do
#  ID=`echo $i | awk '{print $1}'`
#  LNAME=`echo $i | awk '{print $2}'`
#  UNAME=`echo $i | awk '{print $3}'`
#  echo "!>   - @dependency [TYPE] GRIB2_SECTION2_${ID}_MOD::GRIB2_SECTION2_${ID}_T"
#done
