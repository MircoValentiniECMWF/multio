#!/bin/bash

# Save the current Internal Field Separator
OLD_IF=$IFS;

# Geenrate files for different section4
IFS=$'\n'
echo "Automatic generation of section4 templates"
for i in `cat type_of_section4.txt` ; do
  XXX="${i}";
  tmp=`echo "${i}" | sed -e 's/^0*//'`;
  YYY=`printf "%d" "${tmp}"`;
  S4NAME="grib2_section4_${XXX}_mod.F90";
  echo "${XXX}", "${YYY}", "grib2_section4_${XXX}_mod.F90"
  cat "./grib2_section4_XXX_mod.F90" | sed -e '{s/@XXX@/'${XXX}'/};{s/@YYY@/'${YYY}'/}' > "../${S4NAME}";
done

# Generate all the include files
{
rm -f grib2_section4_use.F90.in
IFS=$'\n'
for i in `cat type_of_section4.txt` ; do
  ID="${i}";
  echo "  USE :: GRIB2_SECTION4_${ID}_MOD, ONLY: GRIB2_SECTION4_${ID}_T" >> "./grib2_section4_use.F90.in";
done
}

{
rm -f grib2_section4_allocate.F90.in
IFS=$'\n'
for i in `cat type_of_section4.txt` ; do
  ID="${i}";
  tmp=`echo "${i}" | sed -e 's/^0*//'`;
  II=`printf "%d" "${tmp}"`;
  echo "  CASE (${II})"  >> "./grib2_section4_allocate.F90.in";
  echo ""  >> "./grib2_section4_allocate.F90.in";
  echo "    ALLOCATE( GRIB2_SECTION4_${ID}_T::GRIB2_SECTION4, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )"  >> "./grib2_section4_allocate.F90.in";
  echo "    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )"  >> "./grib2_section4_allocate.F90.in";
  echo ""  >> "./grib2_section4_allocate.F90.in";
done
}



# IFS=$'\n'
# for i in `cat type_of_level.txt` ; do
#   ID=`echo $i | awk '{print $1}'`
#   LNAME=`echo $i | awk '{print $2}'`
#   UNAME=`echo $i | awk '{print $4}'`
#   II=`echo ${ID} | sed -e 's/^0*//'`
#   echo "      CASE ('${LNAME}')"
#   echo "        SECTION4_TYPE = ${II}_JPIB_K"
# done


#IFS=$'\n'
#for i in `cat type_of_level.txt` ; do
#  ID=`echo $i | awk '{print $1}'`
#  echo "  grib/grib2/grib2_section4/grib2_section4/grib2_section4_${ID}_mod.F90"
#done
#echo "  grib/grib2/grib2_section4/grib2_section4/grib2_section4_factory_mod.F90"


#IFS=$'\n'
#for i in `cat type_of_level.txt` ; do
#  ID=`echo $i | awk '{print $1}'`
#  LNAME=`echo $i | awk '{print $2}'`
#  UNAME=`echo $i | awk '{print $4}'`
#  echo "!>   - @dependency [TYPE] GRIB2_SECTION4_${ID}_MOD::GRIB2_SECTION4_${ID}_T"
#done
