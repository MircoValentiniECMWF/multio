#!/bin/bash

if [[ $# -eq 1  ]] ; then
  outDir="$1";
else
  outDir="."
fi

for i in `cat type_of_level.txt` ; do
    echo $i
    UNAME=`echo $i | awk '{print $1}'`
    LNAME=`echo $i | awk '{print $1}' |sed -e 's/.*/\L&/'`
    FNAME="${outDir}/grib2_section4_${LNAME}_mod.F90"
    rm -f "${FNAME}"
    # Open the file and process it line by line
    while IFS= read -r line; do
        # echo $line
        # echo '@SET_LEVEL_CODE@'
        if [[ "${line}" == '@SET_LEVEL_CODE@' ]]; then
            # echo "${line}"
            # Call the executable and forward its output
            ./generateLevelMetadata.x "${i}" >> "${FNAME}"
        else
            parsedLine=`echo "${line}" | sed -e "s/@XXX@/${UNAME}/" | sed -e "s/@YYY@/${UNAME}/" | sed -e "s/@ZZZ@/${LNAME}/"`;
            # Forward the line as is
            echo "${parsedLine}" >> "${FNAME}"
        fi
    done < "./grib2_section4_XXX_mod.F90.tmpl"

done




IFS=$'\n'
for i in `cat type_of_level.txt` ; do
  UNAME=`echo $i | awk '{print $1}'`
  LNAME=`echo $i | awk '{print $1}' |sed -e 's/.*/\L&/'`
  echo "  USE :: GRIB2_LEVEL_CONFIGURATOR_${UNAME}_MOD, ONLY: GRIB2_LEVEL_CONFIGURATOR_${UNAME}_T"
done > grib2_section4_use.F90.in


IFS=$'\n'
II=0
for i in `cat type_of_level.txt` ; do
  UNAME=`echo $i | awk '{print $1}'`
  LNAME=`echo $i | awk '{print $1}' |sed -e 's/.*/\L&/'`
  echo "  CASE (${II})"
  echo " "
  echo "    ALLOCATE( GRIB2_LEVEL_CONFIGURATOR_${UNAME}_T::GRIB2_LEVEL_CONFIGURATOR, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )"
  echo "    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )"
  echo " "
  II=$((${II}+1));
done > grib2_section4_allocate.F90.in



IFS=$'\n'
II=0
for i in `cat type_of_level.txt` ; do
  UNAME=`echo $i | awk '{print $1}'`
  LNAME=`echo $i | awk '{print $1}' |sed -e 's/.*/\L&/'`
  echo "      CASE ('${LNAME}')"
  echo "        LEVEL_CONFIGURATOR_TYPE = ${II}_JPIB_K"
  II=$((${II}+1));
done > grib2_section4_string2id.F90.in


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
