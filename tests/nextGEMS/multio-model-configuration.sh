#
#
# Required input variables
MULTIO_RAPS_ETC_DIR=/ec/res4/scratch/mavm/RAPSrun
MULTIO_RAPS_OUT_DIR=/ec/res4/scratch/mavm/RAPSrun
MULTIO_RAPS_GRID_TYPE=O80
FDB_HOME=/ec/res4/scratch/mavm/fakeFDB/
FDB_CONFIG=${FDB_HOME}/config.yaml
FDB5_SUB_TOCS=ON

#
#
# More fine configurations need to touch only for debug purposes

#
# Global controls for the plans
MULTIO_DEBUG=1
MULTIO_RAPS_ENABLE_DEBUG_PLAN=0
MULTIO_RAPS_ENABLE_FDB5_OUTPUT=1
MULTIO_RAPS_ENABLE_FILE_OUTPUT=1
MULTIO_RAPS_ENABLE_PLAN_1=1
MULTIO_RAPS_ENABLE_PLAN_2=1
MULTIO_RAPS_ENABLE_PLAN_3=0
MULTIO_RAPS_ENABLE_PLAN_4=0


# TODO:
# Due to a bug at the moment it is not possible to run with both
# statistics restart and PLAN_4 enabled at the sam time
MULTIO_RAPS_STATISTICS_RESTART=0


#
# Grid type (needs to arrive from the script)
MULTIO_RAPS_RESTART_FREQUENCY=24


#
# Configuration files
MULTIO_SERVER_CONFIG_PATH=${MULTIO_RAPS_ETC_DIR}/cfg
MULTIO_PLANS_FILE=${MULTIO_SERVER_CONFIG_PATH}/multio-server-ifsio.yaml


#
# General configuration of multIO
MULTIO_RAPS_RESTART_PATH=${MULTIO_RAPS_OUT_DIR}/restart.multio.${MULTIO_RAPS_GRID_TYPE}
MULTIO_RAPS_OUTPUT_PATH=${MULTIO_RAPS_OUT_DIR}/out.${MULTIO_RAPS_GRID_TYPE}
MULTIO_RAPS_TEMPLATES_PATH=${MULTIO_RAPS_OUT_DIR}/templates.${MULTIO_RAPS_GRID_TYPE}
MIR_CACHE_PATH=${MULTIO_RAPS_OUT_DIR}/mircache.${MULTIO_RAPS_GRID_TYPE}
MULTIO_RAPS_PLANS_PATH=${MULTIO_SERVER_CONFIG_PATH}/plans
MULTIO_RAPS_DUMP_CONFIGURATION_FILE=${MULTIO_RAPS_OUT_DIR}/multio_configuration.${MULTIO_RAPS_GRID_TYPE}.log


#
# Checks in the directories
MULTIO_RAPS_PRODUCTS_SUBTREE=(
"01-OriginalGrids"
"01-OriginalGrids/01-HourlySurfaces"
"01-OriginalGrids/02-6Hourly3D"
"01-OriginalGrids/03-Hourly3D"
"02-InterpolatedFields"
"02-InterpolatedFields/01-RegularLatLong_1.0x1.0"
"02-InterpolatedFields/02-RegularLatLong_0.25x0.25"
"03-MonthlyAverages"
"03-MonthlyAverages/01-OriginalGrids"
"03-MonthlyAverages/02-RegularLL-1.0x1.0"
"03-MonthlyAverages/03-RegularLL-0.25x0.25"
"04-WaveModel"
)

# Create and clean the output/restart directory tree
[[ -d ${MULTIO_RAPS_OUTPUT_PATH} ]] || mkdir -p ${MULTIO_RAPS_OUTPUT_PATH}
[[ -d ${MULTIO_RAPS_RESTART_PATH} ]] || mkdir -p ${MULTIO_RAPS_RESTART_PATH}
for subTree in ${MULTIO_RAPS_PRODUCTS_SUBTREE[@]}; do
    name=${MULTIO_RAPS_OUTPUT_PATH}/${subTree}
    [[ -d $name ]] || mkdir -p ${name}
    name=${MULTIO_RAPS_RESTART_PATH}/${subTree}
    [[ -d $name ]] || mkdir -p ${name}
done

for f in `find ${MULTIO_RAPS_OUTPUT_PATH} -type f -iname '*.grib'` ; do
    rm -f $i;
done
for f in `find ${MULTIO_RAPS_RESTART_PATH} -type f -iname '*.bin'` ; do
    rm -f $i;
done


#
# Plans configurations
MULTIO_RAPS_ENABLE_PLAN_0_0_0=${MULTIO_RAPS_ENABLE_DEBUG_PLAN}

MULTIO_RAPS_ENABLE_PLAN_1_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_1_1_2=$((0*${MULTIO_RAPS_ENABLE_PLAN_1}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_1_1_3=$((0*${MULTIO_RAPS_ENABLE_PLAN_1}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_1_2_2=$((0*${MULTIO_RAPS_ENABLE_PLAN_1}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_1_3_1=$((0*${MULTIO_RAPS_ENABLE_PLAN_1}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_PLAN_2_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_2_1_2=$((0*${MULTIO_RAPS_ENABLE_PLAN_2}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_2_2_1=$((0*${MULTIO_RAPS_ENABLE_PLAN_2}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_2_2_2=$((0*${MULTIO_RAPS_ENABLE_PLAN_2}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_PLAN_3_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_1_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_1_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_1_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_2_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_2_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_2_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_2_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_3_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_3_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_3_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_PLAN_3_3_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_PLAN_4_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_4}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))



MULTIO_RAPS_ENABLE_FDB5_1_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_1_1_2=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_1_1_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_1_2_2=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_1_3_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_FDB5_2_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_2_1_2=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_2_2_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_2_2_2=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_FDB5_3_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_1_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_1_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_1_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_2_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_2_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_2_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_2_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_3_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_3_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_3_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FDB5_3_3_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_FDB5_4_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_4}*${MULTIO_RAPS_ENABLE_FDB5_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))


MULTIO_RAPS_ENABLE_FILE_1_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_1_1_2=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_1_1_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_1_2_2=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_1_3_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_1}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_FILE_2_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_2_1_2=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_2_2_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_2_2_2=$((1*${MULTIO_RAPS_ENABLE_PLAN_2}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_FILE_3_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_1_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_1_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_1_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_2_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_2_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_2_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_2_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_3_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_3_2a=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_3_2b=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))
MULTIO_RAPS_ENABLE_FILE_3_3_3=$((1*${MULTIO_RAPS_ENABLE_PLAN_3}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))

MULTIO_RAPS_ENABLE_FILE_4_1_1=$((1*${MULTIO_RAPS_ENABLE_PLAN_4}*${MULTIO_RAPS_ENABLE_FILE_OUTPUT}*(1-${MULTIO_RAPS_ENABLE_DEBUG_PLAN})))




# Global controllers for the plans
echo MULTIO_DEBUG=${MULTIO_DEBUG} > ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_DEBUG_PLAN=${MULTIO_RAPS_ENABLE_DEBUG_PLAN} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_OUTPUT=${MULTIO_RAPS_ENABLE_FDB5_OUTPUT} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_OUTPUT=${MULTIO_RAPS_ENABLE_FILE_OUTPUT} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_1=${MULTIO_RAPS_ENABLE_PLAN_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_2=${MULTIO_RAPS_ENABLE_PLAN_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3=${MULTIO_RAPS_ENABLE_PLAN_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_4=${MULTIO_RAPS_ENABLE_PLAN_4} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_STATISTICS_RESTART=${MULTIO_RAPS_STATISTICS_RESTART} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ETC_DIR=${MULTIO_RAPS_ETC_DIR} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_OUT_DIR=${MULTIO_RAPS_OUT_DIR} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}


# Grid type (needs to arrive from the script)
echo MULTIO_RAPS_GRID_TYPE=${MULTIO_RAPS_GRID_TYPE} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_RESTART_FREQUENCY=${MULTIO_RAPS_RESTART_FREQUENCY} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}


# General configuration of multIO
echo MULTIO_RAPS_RESTART_PATH=${MULTIO_RAPS_RESTART_PATH} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_OUTPUT_PATH=${MULTIO_RAPS_OUTPUT_PATH} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_TEMPLATES_PATH=${MULTIO_RAPS_TEMPLATES_PATH} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MIR_CACHE_PATH=${MIR_CACHE_PATH} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_PLANS_PATH=${MULTIO_RAPS_PLANS_PATH} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_DUMP_CONFIGURATION_FILE=${MULTIO_RAPS_DUMP_CONFIGURATION_FILE} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_PRODUCTS_SUBTREE=${MULTIO_RAPS_PRODUCTS_SUBTREE[@]} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}

# FDB configuration
echo FDB_HOME=${FDB_HOME} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo FDB_CONFIG=${FDB_CONFIG} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo FDB5_SUB_TOCS=${FDB5_SUB_TOCS} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}


# Configuration files
echo MULTIO_SERVER_CONFIG_PATH=${MULTIO_SERVER_CONFIG_PATH} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_PLANS_FILE=${MULTIO_PLANS_FILE} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}


# Plans configurations
echo MULTIO_RAPS_ENABLE_PLAN_0_0_0=${MULTIO_RAPS_ENABLE_PLAN_0_0_0} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}

echo MULTIO_RAPS_ENABLE_PLAN_1_1_1=${MULTIO_RAPS_ENABLE_PLAN_1_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_1_1_2=${MULTIO_RAPS_ENABLE_PLAN_1_1_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_1_1_3=${MULTIO_RAPS_ENABLE_PLAN_1_1_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_1_2_2=${MULTIO_RAPS_ENABLE_PLAN_1_2_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_1_3_1=${MULTIO_RAPS_ENABLE_PLAN_1_3_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_2_1_1=${MULTIO_RAPS_ENABLE_PLAN_2_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_2_1_2=${MULTIO_RAPS_ENABLE_PLAN_2_1_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_2_2_1=${MULTIO_RAPS_ENABLE_PLAN_2_2_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_2_2_2=${MULTIO_RAPS_ENABLE_PLAN_2_2_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_1_1=${MULTIO_RAPS_ENABLE_PLAN_3_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_1_2a=${MULTIO_RAPS_ENABLE_PLAN_3_1_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_1_2b=${MULTIO_RAPS_ENABLE_PLAN_3_1_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_1_3=${MULTIO_RAPS_ENABLE_PLAN_3_1_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_2_1=${MULTIO_RAPS_ENABLE_PLAN_3_2_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_2_2a=${MULTIO_RAPS_ENABLE_PLAN_3_2_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_2_2b=${MULTIO_RAPS_ENABLE_PLAN_3_2_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_2_3=${MULTIO_RAPS_ENABLE_PLAN_3_2_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_3_1=${MULTIO_RAPS_ENABLE_PLAN_3_3_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_3_2a=${MULTIO_RAPS_ENABLE_PLAN_3_3_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_3_2b=${MULTIO_RAPS_ENABLE_PLAN_3_3_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_PLAN_3_3_3=${MULTIO_RAPS_ENABLE_PLAN_3_3_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}


echo MULTIO_RAPS_ENABLE_FDB5_1_1_1=${MULTIO_RAPS_ENABLE_FDB5_1_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_1_1_2=${MULTIO_RAPS_ENABLE_FDB5_1_1_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_1_1_3=${MULTIO_RAPS_ENABLE_FDB5_1_1_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_1_2_2=${MULTIO_RAPS_ENABLE_FDB5_1_2_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_1_3_1=${MULTIO_RAPS_ENABLE_FDB5_1_3_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_2_1_1=${MULTIO_RAPS_ENABLE_FDB5_2_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_2_1_2=${MULTIO_RAPS_ENABLE_FDB5_2_1_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_2_2_1=${MULTIO_RAPS_ENABLE_FDB5_2_2_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_2_2_2=${MULTIO_RAPS_ENABLE_FDB5_2_2_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_1_1=${MULTIO_RAPS_ENABLE_FDB5_3_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_1_2a=${MULTIO_RAPS_ENABLE_FDB5_3_1_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_1_2b=${MULTIO_RAPS_ENABLE_FDB5_3_1_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_1_3=${MULTIO_RAPS_ENABLE_FDB5_3_1_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_2_1=${MULTIO_RAPS_ENABLE_FDB5_3_2_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_2_2a=${MULTIO_RAPS_ENABLE_FDB5_3_2_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_2_2b=${MULTIO_RAPS_ENABLE_FDB5_3_2_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_2_3=${MULTIO_RAPS_ENABLE_FDB5_3_2_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_3_1=${MULTIO_RAPS_ENABLE_FDB5_3_3_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_3_2a=${MULTIO_RAPS_ENABLE_FDB5_3_3_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_3_2b=${MULTIO_RAPS_ENABLE_FDB5_3_3_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FDB5_3_3_3=${MULTIO_RAPS_ENABLE_FDB5_3_3_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}


echo MULTIO_RAPS_ENABLE_FILE_1_1_1=${MULTIO_RAPS_ENABLE_FILE_1_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_1_1_2=${MULTIO_RAPS_ENABLE_FILE_1_1_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_1_1_3=${MULTIO_RAPS_ENABLE_FILE_1_1_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_1_2_2=${MULTIO_RAPS_ENABLE_FILE_1_2_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_1_3_1=${MULTIO_RAPS_ENABLE_FILE_1_3_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_2_1_1=${MULTIO_RAPS_ENABLE_FILE_2_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_2_1_2=${MULTIO_RAPS_ENABLE_FILE_2_1_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_2_2_1=${MULTIO_RAPS_ENABLE_FILE_2_2_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_2_2_2=${MULTIO_RAPS_ENABLE_FILE_2_2_2} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_1_1=${MULTIO_RAPS_ENABLE_FILE_3_1_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_1_2a=${MULTIO_RAPS_ENABLE_FILE_3_1_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_1_2b=${MULTIO_RAPS_ENABLE_FILE_3_1_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_1_3=${MULTIO_RAPS_ENABLE_FILE_3_1_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_2_1=${MULTIO_RAPS_ENABLE_FILE_3_2_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_2_2a=${MULTIO_RAPS_ENABLE_FILE_3_2_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_2_2b=${MULTIO_RAPS_ENABLE_FILE_3_2_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_2_3=${MULTIO_RAPS_ENABLE_FILE_3_2_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_3_1=${MULTIO_RAPS_ENABLE_FILE_3_3_1} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_3_2a=${MULTIO_RAPS_ENABLE_FILE_3_3_2a} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_3_2b=${MULTIO_RAPS_ENABLE_FILE_3_3_2b} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
echo MULTIO_RAPS_ENABLE_FILE_3_3_3=${MULTIO_RAPS_ENABLE_FILE_3_3_3} >> ${MULTIO_RAPS_DUMP_CONFIGURATION_FILE}
