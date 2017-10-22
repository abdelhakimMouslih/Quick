#!/usr/bin/env bash

function getMeMyDir {
    local l_executablePath="${1}"
    local l_executableDir="$( dirname "${l_executablePath}" 2> /dev/null )"
    printf '%s' "${l_executableDir}"
}

function getTestDataDir {
    local l_testId="${1}"
    local l_testDataDir=''
    case ${l_testId} in
    flSmokeTest)
        l_testDataDir='smokeTests/fixedLengthFiles'
        ;;
    dSmokeTest)
        l_testDataDir='smokeTests/delimitedFiles'
        ;;
    esac
}

function main {
    local l_executablePath="${0}"
    local l_end2endRootDir="$( getMeMyDir "${l_executablePath}" 2> /dev/null )"

    local l_testDataDir="${l_end2endRootDir}/smokeTests/fixedLengthFiles"

    local l_leftFile="${l_testDataDir}/leftFile"
    local l_rightFile="${l_testDataDir}/rightFile"
    local l_fileDescription="${l_testDataDir}/filesDescription.xml"
    local l_fileDescriptionId='flSmokeTest'

    local l_quickJar="${l_end2endRootDir}/quick.jar"
    java -jar "${l_quickJar}" -d "${l_fileDescription}" -i "${l_fileDescriptionId}" -l "leftFile" -l "rightFile" "${l_leftFile}" "${l_rightFile}"
}


main ${*}
