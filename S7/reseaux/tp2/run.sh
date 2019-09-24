#!/bin/bash
. ./vms.sh

if [ ! -d "$1" ] 
then
    echo "Directory $1 DOES NOT exists." 
    exit 9999
fi

VBoxManage setproperty machinefolder "$1"

for vm in ${vms[*]}
do
cd $vm
vagrant up &
cd -
done
