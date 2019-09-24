#!/bin/bash
. ./vms.sh

for vm in ${vms[*]}
do
id -u `id -un` > $vm/.vagrant/machines/default/virtualbox/creator_uid
done
