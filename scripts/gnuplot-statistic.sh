#! /bin/bash

qq=${1-.}
function a() { 
	for a in $(ls "$qq"/statistic.*|sort); do
	  number=$(grep "Size of Network" $a|cut -d ":" -f 2)
	  value=$(grep "$1" $a|cut -d ":" -f 2)
	  echo $number $value
	done
}

a  "Density of Network"|sort -n >"network-density.lst"
a  "Density of Cloud"|sort -n >"cloud-density.lst"
a  "Deviation from central"|sort -n >"central-deviation.lst"
a  "Deviation from target"|sort -n >"target-deviation.lst"
a  "Deviation from average"|sort -n >"average-deviation.lst"
a  "target - central deviation"|sort -n >"target-central-deviation.lst"
a  "target - average deviation"|sort -n >"target-average-deviation.lst"
a  "Accumulated priority"|sort -n >"accumulated-priority.lst"

gnuplot <<EOF
plot 'target-average-deviation.lst' with lines, 'target-central-deviation.lst' with lines, 'target-deviation.lst' with lines, 'accumulated-priority.lst' with lines, 'average-deviation.lst' with lines, 'central-deviation.lst' with lines, 'cloud-density.lst' with lines, 'network-density.lst' with lines;
pause 1000
EOF

