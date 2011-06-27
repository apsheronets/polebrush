#!/bin/sh

prefix=`dirname $0`
prefix=`readlink -f $prefix/../`

polebrush=$prefix/polebrush
polebrush_duce=$prefix/polebrush_duce

polebrush_tmp=/tmp/polebrush
polebrush_duce_tmp=/tmp/polebrush_duce

src=$1

$polebrush -escape-html -escape-nott < $src > $polebrush_tmp || exit 1
$polebrush_duce < $src > $polebrush_duce_tmp || exit 1

cmp -b $polebrush_tmp $polebrush_duce_tmp
