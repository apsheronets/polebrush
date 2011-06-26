#!/bin/sh

prefix=`dirname $0`
textiler=$prefix/textiler
textiler_duce=$prefix/textiler_duce

textiler_tmp=/tmp/textiler
textiler_duce_tmp=/tmp/textiler_duce

src=$1

$textiler -escape-html -escape-nott < $src > $textiler_tmp || exit 1
$textiler_duce < $src > $textiler_duce_tmp || exit 1

cmp -b $textiler_tmp $textiler_duce_tmp
