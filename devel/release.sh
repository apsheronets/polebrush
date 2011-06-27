#!/bin/bash

# do not run

basedir=`dirname $0`
basedir=`readlink -f $basedir/../`
cd $basedir || exit 1

version=`head -n1 VERSION`
name=textile-ocaml
tar=$name-$version.tar.gz
tar_dst=$HOME/htdocs/src/textile-ocaml
doc_dst=$tar_dst
chroot_dir=/home/komar/chroot/squeeze-x86
chroot_dist_dir=/home/komar/textile
user=komar
textiler_name=textiler-bin-x86-$version

function release_sources {
  darcs dist -d $name-$version || exit 1
  cp $tar $tar_dst || exit 1
  make -s clean || exit 1
  make -s doc || exit 1
  cp -r doc $doc_dst || exit 1
}

function release_textiler {
  cd $chroot_dist_dir || exit 1
  make -s -j4 textiler textiler.byte || exit 1
  strip --strip-unneeded textiler || exit 1
  mkdir -p $textiler_name
  mv textiler textiler.byte $textiler_name || exit 1
  tar -cf $textiler_name.tar.gz $textiler_name || exit 1
  make -s clean || exit 1
  rm -r $textiler_name
  exit
}

case "$1" in
  sources) release_sources;;
  textiler-root) su -c "$0 textiler" $user;;
  textiler) release_textiler;;
  *) $0 sources &&
    darcs push --no-set-default --all $chroot_dir/$chroot_dist_dir &&
    chmod +x $chroot_dir/$chroot_dist_dir/devel/release.sh &&
    sudo chroot $chroot_dir $chroot_dist_dir/devel/release.sh textiler-root;
    cp $chroot_dir/$chroot_dist_dir/$textiler_name.tar.gz $tar_dst/bin/;;
esac

