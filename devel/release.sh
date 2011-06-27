#!/bin/bash

# do not run

basedir=`dirname $0`
basedir=`readlink -f $basedir/../`
cd $basedir || exit 1

version=`head -n1 VERSION`
name=polebrush
tar=$name-$version.tar.gz
tar_dst=$HOME/htdocs/src/polebrush
doc_dst=$tar_dst
chroot_dir=/home/komar/chroot/squeeze-x86
chroot_dist_dir=/home/komar/polebrush
user=komar
polebrush_name=polebrush-bin-x86-$version

function release_sources {
  darcs dist -d $name-$version || exit 1
  cp $tar $tar_dst || exit 1
  make -s clean || exit 1
  make -s doc || exit 1
  cp -r doc $doc_dst || exit 1
}

function release_polebrush {
  cd $chroot_dist_dir || exit 1
  make -s -j4 polebrush polebrush.byte || exit 1
  strip --strip-unneeded polebrush || exit 1
  mkdir -p $polebrush_name
  mv polebrush polebrush.byte $polebrush_name || exit 1
  tar -cf $polebrush_name.tar.gz $polebrush_name || exit 1
  make -s clean || exit 1
  rm -r $polebrush_name
  exit
}

case "$1" in
  sources) release_sources;;
  polebrush-root) su -c "$0 polebrush" $user;;
  polebrush) release_polebrush;;
  *) $0 sources &&
    darcs push --no-set-default --all $chroot_dir/$chroot_dist_dir &&
    chmod +x $chroot_dir/$chroot_dist_dir/devel/release.sh &&
    sudo chroot $chroot_dir $chroot_dist_dir/devel/release.sh polebrush-root;
    cp $chroot_dir/$chroot_dist_dir/$polebrush_name.tar.gz $tar_dst/bin/;;
esac

