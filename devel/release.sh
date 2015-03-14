#!/bin/bash

# do not run

basedir=`dirname $0`
basedir=`readlink -f $basedir/../`
cd $basedir || exit 1

version=`head -n1 VERSION`
name=polebrush
tar=$name-$version.tar.gz
tar_dst=$HOME/htdocs/src/$name
doc_dst=$tar_dst
chroot_dir=/home/komar/chroot/wheezy-x86
chroot_dist_dir=/home/komar/
user=komar
polebrush_name=polebrush-bin-x86-$version

function release_sources {
  git archive --format tar.gz --prefix $name-$version/ HEAD > $tar || exit 1
  cp $tar $tar_dst || exit 1
  make -s clean || exit 1
  make -s doc || exit 1
  cp -r doc $doc_dst || exit 1
}

function release_build {
  cd $chroot_dist_dir/$name || exit 1
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
  build-root) su -c "$0 build" $user;;
  build) release_build;;
  *) $0 sources || exit 1
    rm -rf $chroot_dir/$chroot_dist_dir/$name || exit 1
    cd $chroot_dir/$chroot_dist_dir || exit 1
    git clone $basedir || exit 1
    cd $basedir || exit 1
    chmod +x $chroot_dir/$chroot_dist_dir/$name/devel/release.sh || exit 1
    sudo chroot $chroot_dir $chroot_dist_dir/$name/devel/release.sh build-root;
    mkdir -p $tar_dst/../../builds/$name/;
    cp $chroot_dir/$chroot_dist_dir/$name/$polebrush_name.tar.gz $tar_dst/../../builds/$name/;;
esac

