#! /bin/sh

# $Id$

cd $(dirname $(readlink -f $0))
gforth -e "warnings off" tlisp.fs -e "driver"
