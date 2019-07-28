#!/bin/sh
export GFORTHD=$(locate gforth-ditc) 
gforthmi --application tlisp.fi tlisp.fs -e "' driver IS bootmessage"
