#!/usr/bin/env bash
set -e
shopt -s nullglob
for recipe in .recipes/*; do
  LOCAL_REPO="$PWD" melpazoid --license "$recipe"; exit $?
done
