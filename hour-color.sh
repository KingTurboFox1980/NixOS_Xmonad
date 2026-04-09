#!/usr/bin/env bash

sec=$(date +%S)

case $((sec / 10)) in
  0) echo "#6272a4" ;;
  1) echo "#50fa7b" ;;
  2) echo "#8be9fd" ;;
  3) echo "#f1fa8c" ;;
  4) echo "#ffb86c" ;;
  *) echo "#ff79c6" ;;
esac
