#!/usr/bin/env bash

get_volume() {
  # return volume levels (0-100)
  vol=$(amixer sget Master | grep -o -m 1 '[[:digit:]]*%' | tr -d '%')
  echo ${vol}% | tee /tmp/.volume-pipe
}

case $1 in
  "")
    ;;
  "up")
    amixer set Master 3+ >/dev/null
    ;;
  "down")
    amixer set Master 3- > /dev/null
    ;;
  "toggle")
    amixer set Master "toggle" >/dev/null
    ;;
  *)
    echo "unknown command"
    exit 1
    ;;
esac
get_volume
