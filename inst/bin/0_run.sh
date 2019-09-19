#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****UI****"

  /usr/local/bin/Rscript /r/ui/src/RunProcess.R

  echo "****END****UI****"
  
) 200>/var/lock/.ui.exclusivelock
