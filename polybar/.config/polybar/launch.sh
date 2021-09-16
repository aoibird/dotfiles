#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use
# polybar-msg cmd quit

# Launch bar default
logfile="/tmp/polybar_default.log"
echo "---" | tee -a $logfile
polybar default >>$logfile 2>&1 &

echo "Bars launched..."
