#!/usr/bin/env bash

if [[ $(pgrep -x expressvpnd) != "" ]] && [[ $(expressvpn status) != "Not connected" ]]; then 
  echo "VPN";
else 
  echo "VPN-OFF";
fi
