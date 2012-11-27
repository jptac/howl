#!/usr/bin/bash

case $2 in
    DEINSTALL)
	echo "Stopping Howl service."
	svcadm disable network/howl
	;;
    POST-DEINSTALL)
	echo "Removing Howl service."
	svccfg delete network/howl
	echo "Please beware that database and logfiles have not been"
	echo "deleted! Neither have the howl user or gorup."
	echo "If you don't need them any more remove the directories:"
	echo " /var/log/howl"
	echo " /var/db/howl"
	;;
esac
