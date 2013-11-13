#!/usr/bin/bash

USER=howl
GROUP=$USER

case $2 in
    PRE-INSTALL)
        if grep '^Image: base64 13.2.*$' /etc/product
        then
            echo "Image version supported"
        else
            echo "This image version is not supported please use the base64 13.2.1 image."
            exit 1
        fi
	if grep "^$GROUP:" /etc/group > /dev/null 2>&1
	then
	    echo "Group already exists, skipping creation."
	else
	    echo Creating howl group ...
	    groupadd $GROUP
	fi
	if id $USER > /dev/null 2>&1
	then
	    echo "User already exists, skipping creation."
	else
	    echo Creating howl user ...
	    useradd -g $GROUP -d /var/db/howl -s /bin/false $USER
	fi
	echo Creating directories ...
	mkdir -p /var/db/howl/ring
	chown -R howl:howl /var/db/howl
	mkdir -p /var/log/howl/sasl
	chown -R howl:howl /var/log/howl
	;;
    POST-INSTALL)
	if svcs svc:/network/howl:default > /dev/null 2>&1
	then
	    echo Service already existings ...
	else
	    echo Importing service ...
	    svccfg import /opt/local/fifo-howl/share/howl.xml
	fi
	echo Trying to guess configuration ...
	IP=`ifconfig net0 | grep inet | awk -e '{print $2}'`
	if [ ! -f /opt/local/fifo-howl/etc/vm.args ]
	then
	    cp /opt/local/fifo-howl/etc/vm.args.example /opt/local/fifo-howl/etc/vm.args
	    sed --in-place -e "s/127.0.0.1/${IP}/g" /opt/local/fifo-howl/etc/vm.args
	fi
	if [ ! -f /opt/local/fifo-howl/etc/app.config ]
	then
	    cp /opt/local/fifo-howl/etc/app.config.example /opt/local/fifo-howl/etc/app.config
	    sed --in-place -e "s/127.0.0.1/${IP}/g" /opt/local/fifo-howl/etc/app.config
	fi

	;;
esac
