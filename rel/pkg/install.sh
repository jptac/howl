#!/usr/bin/bash

USER=howl
GROUP=$USER
AWK=/usr/bin/awk
SED=/usr/bin/sed

case $2 in
    PRE-INSTALL)
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
            echo "Granting permissions to use low port numbers"
            /usr/sbin/usermod -K defaultpriv=basic,net_privaddr $USER
        fi
        echo Creating directories ...
        mkdir -p /var/db/howl/ring
        chown -R howl:howl /var/db/howl
        mkdir -p /var/log/howl/sasl
        chown -R howl:howl /var/log/howl
        ;;
    POST-INSTALL)
        svccfg import /opt/local/fifo-howl/share/howl.xml
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | $AWK '{print $2}'`
        CONFFILE=/opt/local/fifo-howl/etc/howl.conf

        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
            $SED -i bak -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
        # else
            # echo "Merging old file with new template, the original can be found in ${CONFFILE}.old."
            # /opt/local/fifo-howl/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
            #    mv ${CONFFILE} ${CONFFILE}.old &&
            #    mv ${CONFFILE}.new ${CONFFILE}
        fi
        ;;
esac
