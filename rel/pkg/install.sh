#!/usr/bin/bash

USER=howl
GROUP=$USER
AWK=/usr/bin/awk
SED=/usr/bin/sed

DOMAIN="project-fifo.net"
CERTDIR="/data/fifo"
CERTPREFIX="fifo"
DAYS=3650

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
            useradd -g $GROUP -d /data/howl/db -s /bin/false $USER
            echo "Granting permissions to use low port numbers"
            /usr/sbin/usermod -K defaultpriv=basic,net_privaddr $USER
        fi
        echo Creating directories ...
        mkdir -p /data/howl/db/ring
        mkdir -p /data/howl/etc
        mkdir -p /var/log/howl/sasl
        chown -R howl:howl /data/howl

        ## Certificate creation:
        if [ ! -d $CERTDIR ]
        then
            echo "Generating self signed SSL cert in $CERTDIR"
            #echo Trying to guess network configuration ...

            if ifconfig net1 > /dev/null 2>&1
            then
                IP=`ifconfig net1 | grep inet | $AWK '{print $2}'`
            else
                IP=`ifconfig net0 | grep inet | $AWK '{print $2}'`
            fi
            SUBJ="
C=AU
ST=Victoria
O=Company
localityName=Melbourne
commonName=$IP
organizationalUnitName=None
emailAddress=blah@blah.com
"


            export PASSPHRASE=$(head -c 128 /dev/random  | uuencode - | grep -v "^end" | tr "\n" "d")
            echo "Creating certificates"
            mkdir -p $CERTDIR

            openssl genrsa -des3 -out $CERTDIR/$CERTPREFIX.key -passout env:PASSPHRASE 2048
            fail_if_error $?

            openssl req \
                    -new \
                    -batch \
                    -subj "$(echo -n "$SUBJ" | tr "\n" "/")" \
                    -key $CERTDIR/$CERTPREFIX.key \
                    -out $CERTDIR/$CERTPREFIX.csr \
                    -passin env:PASSPHRASE
            fail_if_error $?

            cp $CERTDIR/$CERTPREFIX.key $CERTDIR/$CERTPREFIX.key.org
            fail_if_error $?

            openssl rsa -in $CERTDIR/$CERTPREFIX.key.org -out $CERTDIR/$CERTPREFIX.key -passin env:PASSPHRASE
            fail_if_error $?

            openssl x509 -req -days $DAYS -in $CERTDIR/$CERTPREFIX.csr -signkey $CERTDIR/$CERTPREFIX.key -out $CERTDIR/$CERTPREFIX.crt
            fail_if_error $?

            cat $CERTDIR/$CERTPREFIX.key $CERTDIR/$CERTPREFIX.crt > $CERTDIR/$CERTPREFIX.pem

            chgrp -R $GROUP $CERTDIR

            unset PASSPHRASE
        fi

        if [ -d /tmp/snarl ]
        then
            chown -R $USER:$GROUP /tmp/snarl/
        fi

        ;;
    POST-INSTALL)
        svccfg import /opt/local/fifo-howl/share/howl.xml
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | $AWK '{print $2}'`

        CONFFILE=/data/howl/etc/howl.conf
        cp /opt/local/fifo-howl/etc/howl.conf.example ${CONFFILE}.example

        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
            $SED -i bak -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
        else
            echo "Please make sure you update your config according to the update manual!"
            #/opt/local/fifo-sniffle/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
            #    mv ${CONFFILE} ${CONFFILE}.old &&
            #    mv ${CONFFILE}.new ${CONFFILE}
        fi
        ;;
esac
