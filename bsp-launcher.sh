#!/bin/sh

PROTOCOL="tcp"
HOST="localhost"
PORT="5001"

while true; do
    case $1 in
        --protocol)
            PROTOCOL="$2"
            shift
            ;;
        --host)
            HOST="$2"
            shift
            ;;
        --port)
            PORT="$2"
            shift
            ;;
        -?*)
            echo "$0: Unknown option: $1" >&2
            exit 1
            ;;
        *)
            break
    esac

    shift
done

exec mill mill.bspserver.BspServer/bspTcpServer $HOST $PORT
