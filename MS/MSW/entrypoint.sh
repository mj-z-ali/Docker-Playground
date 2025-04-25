#!/bin/sh

FIFO_START="/src/start/mypipestart"
FIFO_DONE="/src/done/mypipedone"
TARGET="$1"

[ -p "$FIFO_START" ] || mkfifo "$FIFO_START"

cat < "$FIFO_START"

/usr/local/bin/mergesortwork "$TARGET"

[ -p "$FIFO_DONE" ] || mkfifo "$FIFO_DONE"

echo "1" > "$FIFO_DONE"