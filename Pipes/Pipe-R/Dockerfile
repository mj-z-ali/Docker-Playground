FROM ubuntu:latest 

WORKDIR /src

RUN mkfifo mypipe

CMD cat < /src/mypipe