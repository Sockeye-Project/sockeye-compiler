FROM ubuntu:20.04

RUN apt-get update
RUN apt-get install ghc make curl -y
RUN mkdir eclipse && \
    cd eclipse &&\
    curl https://eclipseclp.org/Distribution/CurrentRelease/7.0_54%20x86_64_linux%20Intel-64bit-Linux/eclipse_basic.tgz -o eclipse_basic.tgz &&\
    tar xfvz eclipse_basic.tgz &&\
    ln -s /eclipse/lib/x86_64_linux/eclipse.exe /usr/bin/eclipseclp 
ENV ECLIPSEDIR=/eclipse

RUN apt-get install libghc-aeson-dev libghc-missingh-dev -y
