FROM ubuntu:18.04

# Use aliyun registry
RUN sed -i s@/archive.ubuntu.com/@/mirrors.aliyun.com/@g /etc/apt/sources.list
RUN apt clean
RUN apt update
RUN apt install -y sudo git cmake g++ gcc ninja-build vim tar gdb flexc++ bisonc++ openssh-server rsync python3.8 python3-pip dos2unix

RUN useradd -ms /bin/bash -G sudo stu
USER stu