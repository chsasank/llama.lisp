FROM ubuntu:22.04

RUN apt-get update && apt-get install -y --no-install-recommends guile-3.0 clang guile-json \
    wget ca-certificates python3-pip python-is-python3
RUN pip install munch llvmlite turnt