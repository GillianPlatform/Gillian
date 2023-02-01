FROM node:latest

LABEL maintaner "Sacha \"Giltho\" Ayoun"

ARG DEBIAN_FRONTEND=noninteractive

ENV LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib

RUN apt-get update && apt-get install -y apt-utils

RUN apt-get install -y \
  build-essential \
  curl \
  git \
  zsh \
  m4

RUN npm install -g esy@0.6.12 --unsafe-perm

RUN mkdir /app

WORKDIR /app

RUN git clone https://github.com/GillianPlatform/javert-test262.git test262

RUN git clone https://github.com/GillianPlatform/collections-c-for-gillian.git collections-c

WORKDIR /app/Gillian

COPY . .
RUN rm -rf _esy _build debugger-vscode-extension/node_modules *.install *.log *.db

RUN esy install
RUN esy import-dependencies _export
RUN rm -rf _export

RUN esy

CMD [ "zsh" ]
