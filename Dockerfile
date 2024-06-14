FROM ocaml/opam:debian-ocaml-5.2

LABEL maintaner "Sacha \"Giltho\" Ayoun"

ARG DEBIAN_FRONTEND=noninteractive

RUN sudo apt-get update

RUN sudo apt install software-properties-common

RUN sudo add-apt-repository ppa:deadsnakes/ppa

RUN sudo apt update

RUN sudo apt-get install libgmp-dev pkg-config python3.9 -y

RUN mkdir /home/opam/app

WORKDIR /home/opam/app

RUN git clone https://github.com/GillianPlatform/javert-test262.git test262

RUN git clone https://github.com/GillianPlatform/collections-c-for-gillian.git collections-c

WORKDIR /home/opam/app/Gillian

COPY . .

RUN opam update -y

RUN opam install . --deps-only

RUN opam exec -- dune build @all

CMD [ "zsh" ]
