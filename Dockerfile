FROM ocaml/opam:debian-ocaml-5.2 AS build
LABEL maintainer "Nat Karmios"
ARG DEBIAN_FRONTEND=noninteractive
RUN sudo apt-get update
RUN sudo apt-get install libgmp-dev pkg-config libsqlite3-dev python3 z3 -y
WORKDIR /home/opam/app/Gillian
COPY . .
RUN [ ! -f _opam ] || mv _opam ~/.opam/5.2
RUN opam update -y
RUN opam install . --deps-only
RUN opam exec -- dune build @all
CMD [ "bash" ]

FROM build AS test
WORKDIR /home/opam/app
RUN git clone https://github.com/GillianPlatform/javert-test262.git test262
RUN git clone https://github.com/GillianPlatform/collections-c-for-gillian.git collections-c
WORKDIR /home/opam/app/Gillian
CMD [ "bash" ]

FROM build AS run
RUN opam install .
WORKDIR /home/opam/app
RUN sudo rm -rf Gillian
CMD [ "bash" ]
