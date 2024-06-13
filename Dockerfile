FROM ocaml/opam:debian-ocaml-5.2

LABEL maintaner "Sacha \"Giltho\" Ayoun"

ARG DEBIAN_FRONTEND=noninteractive

RUN mkdir /app

WORKDIR /app

RUN git clone https://github.com/GillianPlatform/javert-test262.git test262

RUN git clone https://github.com/GillianPlatform/collections-c-for-gillian.git collections-c

WORKDIR /app/Gillian

COPY . .

RUN rm -rf _esy _build debugger-vscode-extension/node_modules *.install *.log *.db

RUN opam install . --deps-only --locked

RUN opam exec -- dune build @all

CMD [ "zsh" ]
