FROM erlang:20.3.6-alpine as builder

RUN apk add --update tar curl git bash make libc-dev gcc g++ vim libyaml-dev && \
    rm -rf /var/cache/apk/*

RUN set -xe \
    && curl -fSL -o rebar3 "https://s3.amazonaws.com/rebar3-nightly/rebar3" \
    && chmod +x ./rebar3 \
    && ./rebar3 local install \
    && rm ./rebar3

ENV PATH "$PATH:/root/.cache/rebar3/bin"