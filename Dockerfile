FROM erlang:26

WORKDIR /LeaderElectionErlang

RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 > /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

COPY ./rebar.config .
COPY src src
COPY config config

RUN rebar3 compile