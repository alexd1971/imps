FROM alpine:3.12 as build

WORKDIR /tmp/build
RUN apk add curl ghc musl-dev zlib-dev jpeg-dev libpng-dev fontconfig-dev freetype-dev gd-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /tmp/build/

RUN stack build --system-ghc --copy-bins

FROM alpine:3.12 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apk add --no-cache gmp libffi gd fontconfig expat

COPY --from=build /root/.local/bin/imps .
EXPOSE 7777
CMD []
ENTRYPOINT ["/opt/app/imps"]
