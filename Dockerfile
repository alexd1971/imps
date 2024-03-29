FROM haskell:8.10.4-buster as build

# Install build dependencies
RUN     apt-get update && apt-get -y install wget libjpeg62-turbo-dev libpng-dev
# Copying imps sources
WORKDIR /tmp/build
COPY    . ./
# Building ImageMagick
RUN sh install-ImageMagick.sh
# Running tests
RUN     stack test
# Building imps
RUN     stack build --system-ghc --copy-bins
# Collecting shared libraries
WORKDIR /root/.local/bin
RUN     mkdir /tmp/lib && for lib in `ldd ./imps | grep "=>"| awk '{print $3;}' | egrep -v 'libm.so.6|libc.so.6|ld-linux-x86-64.so.2|libresolv.so.2'`; do cp $lib /tmp/lib/; done

# Building imps docker image
FROM    busybox:stable-glibc as app
# Copying imps binary
COPY    --from=build /root/.local/bin/imps /usr/local/bin/imps
# Copying shared library dependencies
COPY    --from=build /tmp/lib /lib
# Running service
EXPOSE  7777
ENTRYPOINT ["imps"]
