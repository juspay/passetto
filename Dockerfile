FROM haskell:8.6.5 as deps
WORKDIR /opt/build

RUN \
  curl https://download.libsodium.org/libsodium/releases/libsodium-1.0.18-stable.tar.gz --output libsodium.tar.gz && \
  tar -xvf libsodium.tar.gz && \
  rm libsodium.tar.gz && \
  cd libsodium-stable && \
  ./configure && \
  make && \
  make check && \
  make install

RUN apt-get update && apt-get install -y libpq-dev=9.6.17-0+deb9u1

COPY stack.yaml /opt/build/
COPY hpack /opt/build/hpack
COPY core/package.yaml /opt/build/core/
COPY client/package.yaml /opt/build/client/
COPY service/package.yaml /opt/build/service/
COPY tests/package.yaml /opt/build/tests/
RUN stack build -j4 --only-dependencies


FROM haskell:8.6.5 as build
WORKDIR /opt/build
COPY . /opt/build/
COPY --from=deps /root/.stack /root/.stack
COPY --from=deps /opt/build /opt/build
RUN cd libsodium-stable && make install && ldconfig
RUN apt-get update && apt-get install -y libpq-dev=9.6.17-0+deb9u1

RUN stack install
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin


FROM postgres:9.6.17 as app
WORKDIR /opt/app
COPY ./scripts /opt/app
COPY ./cert /opt/app/cert
COPY ./service/pgsql/create_schema.sql /opt/app/create_schema.sql
COPY --from=build /opt/build/bin .
COPY --from=deps /opt/build/libsodium-stable /opt/app/libsodium-stable
RUN apt-get update && apt-get install -y make gcc
RUN cd libsodium-stable && make install && ldconfig

EXPOSE 8012
ENTRYPOINT ["./launcher.sh"]