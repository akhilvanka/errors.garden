FROM debian:bookworm AS build
WORKDIR /app
RUN apt-get update && apt-get install -y curl gcc g++ git gnupg libffi-dev libgmp-dev libtinfo-dev make netbase xz-utils zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN curl -sSL https://get.haskellstack.org/ | sh -s - -f
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack setup
RUN stack build --only-dependencies
COPY src ./src
RUN stack build && stack install --local-bin-path /app/bin

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libgmp10 ca-certificates && rm -rf /var/lib/apt/lists/*
COPY --from=build /app/bin/errors-garden /usr/local/bin/
EXPOSE 8080
CMD ["errors-garden"]
